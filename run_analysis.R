rm(list=ls())

library(data.table)


read.features<-function(data_directory="data",base_file_name="UCI HAR Dataset") {
  # 
  #
  # Args:
  #   
  #
  # Returns:
  #   
  features<-read.table(file.path(data_directory,base_file_name="UCI HAR Dataset","features.txt"))
  setnames(features,names(features),c("pos","Feature Name"))
  return (as.vector(features[,2]))
}

read.activities<-function(data_directory="data",base_file_name="UCI HAR Dataset") {
  # 
  #
  # Args:
  #   
  #
  # Returns:
  #   
  activities<-read.table(file.path(data_directory,base_file_name="UCI HAR Dataset","activity_labels.txt"))
  setnames(activities,names(activities),c("activity","ActivityName"))
  activities
}



read.dataset<-function(mode,features,data_directory="data",base_file_name="UCI HAR Dataset"){
  # Read the test dataset or training datset
  #
  # Args:
  #   mode: Specified which dataset to load, either "test" or "train"
  #   data_directory: The directory from which data will be loaded
  #   base_file_name: The directory into which the zipfile has been expanded
  #
  # Returns:
  #   Frame containing subjects, activities, measurements
  
  # Error handling
  if(!file.exists(data_directory)) 
    stop(paste("Data directory '",
               data_directory,
               "' does not exist. Run download_data.R.\n",
               "NB Make sure both scripts are pointing at the same directory",
               sep=""))
  
  X=read.table(file.path(data_directory,base_file_name,mode,paste("X_",mode,".txt",sep="")))
  setnames(X,names(X),features)
  y=read.table(file.path(data_directory,base_file_name,mode,paste("y_",mode,".txt",sep="")))
  setnames(y,names(y),"activity")
  subject=read.table(file.path(data_directory,base_file_name,mode,paste("subject_",mode,".txt",sep="")))
  setnames(subject,names(subject),"subject")
  return (cbind(subject,y,X))
}

merge.training.test<-function(features){
  # Read the training and the test sets, then merge them to create one data set.
  #
  # Args:
  #   features: List of feature names
  #
  # Returns: subject, Ys, meanurements
  #   
  all<-rbind(read.dataset("test",features),read.dataset("train",features))
}

extract.means.sigma<-function(all){
  # Extracts only the measurements on the mean and standard deviation for each measurement.
  #
  # Args:
  #   
  #
  # Returns: Subject, activity, and the measurements names "....mean..." or "..std.."
  #
  all_names<-names(all)
  keeps<-grep("(subject)|(activity)|(.*((mean)|(std)).*)",all_names)
  all[,keeps]
}

use.descriptive.activity.names<-function (extracted,activities) {
  # 
  #
  # Args:
  #   
  #
  # Returns:
  #
  extracted_with_activities<-merge(extracted,activities,by=c("activity"))
  seq<-order(extracted_with_activities[,1],extracted_with_activities[,2])
  selector<-1:length(extracted_with_activities)-1
  selector[1]<-2
  selector[2]=length(extracted_with_activities)
  extracted_with_activities<-extracted_with_activities[seq,selector]
  extracted_with_activities$subject.1<-NULL
  extracted_with_activities
}

# 
#
# Args:
#   
#
# Returns:
#   

#3. Uses descriptive activity names to name the activities in the data set
activities<-read.activities()

#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable
#   for each activity and each subject.

features=read.features()

#1.Merges the training and the test sets to create one data set.

merged<-merge.training.test(features)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
extracted<-extract.means.sigma(merged)

#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
activities<-read.activities()
dataset_with_activities<-use.descriptive.activity.names(extracted,activities)


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable
#   for each activity and each subject.
keys<-unique(dataset_with_activities[,1:2])
fields<-names(dataset_with_activities)
fields<-fields[3:length(fields)]
f1<-function(field) {
  lapply(keys,calculateOneAverage,field,dataset)
  
}
calculateOneAverage<-function(key,field,dataset) {
  dataset[dataset$subject==key[1] & dataset$ActivityName==key[2],field]
}
lapply(fields,f1,keys,dataset_with_activities)