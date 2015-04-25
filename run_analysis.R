rm(list=ls())

library(data.table)

read.features<-function(data_directory="data",base_file_name="UCI HAR Dataset") {
  features<-read.table(file.path(data_directory,base_file_name="UCI HAR Dataset","features.txt"))
  setnames(features,names(features),c("pos","Feature Name"))
  return (as.vector(features[,2]))
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
  #   A list containg X, y, and subject
  
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

#1. Merges the training and the test sets to create one data set.


features=read.features()

test<-read.dataset("test",features)

train<-read.dataset("train",features)


#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable
#   for each activity and each subject.
