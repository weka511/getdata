# Download and tidy Samsung data
#
# Copyright (C) 2015 "weka511"
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

# TODO: Pull the data.directory variable up to the top level

rm(list=ls())

library(data.table)


read.features<-function(data.directory="data",base.file.name="UCI HAR Dataset") {
  # 
  #
  # Args:
  #   
  #
  # Returns:
  #   
  features<-read.table(file.path(data.directory,base.file.name,"features.txt"))
  setnames(features,names(features),c("pos","Feature Name"))
  return (as.vector(features[,2]))
}

read.activities<-function(data.directory="data",base.file.name="UCI HAR Dataset") {
  # 
  #
  # Args:
  #   
  #
  # Returns:
  #   
  activities<-read.table(file.path(data.directory,base.file.name,"activity_labels.txt"))
  setnames(activities,names(activities),c("activity","ActivityName"))
  activities
}



read.dataset<-function(mode,features,data.directory="data",base.file.name="UCI HAR Dataset"){
  # Read the test dataset or training datset
  #
  # Args:
  #   mode: Specified which dataset to load, either "test" or "train"
  #   data.directory: The directory from which data will be loaded
  #   base.file.name: The directory into which the zipfile has been expanded
  #
  # Returns:
  #   Frame containing subjects, activities, measurements
  
  # Error handling
  if(!file.exists(data.directory)) 
    stop(paste("Data directory '",
               data.directory,
               "' does not exist. Run download_data.R.\n",
               "NB Make sure both scripts are pointing at the same directory",
               sep=""))
  
  X=read.table(file.path(data.directory,base.file.name,mode,paste("X_",mode,".txt",sep="")))
  setnames(X,names(X),features)
  y=read.table(file.path(data.directory,base.file.name,mode,paste("y_",mode,".txt",sep="")))
  setnames(y,names(y),"activity")
  subject=read.table(file.path(data.directory,base.file.name,mode,paste("subject_",mode,".txt",sep="")))
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
  result<-merge(extracted,activities,by=c("activity"))
  seq<-order(result[,1],result[,2])
  selector<-1:length(result)-1
  selector[1]<-2
  selector[2]=length(result)
  result<-result[seq,selector]
  result$subject.1<-NULL
  result
}


calculate.one.average<-function(activity,subject,field,dataset) {
  # 
  #
  # Args:
  #   
  #
  # Returns:
  #   
  average<-mean(dataset[dataset$subject==subject & dataset$activity==activity,field])
  cbind(activity,subject,field,average)
}


calculate.for.activities.in.subject<- function(subject,activities,field,dataset){
  # 
  #
  # Args:
  #   
  #
  # Returns:
  #   
  result<-calculate.one.average(activities[1],subject,field,dataset)
  for (i in 2:length(activities))
    result<-rbind(result,calculate.one.average(activities[i],subject,field,dataset))
  result
}


calculate.for.one.field<-function(subjects,activities,field,dataset){
  # 
  #
  # Args:
  #   
  #
  # Returns:
  #   
  result<-calculate.for.activities.in.subject(subjects[1],activities,field,dataset)
  for (i in 2:length(subjects))
    result<-rbind(result,calculate.for.activities.in.subject(subjects[i],activities,field,dataset))
  result
}

create.dataset.with.averages<-function(dataset){
  # 
  #
  # Args:
  #   
  #
  # Returns:
  #
  fields<-names(dataset)
  fields<-fields[3:length(fields)]
  subjects<-unique(dataset[,1])
  activities<-unique(dataset[,2])
  
  result<-calculate.for.one.field(subjects,activities,fields[1],dataset)
  
  for (i in 2:length(fields))
    result<-rbind(result,calculate.for.one.field(subjects,activities,fields[i],dataset))
  
  result
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
#activities<-read.activities()
#dataset<-use.descriptive.activity.names(extracted,activities)


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable
#   for each activity and each subject.

averaged.dataset<-create.dataset.with.averages(extracted)

labelled.dataset<-use.descriptive.activity.names(averaged.dataset,activities)

write.table(labelled.dataset,file.path("./data","tidied_data.txt"),row.names=FALSE)
