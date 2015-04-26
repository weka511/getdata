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

# Tidy Samsung data as specified in the course project for Getting & Cleaning Data
# - https://class.coursera.org/getdata-013 
#
# TODO: Pull the data.directory variable up to the top level
# TODO: pull the data.directory and base.file.name variables out into a separate source file
#       so they can be shared with download_data.R
# TODO: Replace loops with one of the apply family

# Gentle reader, you will find the controlling logic for this program near the bottom. Nearly all 
# the code consists of functions that do the heavy lifting; the main program merely invokes them
# in sequence. This way I can test the functions one at a time before tunning the whole thing.

rm(list=ls())

library(data.table)


read.features<-function(data.directory="data",base.file.name="UCI HAR Dataset") {
  # Read list of feature names from downloaded dataset. These are the name of the original measurements
  #
  # Args:
  #      data.directory: identifies where dataset has been stored
  #      base.file.name: sub-directory into which data has been unzipped
  #   
  #
  # Returns: A vector of features
  #   
  features<-read.table(file.path(data.directory,base.file.name,"features.txt"))
  setnames(features,names(features),c("pos","Feature Name"))
  return (as.vector(features[,2]))
}

read.activities<-function(data.directory="data",base.file.name="UCI HAR Dataset") {
  # Read activity numbers and names from dataset
  #
  # Args:
  #      data.directory: identifies where dataset has been stored
  #      base.file.name: sub-directory into which data has been unzipped
  #   
  #
  # Returns: A dataframe whose columns are the activity code and activity name
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
  #   features: List of measurements, used to label columns
  #   data.directory: The directory from which data will be loaded
  #   base.file.name: The directory into which the zipfile has been expanded
  #
  # Returns: Data Frame containing subjects, activities, measurements
  
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
  # Returns: Data Frame containing subjects, activities, measurements
  #   
  rbind(read.dataset("test",features),read.dataset("train",features))
}

extract.means.sigma<-function(data){
  # Extract the measurements on the mean and standard deviation for each measurement.
  #
  # Args:
  #   data: Data frame of merged data
  #
  # Returns: Subject, activity, and the measurements names "....mean..." or "..std.."
  #
  data_names<-names(data)
  # we want the two columns 'subject' and 'activity' in addition to the measurments, so we'll
  # build a logical vector of coumns we want to keep
  keeps<-grep("(subject)|(activity)|(.*((mean)|(std)).*)",data_names)
  data[keeps]
}

use.descriptive.activity.names<-function (data,activities) {
  # This function replaces activity codes with activity names
  #
  # Args:
  #   data: Data frame with an activity code in the 1st column
  #   activities: Data frame of activity codes and name
  #
  # Returns: New data frame where activity code has been replaced by name
  #          This will be sorted by subject and activity code
  #
  result<-merge(data,activities,by=c("activity"))
  
  seq<-order(result[,1],result[,2])  # Allow result to be sorted
  # At this stage each row contains activity number and name. We want to remove the activity code
  # as it is redundant. There is also a duplicate subject number
  result$subject.1<-NULL
  result$activity<-NULL
  
  # My experiments have also shown that the activity code is at the end,
  # so we need to reorder columns.
  # TODO: this is a hack. We should bas this on names. Howver, tempus fugit.
  
  selector <- 1:length(result)
  selector[2] <- length(result)
  selector[3] <- 2
  selector[4] <- 3
  
  result[seq,selector]
}


calculate.one.average<-function(activity,subject,field,dataset) {
  # Calculate the average of one specified mesuarment for combination of user and subject
  #
  # Args:
  #   activity:
  #   subject:
  #   field:
  #   dataset:
  #   
  #
  # Returns: One row data frame with activity,subject,field, and average
  #   
  average<-mean(dataset[dataset$subject==subject & dataset$activity==activity,field])
  cbind(activity,subject,field,average)
}


calculate.for.activities.in.subject<- function(subject,activities,field,dataset){
  # Calculate a bunch of averages for specified field and subject, one value for each activity
  #
  # Args:
  #   activities:
  #   subject:
  #   field:
  #   dataset:
  #   
  # Returns: Data frame with one row for each activity containing activity, subject,field, and average
  #   
  result<-calculate.one.average(activities[1],subject,field,dataset)
  for (i in 2:length(activities))
    result<-rbind(result,calculate.one.average(activities[i],subject,field,dataset))
  result
}


calculate.for.one.field<-function(subjects,activities,field,dataset){
  # Calculate a bunch of averages for specified field, one value for each activity and subject
  #
  # Args:
  #   activities:
  #   subject:
  #   field:
  #   dataset:
  #   
  # Returns: Data frame with one row for each activity/subject combination
  #          containing activity, subject,field, and average
  #   
  result<-calculate.for.activities.in.subject(subjects[1],activities,field,dataset)
  for (i in 2:length(subjects))
    result<-rbind(result,calculate.for.activities.in.subject(subjects[i],activities,field,dataset))
  result
}

create.dataset.with.averages<-function(dataset){
  # Calculate a bunch of averages, one value for each field, activity and subject
  #
  # Args:
  #   datset: 
  #
  # Returns: Data frame with one row for each activity/subject/field combination
  #          containing activity, subject,field, and average
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
# Here is the main program at last!All the hard work is done by the functions above
#   
#   



#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable
#   for each activity and each subject.

features=read.features()

#1.Merges the training and the test sets to create one data set.

merged<-merge.training.test(features)

#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
extracted<-extract.means.sigma(merged)


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable
#   for each activity and each subject.

averaged.dataset<-create.dataset.with.averages(extracted)

#3. Uses descriptive activity names to name the activities in the data set
activities<-read.activities()

#4. Appropriately labels the data set with descriptive variable names. 
#activities<-read.activities()
#dataset<-use.descriptive.activity.names(extracted,activities)

labelled.dataset<-use.descriptive.activity.names(averaged.dataset,activities)

write.table(labelled.dataset,file.path("./data","tidied_data.txt"),row.names=FALSE)
