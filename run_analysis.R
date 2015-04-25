rm(list=ls())
data_directory<-"project"

if(!file.exists(data_directory))
  dir.create(data_directory)

#0. Download file
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
base_file_name <- "UCI HAR Dataset"
zip_file_name<-paste(base_file_name,".zip",sep="")
full_zip_file_name<-file.path(data_directory,zip_file_name)

download.file(url,destfile=full_zip_file_name,mode="wb")

unzip(full_zip_file_name,exdir=data_directory)

#1. Merges the training and the test sets to create one data set.
#2. Extracts only the measurements on the mean and standard deviation for each measurement. 
#3. Uses descriptive activity names to name the activities in the data set
#4. Appropriately labels the data set with descriptive variable names. 
#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable
#   for each activity and each subject.


read.stuff<-function(mode="test"){
  X=read.table(file.path(data_directory,base_file_name,mode,paste("X_",mode,".txt",sep="")))
  y=read.table(file.path(data_directory,base_file_name,mode,paste("y_",mode,".txt",sep="")))
  subject=read.table(file.path(data_directory,base_file_name,mode,paste("subject_",mode,".txt",sep="")))
  return (list("X"=X,"y"=y,"subject"=subject))
}

test<-read.stuff()
X_test<-test$X
y_test<-test$y
subject_test<-test$subject

train<-read.stuff("train")
X_train<-train$X
y_train<-train$y
subject_train<-train$subject

