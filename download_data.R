rm(list=ls())
data_directory<-"data"

if(!file.exists(data_directory))
  dir.create(data_directory)

url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
base_file_name <- "UCI HAR Dataset"
zip_file_name<-paste(base_file_name,".zip",sep="")
full_zip_file_name<-file.path(data_directory,zip_file_name)

download.file(url,destfile=full_zip_file_name,mode="wb")

unzip(full_zip_file_name,exdir=data_directory)

