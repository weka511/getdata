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

