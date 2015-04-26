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

rm(list=ls())

# TODO: pull the next variable out into a separate source file so it can be shared with run_analysis.R
data.directory<-"data"

if(!file.exists(data.directory))
  dir.create(data.directory)

# TODO: pull the next variable out into a separate source file so it can be shared with run_analysis.R

url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

base.file.name <- "UCI HAR Dataset"
zip.file.name<-paste(base.file.name,".zip",sep="")
full.zip.file.name<-file.path(data.directory,zip.file.name)

download.file(url,destfile=full.zip.file.name,mode="wb")

unzip(full.zip.file.name,exdir=data.directory)

