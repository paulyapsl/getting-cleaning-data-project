#Background of this exercise (from Coursera): 
#The purpose of this project is to demonstrate your ability to collect, work with, and 
#clean a data set. The goal is to prepare tidy data that can be used for later analysis. 
#You will be graded by your peers on a series of yes/no questions related to the project. 
#You will be required to submit: 1) a tidy data set as described below, 2) a link to a 
#Github repository with your script for performing the analysis, and 3) a code book that 
#describes the variables, the data, and any transformations or work that you performed to 
#clean up the data called CodeBook.md. You should also include a README.md in the repo 
#with your scripts. This repo explains how all of the scripts work and how they are 
#connected.  

#Here are the data for the project: 
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

#You should create one R script called run_analysis.R that does the following. 
#1) Merges the training and the test sets to create one data set.
#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#3) Uses descriptive activity names to name the activities in the data set
#4) Appropriately labels the data set with descriptive variable names. 
#5) From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.

setwd("~/R_Stat/Coursera/3 Cleaning Data/UCI HAR Dataset") #set working directory
activity_labels<-read.table("activity_labels.txt",header=FALSE) #read the file from directory and name file
features<-read.table("features.txt",header=FALSE) #read the file from directory and name file

#organizing the "train" sets
X_train<-read.table("./train/X_train.txt",header=FALSE) #read the file from directory and name file (training set)
y_train<-read.table("./train/y_train.txt",header=FALSE) #read the file from directory and name file (training labels)
subject_train<-read.table("./train/subject_train.txt",header=FALSE) #read the file from directory and name file. Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30)

#4) Appropriately labels the data set with descriptive variable names, see below for continuation of #4
colnames(X_train)<-features[,2] #uses the 2nd column of the features table for column naming of X_train
colnames(y_train)<-"activity_id" #name the column header as activity_id
colnames(subject_train)<-"subject_id" #name the column header as subject_id

colnames(activity_labels)<-c("activity_id","activity") #name the column headers

#merges the train data sets
train_data<-cbind(y_train,subject_train,X_train) #combines the tables of y_train, subject_train, X_train by columns

#organizing the "test" sets
X_test<-read.table("./test/X_test.txt",header=FALSE) #read the file from directory and name file (test sets)
y_test<-read.table("./test/y_test.txt",header=FALSE) #read the file from directory and name file (test labels)
subject_test<-read.table("./test/subject_test.txt",header=FALSE) #read the file from directory and name file. Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30)

#4) Appropriately labels the data set with descriptive variable names, see below for continuation of #4
colnames(X_test)<-features[,2] #uses the 2nd column of the features table for column naming of X_test
colnames(y_test)<-"activity_id" #name the column header of y_test
colnames(subject_test)<-"subject_id" #name the column header of subject_test

#merges the test data sets
test_data<-cbind(y_test,subject_test,X_test) #combines the tables of y_test, subject_test, X_test by columns

#merge both the train and test data sets
#1) Merges the training and the test sets to create one data set.
combine_data<-rbind(train_data,test_data) #combines both train (70% of data sets) and test data (30% of data sets)
#3) Uses descriptive activity names to name the activities in the data set
combine_data2<-merge(combine_data,activity_labels,by.x="activity_id",by.y="activity_id",all=FALSE,sort=FALSE) #maps the activity labels to the combine_data
write.table(combine_data2,file="combine_data2.txt",row.name=FALSE) #create the data set saved as "combine_data2.csv" to the selected directory

#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#identify column names that matches "mean", "std", "activity" and "subject"
combine_data_colnames<-colnames(combine_data2) #extract the column names of combine_data set
combine_data_meanstd<-grepl("activity",combine_data_colnames)|grepl("subject",combine_data_colnames)|!grepl("meanFreq",combine_data_colnames)&grepl("mean",combine_data_colnames)|grepl("std",combine_data_colnames) #returns true if the column names matches the criteria described
extract_meanstd<-combine_data2[combine_data_meanstd==TRUE] #extract the relevant columns based on the combine_data_meanstd criteria

#4) Appropriately labels the data set with descriptive variable names.
column_names<-colnames(extract_meanstd) #extracts the column names for the extract_meanstd data
for (i in 1:length(column_names)) #loop through the column_names and replace the 1st argument in the gsub function with the 2nd argument
{
  column_names[i] = gsub("\\()","",column_names[i])
  column_names[i] = gsub("-mean","_Mean",column_names[i])
  column_names[i] = gsub("-std","_StdDev",column_names[i])
  
  column_names[i] = gsub("BodyBody","Body",column_names[i])
  
  column_names[i] = gsub("tBodyAcc","Time_BodyAcceleration",column_names[i])
  column_names[i] = gsub("tGravityAcc","Time_GravityAcceleration",column_names[i])
  column_names[i] = gsub("tBodyGyro","Time_BodyGyroscope",column_names[i])
  column_names[i] = gsub("Mag","Magnitude",column_names[i])
  
  column_names[i] = gsub("fBodyAcc","Frequency_BodyAcceleration",column_names[i])
  column_names[i] = gsub("fGravityAcc","Frequency_GravityAcceleration",column_names[i])
  column_names[i] = gsub("fBodyGyro","Frequency_BodyGyroscope",column_names[i])
  column_names[i] = gsub("fBodyGyro","Frequency_BodyGyroscope",column_names[i])
};

colnames(extract_meanstd)<-column_names

#5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(reshape2) #activate the reshape2 package
melt_data<-melt(extract_meanstd,id=c("subject_id","activity","activity_id")) #melt the extract_meanstd data so as to dcast the data later to desired structure
cast_data<-dcast(melt_data,subject_id+activity+activity_id~variable,mean) #re-casts the melt_data with the variables now recast into the variables columns, displaying subject_id+activity+activity_id in first few columns.
write.table(cast_data,file="tidydata.txt",row.name=FALSE) #create the data set saved as "cast_data.csv" to the selected directory
 
