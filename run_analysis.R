#Coursera - 03 Getting and Cleaning Data
#Course Project
#September 2015
#Anthony Lui

#load packages
library(dplyr) 
library(plyr)
library(reshape)
library(reshape2)

#set work directory
setwd("C:/Users/Anthony/Documents/Coursera Data Scientist/Getting and Cleaning Data/Project/data/UCI HAR Dataset")
getwd()

#read in meta data
activity.labels <- read.table("activity_labels.txt", col.names = c("activity", "labels"))
features <- read.table("features.txt")
features1 <- t(features)[2,1:561]

#read in test data
xtest <- read.table("./test/x_test.txt", col.names=features1)
ytest <- read.table("./test/y_test.txt", col.names = c("activity"))
subjtest <- read.table("./test/subject_test.txt", col.names = c("subject"))

#read in train data
xtrain <- read.table("./train/x_train.txt", col.names=features1)
ytrain <- read.table("./train/y_train.txt", col.names = c("activity"))
subjtrain <- read.table("./train/subject_train.txt", col.names = c("subject"))


#1. Merges the training and the test sets to create one data set.
ds1 <- cbind (ytest, subjtest, xtest)
ds2 <- cbind(ytrain, subjtrain, xtrain)
merge.ds <- rbind(ds1, ds2)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.
names(features)
features1 <- t(features)[2,1:561]
tail(features1, n=3)

meanprx <- grep("mean", features1)
stdprx <- grep("std", features1)
idxprx <- c(meanprx, stdprx)

features2 <- features1[idxprx]

merge.ds1 <- merge.ds[c(1,2,idxprx+2)]

#3. Uses descriptive activity names to name the activities in the data set

merge.ds2 <- join(activity.labels,merge.ds1)
merge.ds2 <- merge.ds2[,-1]

summary(merge.ds2$labels)


#4. Appropriately labels the data set with descriptive variable names.

names(merge.ds2)

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


detach("package:plyr")
temp1 <- melt(merge.ds2, id=c("labels", "subject"))
temp2 <- group_by(temp1, labels, subject, variable)
temp3 <- summarize(temp2, average=mean(value))

final.ds <- temp3
write.table(final.ds, "./final.output.txt", row.names=F, sep=",")

