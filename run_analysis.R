# Load packages
install.packages("data.table")
library(data.table)
install.packages("dplyr")
library(dplyr)

# set working directory
setwd("C:/Users/bspadmin/Documents/gettingandcleaningdata")

# download ZIP file from the web
URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
destFile <- "CourseDataset.zip"
download.file(URL, destfile = destFile, mode='wb')

# unzip downloaded zip file
unzip(zipfile = "C:/Users/bspadmin/Documents/gettingandcleaningdata/CourseDataset.zip",exdir ="C:/Users/bspadmin/Documents/gettingandcleaningdata/CourseDataset" )

# set path and reading files
pathdata <- "C:/Users/bspadmin/Documents/gettingandcleaningdata/CourseDataset/UCI HAR Dataset"

xtrain <- read.table(file.path(pathdata, "train", "X_train.txt"),header = FALSE)
ytrain = read.table(file.path(pathdata, "train", "y_train.txt"),header = FALSE)
subject_train = read.table(file.path(pathdata, "train", "subject_train.txt"),header = FALSE)

xtest = read.table(file.path(pathdata, "test", "X_test.txt"),header = FALSE)
ytest = read.table(file.path(pathdata, "test", "y_test.txt"),header = FALSE)
subject_test = read.table(file.path(pathdata, "test", "subject_test.txt"),header = FALSE)

features = read.table(file.path(pathdata, "features.txt"),header = FALSE)
activityLabels = read.table(file.path(pathdata, "activity_labels.txt"),header = FALSE)


# Rename columns in  activity labels and activity data
colnames(xtrain) = features[,2]
colnames(ytrain) = "activityId"
colnames(subject_train) = "subjectId"

colnames(xtest)= features[,2]
colnames(ytest) = "activityId"
colnames(subject_test) = "subjectId"
colnames(activityLabels) <- c('activityId','activityType')

# 1.Merge dataframes (Merges the training and the test sets to create one data set.)
merge_train = cbind(ytrain,subject_train,xtrain)
merge_test= cbind(ytest,subject_test,xtest)
full_merge = rbind(merge_test, merge_train)
# 2.Create new datasets by extracting only the measurements on the mean and standard deviation for each measurement  
colnames<- colnames(full_merge)
mean_and_std <- (grepl("activityId",colnames) | grepl("subjectId",colnames) | grepl("mean..",colnames) | grepl("std..",colnames))
mean_and_std
setmeanstd <- full_merge[,mean_and_std == TRUE]

#3.Label the dataset with descriptive variable names
setactnames <- merge(setmeanstd,activityLabels, by='activityId', all.x=TRUE)
setactnames[,1] <- sub("1", "WALKING",setactnames[,1])
setactnames[,1] <- sub("2", "WALKING_UPSTAIRS",setactnames[,1])
setactnames[,1] <- sub("3", "WALKING_DOWNSTAIRS",setactnames[,1])
setactnames[,1] <- sub("4", "SITTING",setactnames[,1])
setactnames[,1] <- sub("5", "STANDING",setactnames[,1])
setactnames[,1] <- sub("6", "LAYING",setactnames[,1])

#4. Label the data set with descriptive variable names.
names(setactnames)<- gsub("Acc", "Accelerometer",names(setactnames))
names(setactnames)<- gsub("Gyro", "Gyroscope",names(setactnames))
names(setactnames)<- gsub("BodyBody", "Body",names(setactnames))
names(setactnames)<- gsub("Mag", "Magnitude",names(setactnames))
names(setactnames)<- gsub("^t", "Time",names(setactnames))
names(setactnames)<- gsub("^f", "Frequency",names(setactnames))
names(setactnames)<- gsub("tBody", "TimeBody",names(setactnames))
names(setactnames)<- gsub("-mean()", "Mean",names(setactnames))
names(setactnames)<- gsub("-std()", "STD",names(setactnames))
names(setactnames)<- gsub("-freq()", "Frequency",names(setactnames))
names(setactnames)<- gsub("angle", "Angle",names(setactnames))
names(setactnames)<- gsub("gravity", "Gravity",names(setactnames))

#5.create another dataset, independent tidy data set with the average of each variable for each activity and each subject
tidyset <- aggregate(.~subjectId + activityId + activityType, setactnames,mean)
tidyset <- tidyset[order(tidyset$subjectId, tidyset$activityId), ]

#save this tidy dataset to local file
write.table(tidyset, "tidyset.txt", row.names = FALSE)


