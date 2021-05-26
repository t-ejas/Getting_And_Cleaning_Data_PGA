library(data.table)
library(dplyr)


#Check if file exixsts
if (!file.exists(filename)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, method="curl")
}  


#Reading Training and test data

features_label <- read.table("UCI HAR Dataset/features.txt")
activities_label <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activity_train <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
features_train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activity_test <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
features_test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)


#merge training and test datasets
subjects <- rbind(subject_train, subject_test)
activities <- rbind(activity_train, activity_test)
features <- rbind(features_train, features_test)

#naming teh columns in features dataset
colnames(features) <- t(features_label[2])


#merge features activity and subject
colnames(activities) <- "Activity"
colnames(subjects) <- "Subject"
merged <- cbind(features,activity,subject)

meanstd <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredcols <- c(meanstd, 562, 563)
extractedData <- completeData[,requiredcols]
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
extractedData$Activity[extractedData$Activity == i] <- as.character(activities_label[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

names(extractedData)


#ammend names
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))

#create clean dataset with the average of each variable for each activity and each subject
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)


cleanData <- aggregate(. ~subjects + activities, extractedData, mean)
cleanData <- cleanData[order(cleanData$Subject,cleanData$Activity),]
write.table(cleanData, file = "Tidy.txt", row.names = FALSE)




