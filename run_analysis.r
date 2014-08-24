#run_analysis.r
# Getting and Cleaning Data - Coursera.org
# peer review Project 2
# Dilir Khan

# This R code file performs the following tasks:
# *****copied from corsera course page*****

#1.Extracts only the measurements on the mean and standard deviation for each measurement. 
#2.Uses descriptive activity names to name the activities in the data set
#3.Appropriately labels the data set with descriptive variable names. 
#4.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#5.Merges the training and the test sets to create one data set.
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##

#1. Merges the training and the test sets to create one data set.
#Set working directory (where dataset lives)
setwd("F:/coursera/Coursera/Getting and Cleaning Data/Project2/UCI HAR Dataset")


# Read data files
#Import features.txt
features     = read.table("./features.txt",header=FALSE) 
#imports activity_labels.txt
activityType = read.table("./activity_labels.txt",header=FALSE)
#Import subject_train.txt
subjectTrain = read.table("./train/subject_train.txt",header=FALSE)
#Import x_train.txt
xTrain <- read.table("./train/x_train.txt",header=FALSE)
#Import y_train.txt
yTrain <- read.table("./train/y_train.txt",header=FALSE)

#Assign Column names to imported data 
colnames(activityType) <- c("activityId","activityType") 
colnames(subjectTrain) <- "subjectId" 
colnames(xTrain) <- features[,2]  
colnames(yTrain) <- "activityId" 

#Create the final training set by column binding yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain)

#Read test data
subjectTest <- read.table("./test/subject_test.txt",header=FALSE) 
xTest <- read.table("./test/x_test.txt",header=FALSE) 
yTest <- read.table("./test/y_test.txt",header=FALSE) 

#Assign column names to test data 
colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features[,2] 
colnames(yTest) <- "activityId"

#Create the final test set by column binding xTest, yTest and subjectTest data
testData <- cbind(yTest,subjectTest,xTest)

# Combine training and test datasets to create a final dataset
finalData <- rbind(trainingData,testData)

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  <- colnames(finalData)

#2.Extracts only the measurements on the mean and standard deviation for each measurement. 

# Create a logicalVector that contains TRUE values for ID, mean() & stddev() columns and FALSE for others
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Subset finalData based on logicalVector to keep only the desired columns
finalData <- finalData[logicalVector==TRUE]

#3.Uses descriptive activity names to name the activities in the data set

#Merge "finalData" set with the "acitivityType" table to include descriptive activity names
finalData = merge(finalData, activityType, by="activityId", all.x=TRUE)

#Update colNames vector to include  new column names after the merge
colNames  = colnames(finalData);

#4.Appropriately labels the data set with descriptive variable names. 

#Clean up variable names
for (n in 1:length(colNames)){
  colNames[n] <- gsub("\\()","",colNames[n])
  colNames[n] <- gsub("-std$","StdDev",colNames[n])
  colNames[n] <- gsub("-mean","Mean",colNames[n])
  colNames[n] <- gsub("^(t)","time",colNames[n])
  colNames[n] <- gsub("^(f)","freq",colNames[n])
  colNames[n] <- gsub("([Gg]ravity)","Gravity",colNames[n])
  colNames[n] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[n])
  colNames[n] <- gsub("[Gg]yro","Gyro",colNames[n])
  colNames[n] <- gsub("AccMag","AccMagnitude",colNames[n])
  colNames[n] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[n])
  colNames[n] <- gsub("JerkMag","JerkMagnitude",colNames[n])
  colNames[n] <- gsub("GyroMag","GyroMagnitude",colNames[n])
}

#Reassign new descriptive column names to the finalData set
colnames(finalData) <- colNames

#5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#Create a new dataset - finalDataNoActivityType excluding the activityType column
finalDataNoActivityType <- finalData[,names(finalData) != "activityType"]

#Summarize finalDataNoActivityType table to include only the mean of each variable for each activity and each subject
tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c("activityId","subjectId")],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

#Merge tidyData with activityType to include descriptive acitvity names
tidyData <- merge(tidyData,activityType,by="activityId",all.x=TRUE)

# Export tidyData 
write.table(tidyData, "./tidyData.txt",row.names=TRUE,sep="\t")
