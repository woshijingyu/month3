rm(list=ls())

# read training and test data
trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
testData <- read.table("UCI HAR Dataset/test/X_test.txt")

# read feature info
feature <- read.table("UCI HAR Dataset/features.txt")
vecFeature <- as.vector(feature[,2])
numOfFeature <- length(vecFeature)

# put feature names to train and test data
colnames(trainData) <- vecFeature;
colnames(testData) <- vecFeature;

# add label to data
trainLabel <- read.table("UCI HAR Dataset/train/y_train.txt")
testLabel <- read.table("UCI HAR Dataset/test/y_test.txt")

trainData <- data.frame(trainData, label=as.vector(trainLabel))
testData <- data.frame(testData, label=as.vector(testLabel))

colnames(trainData)[numOfFeature+1] <- "label"
colnames(testData)[numOfFeature+1] <- "label"

# add subject identifier to data
trainSubject <- read.table("UCI HAR Dataset/train/subject_train.txt")
testSubject <- read.table("UCI HAR Dataset/test/subject_test.txt")

trainData <- data.frame(trainData, subject=as.vector(trainSubject))
testData <- data.frame(testData, subject=as.vector(testSubject))

colnames(trainData)[numOfFeature+2] <- "subject"
colnames(testData)[numOfFeature+2] <- "subject"

# combine train and test data
data <- rbind(trainData,testData)

# replace all activity numbers to activity names
data[data[,"label"]==1,"label"] <- "WALKING"
data[data[,"label"]==2,"label"] <- "WALKING_UPSTAIRS"
data[data[,"label"]==3,"label"] <- "WALKING_DOWNSTAIRS"
data[data[,"label"]==4,"label"] <- "SITTING"
data[data[,"label"]==5,"label"] <- "STANDING"
data[data[,"label"]==6,"label"] <- "LAYING"

data$label <- as.factor(data$label)
data$subject <- as.factor(data$subject)

# extract only mean and standard deviation measurements
name <- colnames(data)
selectedIdx <- c();
for (i in 1:ncol(data))
{
  if( grepl("mean",name[i]) || grepl("std",name[i]) )
  {
    selectedIdx <- c(selectedIdx, i)
  } 
}
selectedIdx <- c(selectedIdx, ncol(data)-1, ncol(data))

selectedData <- data[,selectedIdx]
numOfSelectedFeatures <- ncol(selectedData)

# generate second, independent tidy data set
data2 <- tapply(selectedData[,1:numOfSelectedFeatures-2], list(selectedData[,numOfSelectedFeatures-1],selectedData[,numOfSelectedFeatures]), mean)


