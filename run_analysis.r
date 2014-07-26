## Script : run_analysis.R
## Author : Joga Nakka
## Purpose :  this Script does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
 

# Set workspace. 
 
if(getwd()!="C:/Users/jnakka/data")
{
    setwd("C:/Users/jnakka/data")
}

# read the training data from CSV
training_data = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
training_data[,562] = read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE)
training_data[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)

 # read the testing data from CSV
testing_data = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
testing_data[,562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
testing_data[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
  
 
 # get the information of labels of activities
activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
 
# Read features and get the feature names better suited for R with some substitutions
feature_names = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
feature_names[,2] = gsub('-mean', 'Mean', feature_names[,2])
feature_names[,2] = gsub('-std', 'Std', feature_names[,2])
feature_names[,2] = gsub('[-()]', '', feature_names[,2])
 
# Merge training_data and testing_data sets together
allTheData = rbind(training_data, testing_data)
 
# Get only the data on mean and std. dev.
cols_reqd <- grep(".*Mean.*|.*Std.*", feature_names[,2])
# First reduce the features table to what we want
feature_names <- feature_names[cols_reqd,]
# Now add the last two columns (subject and activity)
cols_reqd <- c(cols_reqd, 562, 563)
# And remove the unwanted columns from allTheData
allTheData <- allTheData[,cols_reqd]
# Add the column names (features) to allTheData
colnames(allTheData) <- c(feature_names$V2, "Activity", "Subject")
colnames(allTheData) <- tolower(colnames(allTheData))
 
currentActivity = 1
for (currentActivityLabel in activityLabels$V2) {
  allTheData$activity <- gsub(currentActivity, currentActivityLabel, allTheData$activity)
  currentActivity <- currentActivity + 1
}
 
allTheData$activity <- as.factor(allTheData$activity)
allTheData$subject <- as.factor(allTheData$subject)
 
tidy = aggregate(allTheData, by=list(activity = allTheData$activity, subject=allTheData$subject), mean)
# Remove the subject and activity column, since a mean of those has no use
tidy[,90] = NULL
tidy[,89] = NULL
write.table(tidy, "tidy.txt", sep="\t")
