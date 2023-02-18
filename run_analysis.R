#get the data from the internet

if(!file.exists("./data")){
  dir.create("./data")
}
getData <- function(url) {
  
  download.file(url,destfile = './dataset.zip')
}

getData('https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip')

### UNZIP THE DATASET ###

unzip(zipfile = "dataset.zip")

setwd("./data/UCI HAR Dataset")
# Reading training data 
x_train   <- read.table("./train/X_train.txt")
y_train   <- read.table("./train/Y_train.txt") 
subject_train <- read.table("./train/subject_train.txt")
# Reading test data 
x_test   <- read.table("./test/X_test.txt")
y_test   <- read.table("./test/Y_test.txt") 
subject_test <- read.table("./test/subject_test.txt")

# read features description 
features <- read.table("./features.txt", as.is = TRUE) 

# read activity labels 
activity_labels <- read.table("./activity_labels.txt") 
colnames(activity_labels) <- c("Id", "activityLabel")
# merge of training and test sets
x_total   <- rbind(x_train, x_test)
y_total   <- rbind(y_train, y_test) 
subject_total <- rbind(subject_train, subject_test) 

# combining into one dataframe
MergedData <- cbind( subject_total, x_total, y_total)
colnames(MergedData) <- c("subject", features[,2],"activity")

rm(x_test, x_train, x_total, y_test, y_train, y_total)
rm(subject_test, subject_train, subject_total)

# grep logical to identify entries with 
dataFilter <- grepl("subject|activity|mean|std", features[,2])

MergedData <- MergedData[,dataFilter]

MergedData$activity <- factor(MergedData$activity, levels = activity_labels[,1], 
                              labels = activity_labels[,2])

# get column names
MergedDataCols <- colnames(MergedData)

# remove special characters
MergedDataCols <- gsub("[\\(\\)-]", "", MergedDataCols)

# expand abbreviations and clean up names
MergedDataCols <- gsub("^f", "frequencyDomain", MergedDataCols)
MergedDataCols <- gsub("^t", "timeDomain", MergedDataCols)
MergedDataCols <- gsub("Acc", "Accelerometer", MergedDataCols)
MergedDataCols <- gsub("Gyro", "Gyroscope", MergedDataCols)
MergedDataCols <- gsub("Mag", "Magnitude", MergedDataCols)
MergedDataCols <- gsub("Freq", "Frequency", MergedDataCols)
MergedDataCols <- gsub("mean", "Mean", MergedDataCols)
MergedDataCols <- gsub("std", "StandardDeviation", MergedDataCols)


MergedDataCols <- gsub("BodyBody", "Body",MergedDataCols)
colnames(MergedData) <- MergedDataCols



ActivityMeans <- MergedData %>% group_by(subject, activity) %>% summarise_each(funs(mean))

# write to "tidy_data.txt"
setwd("../")
write.table(ActivityMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)




unlink("data",recursive = TRUE)
unlink("UCI HAR Dataset",recursive = TRUE)

