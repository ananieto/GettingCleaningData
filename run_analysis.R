
### Read the files into R

X_train <- read.table("./UCI HAR Dataset/train/X_train.txt", quote="\"", stringsAsFactors=FALSE)
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt", quote="\"", stringsAsFactors=FALSE)
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt", quote="\"", stringsAsFactors=FALSE)
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt", quote="\"", stringsAsFactors=FALSE)
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt", quote="\"", stringsAsFactors=FALSE)
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt", quote="\"", stringsAsFactors=FALSE)
features <- read.table("./UCI HAR Dataset/features.txt", quote="\"", stringsAsFactors=FALSE)
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt", quote="\"", stringsAsFactors=FALSE)

### Merge the training and test sets to create one data set

mydata<-rbind(X_train, X_test)

### Extract only the measurements on the mean and standard deviation for each measurement

indices <- grep("mean|std", features[,2])
mydata <- mydata[,indices]


### Descriptive activity names to name the activities in the data set

mydata <- cbind(rbind(y_train, y_test), mydata)

for (i in 1:6)
{
        mydata[which(mydata[,1]==i),1]<-activity_labels[which(activity_labels[,1]==i),2]
}


### Appropriately labels the data set with descriptive variable names. 
colnames(mydata) <- c("ActivityNames" ,features[indices,2])
colnames(mydata) <- gsub("-", "",colnames(mydata))
colnames(mydata) <- gsub("[()]", "",colnames(mydata))

### From the data set in step 4, creates a second, independent tidy data set with 
### the average of each variable for each activity and each subject

mydata <- mutate(mydata, subject=c(subject_train[,1], subject_test[,1]))
mydata$subject<-as.factor(mydata$subject)
mydata$ActivityNames<-as.factor(mydata$ActivityNames)


mydata2 <- as.data.table(mydata)[,lapply(.SD,mean), by="ActivityNames,subject"]

write.table(mydata2, file="mytidydata.csv", sep="\t", dec=".", row.names=FALSE)
