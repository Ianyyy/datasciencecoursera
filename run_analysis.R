#Step 1

#read data into data frames
x_test = read.table("x_test.txt")
y_test = read.table("y_test.txt")
x_train = read.table("x_train.txt")
y_train = read.table("y_train.txt")
subject_train = read.table("subject_train.txt")
subject_test = read.table("subject_test.txt")
featureNames = read.table("features.txt")
names(x_train) = featureNames$V2
names(x_test) = featureNames$V2
names(subject_train) = "subjectID"
names(subject_test) = "subjectID"
names(y_train) = "activity"
names(y_test) = "activity"
train = cbind(subject_train, y_train, x_train)
test = cbind(subject_test, y_test, x_test)
combined = rbind(train, test)
head(combined[,1:5])

#Step 2:  Extracts only the measurements on the mean and standard
## deviation for each measurement.

meanstdcols = grepl("mean\\(\\)", names(combined)) |
  grepl("std\\(\\)", names(combined))
meanstdcols[1:2] = TRUE
combined = combined[, meanstdcols]
head(combined[,1:10])



#STEP 3: Uses descriptive activity names to name the activities in the data set.


activityNames = read.table("activity_labels.txt", header = FALSE, sep = " ")
head(activityNames)
colnames(activityNames) = c("activity", "activityNames")
data = merge(x=combined, y=activityNames, by = "activity")
data$activity = data$activityNames
head(data)

#Step 4 Appropriately labels the data set with descriptive activity names.

colnames = colnames(data)
colnames = make.names(colnames, unique = TRUE)
head(colnames)


colnamesclean<-gsub("-", " ", colnames) 
colnamesclean<-gsub("\\.", " ", colnamesclean) 
colnamesclean<-gsub("\\  ", " ", colnamesclean)
colnamesclean<-gsub("\\  ", " ", colnamesclean) 
colnamesclean<-gsub("\\  ", " ", colnamesclean) 
colnamesclean<-gsub("tBody", "Body", colnamesclean) 
colnamesclean<-gsub("tGravity", "Gravity", colnamesclean) 
colnamesclean<-gsub("fBody", "Body", colnamesclean) 
colnamesclean<-gsub("BodyBody", "Body", colnamesclean)
colnamesclean<-gsub("^\\s+|\\s+$", "", colnamesclean) 

colnames(data) = colnamesclean

str(data)




#Step 5 Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
library(tidyr)
library(reshape2)

colnames(data) = make.names(colnames(data), unique = TRUE)
tidy = tbl_df(data)
head(tidy)
tidygroup = group_by(tidy, activity, subjectID)
tidymean = summarise_each(tidygroup, funs(mean))
colnames(tidymean) = colnamesclean
tidymean[1:10, 1:6]
write.table(tidymean, file="tidy.txt", row.names=FALSE, col.names=TRUE, sep="\t", quote=TRUE)
