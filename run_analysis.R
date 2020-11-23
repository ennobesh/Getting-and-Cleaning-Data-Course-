#1.Merges the training and the test sets to create one data set.

#Reading files
X_train <- read.table("./R course/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./R course/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./R course/UCI HAR Dataset/train/subject_train.txt")

X_test <- read.table("./R course/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./R course/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./R course/UCI HAR Dataset/test/subject_test.txt")

#Reading feature vector
features <- read.table('./R course/UCI HAR Dataset/features.txt')

#Reading activity labels
activity_labels = read.table('./R course/UCI HAR Dataset/activity_labels.txt')

#Assigning column names
colnames(X_train) <- features[,2]
colnames(y_train) <-"activityId"
colnames(subject_train) <- "subjectId"

colnames(X_test) <- features[,2] 
colnames(y_test) <- "activityId"
colnames(subject_test) <- "subjectId"

colnames(activity_labels) <- c('activityId','activityType')

#Merging all data in one set
mrg_train <- cbind(y_train, subject_train, X_train)
mrg_test <- cbind(y_test, subject_test, X_test)
combine_data <- rbind(mrg_train, mrg_test)


#2. Extracts only the measurements on the mean and standard deviation for each measurement.

#Reading column names:
colNames <- colnames(combine_data)

#Create vector for ID, mean and standard deviation
mean_and_std <- (grepl("activityId" , colNames) | 
                   grepl("subjectId" , colNames) | 
                   grepl("mean.." , colNames) | 
                   grepl("std.." , colNames) 
)

#Making subset from set_mean_std:
set_mean_std <- combine_data [ , mean_and_std == TRUE]


#3. Uses descriptive activity names to name the activities in the data set

activity_names <- merge(set_mean_std, activity_labels,
                              by='activityId',
                              all.x=TRUE)


#4. Appropriately labels the dataset with descriptive variable names.
#See lines 30-32, 41, 48 and 53


#5. From the data set in Step 4, creates a second, independent tidy data set with the average 
#   of each variable for each activity and each subject.

#Making a second tidy dataset
tidy_set2 <- aggregate(. ~subjectId + activityId, activity_names, mean)
tidy_set2 <- tidy_set2[order(tidy_set2$subjectId, tidy_set2$activityId),]

#Writing second tidy data set in txt file
write.table(tidy_set2, file = "tidy_set2.txt", row.name = FALSE)
