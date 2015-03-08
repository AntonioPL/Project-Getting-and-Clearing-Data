# This R script does the following:
# 0. Set Working Directory:
setwd("D:/MOOCs/Coursera/2015 03 - 2015 03 Getting and Clearing Data/Project")
# 1. Merges the training and the test sets to create one data set.
X.Train <- read.table("X_train.txt")
X.Test<- read.table("X_test.txt")
Y.Train <- read.table("y_train.txt")
Y.Test <- read.table("y_test.txt")
Y.Train$Group="Training"
Y.Test$Group="Test"
S.Train <- read.table("subject_train.txt")
S.Test <- read.table("subject_test.txt")
X <- rbind(X.Train, X.Test)
Y <- rbind(Y.Train, Y.Test)
S <- rbind(S.Train, S.Test)
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("features.txt")
good_features_indexes <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, good_features_indexes]
names(X) <- features[good_features_indexes, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
# 3. Uses descriptive activity names to name the activities in the data set.
activities <- read.table("activity_labels.txt")
Y[,1] = activities[Y[,1], 2]
names(Y) <- c("activity","group")
# 4. Appropriately labels the data set with descriptive activity names.
names(S) <- "subject"
Data <- cbind(S, Y, X)
write.table(Data, "merged clean data.txt")
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(Data)[2]
Data2 = Data[1:(numSubjects*numActivities),-c(3) ]
row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    Data2[row, 1] = uniqueSubjects[s]
    Data2[row, 2] = activities[a, 2]
    tmp = Data[Data$subject==s & Data$activity==activities[a, 2], ]
    Data2[row, 3:(numCols-1)] = colMeans(tmp[, 4:numCols])
    row = row+1
  }
}
write.table(Data2, "features averages per subject and activity dataset.txt",row.name=FALSE)