# run_analysis.R does the following:

# 1. Merges the training and the test sets to create one data set.

xtrn <- read.table("train/X_train.txt")
xtst <- read.table("test/X_test.txt")
X <- rbind(xtrn, xtst)

strn <- read.table("train/subject_train.txt")
stst <- read.table("test/subject_test.txt")
S <- rbind(strn, stst)

ytrn <- read.table("train/y_train.txt")
ytst <- read.table("test/y_test.txt")
Y <- rbind(ytrn, ytst)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")
gfi <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, gfi]
names(X) <- features[gfi, 2]
names(X) <- gsub("\\(|\\)", "", names(X))
names(X) <- tolower(names(X))

# 3. Uses descriptive activity names to name the activities in the data set

act <- read.table("activity_labels.txt")
act[, 2] = gsub("_", "", tolower(as.character(act[, 2])))
Y[,1] = act[Y[,1], 2]
names(Y) <- "activity"

# 4. Appropriately labels the data set with descriptive variable names. 

names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "mergeddata.txt")

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

uniqueS = unique(S)[,1]
numS = length(unique(S)[,1])
numAct = length(act[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numS*numAct), ]

row = 1
for (s in 1:numS) {
        for (a in 1:numAct) {
                result[row, 1] = uniqueS[s]
                result[row, 2] = act[a, 2]
                tmp <- cleaned[cleaned$subject==s & cleaned$activity==act[a, 2], ]
                result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
                row = row+1
        }
}
write.table(result, "ds_averages.txt")
