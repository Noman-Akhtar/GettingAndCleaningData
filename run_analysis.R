
## Download and unzip the dataset:
filename <- "zipped_dataset.zip"

if (!file.exists(filename)){
        fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileUrl, filename, method="libcurl")
}  
if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
}



##**********************************************************************************************************
## 1. Read File: "activity_labels.txt"
## 2. convert data.frame of activity labels to a character vector
## 3. clean strings in the vector (spaces and numbers)
##**********************************************************************************************************

## 1. Read File
act_labs <- read.csv("UCI HAR Dataset/activity_labels.txt", header = FALSE)

## 2. Converts data.frame to vector
act_labs <- as.vector(act_labs[,1])
## acts_labs displays in the console as
##                     V1
## 1            1 WALKING
## 2   2 WALKING_UPSTAIRS
## 3 3 WALKING_DOWNSTAIRS
## 4            4 SITTING
## ...so on

## 3. Clean strings in the vector (spaces and numbers)
secondElement <- function(x){x[2]} ## function returns second element
act_labs <- sapply(strsplit(act_labs, " "), secondElement) ## splits labels and returns second element



##**********************************************************************************************************
## 1. Read File: "features.txt"
## 2. convert data.frame of features to a character vector
## 3. clean strings in the vector (spaces and numbers)
## 4. find element numbers where mean() and std() appears
## 5. capture features only with mean() and std
##**********************************************************************************************************

## 1. Read File
feats <- read.csv("UCI HAR Dataset/features.txt", header = FALSE)

## 2. Converts data.frame to vector
feats <- as.vector(feats[,1])
## feats displays in the console similar to acts_lab

## 3. Clean strings in the vector (spaces and numbers)
feats <- sapply(strsplit(feats, " "), secondElement) ## splits features and returns second element

## 4. find element numbers where mean() and std() appears
feats_needed <- grep("mean|std", feats)
feats_needed_length <- length(feats_needed)

## 5. capture features only with mean() and std
feats_mat <- feats[feats_needed]
feats_mat <- c("Subjects", "Activity", feats_mat)

## 6. removes () and -
feats_mat <- gsub("\\(\\)", "",feats_mat)
feats_mat <- gsub("-", "",feats_mat)



##**********************************************************************************************************
## 1. Read File
## 2. Find unique subject IDs
## 3. convert data.frame of features to a character vector
##**********************************************************************************************************

## 1. Read File
test_sub <- read.csv("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
train_sub <- read.csv("UCI HAR Dataset/train/subject_train.txt", header = FALSE)

## 2. Find unique subject IDs
all_ids <- c(1:30)
all_ids_length <- length(all_ids)

## 3. Convert data.frame to matrix
test_sub_mat <- as.matrix(test_sub[[1]])
train_sub_mat <- as.matrix(train_sub[[1]])



##**********************************************************************************************************
## 1. Read File
## 2. convert data.frame of features to a character vector
##**********************************************************************************************************

## 1. Read File
test_y <- read.csv("UCI HAR Dataset/test/y_test.txt", header = FALSE)
train_y <- read.csv("UCI HAR Dataset/train/y_train.txt", header = FALSE)

## 3. Convert data.frame to matrix
test_y_mat <- as.matrix(test_y[[1]])
train_y_mat <- as.matrix(train_y[[1]])



##**********************************************************************************************************
## 1. Read File
## 2. Declare a dummy matrix to record values from x_test/x_train files
## 3. Write the matrix for test/train
## 4. attach test/train subjects to their respective matrix
##**********************************************************************************************************

## 1. Read File
x_test <- readLines("UCI HAR Dataset/test/X_test.txt")
x_test_row <- length(x_test)
x_train <- readLines("UCI HAR Dataset/train/X_train.txt")
x_train_row <- length(x_train)

## 2. Declare a dummy matrix to record values from x_test file
x_test_matrix <- matrix(NA, ncol = feats_needed_length+2, nrow = x_test_row)
x_train_matrix <- matrix(NA, ncol = feats_needed_length+2, nrow = x_train_row)


## 3. Write the matrix for test
for(i in 1:x_test_row) {
        
        ## split each line and remove " "
        x_test_temp <- strsplit(x_test[i], " ")
        
        ## for each col in each row, paste the required feature value
        for(j in 1:feats_needed_length) {
                k <- feats_needed[j]
                x_test_matrix[i,j+2] <- as.numeric(x_test_temp[[1]][k+2])
        }
}
for(i in 1:x_train_row) {
        
        ## split each line and remove " "
        x_train_temp <- strsplit(x_train[i], " ")
        
        ## for each col in each row, paste the required feature value
        for(j in 1:feats_needed_length) {
                k <- feats_needed[j]
                x_train_matrix[i,j+2] <- as.numeric(x_train_temp[[1]][k+2])
        }
}

## 4. attach test subjects to the matrix
x_test_matrix[ ,1] <- test_sub_mat[ ,1]
x_test_matrix[ ,2] <- test_y_mat[ ,1]
x_train_matrix[ ,1] <- train_sub_mat[ ,1]
x_train_matrix[ ,2] <- train_y_mat[ ,1]



##**********************************************************************************************************
## 1. Compile both matrices
##**********************************************************************************************************
full_mat_row <- x_test_row + x_train_row
full_mat_col <- feats_needed_length+2

full_mat <- matrix(NA, ncol = full_mat_col, nrow = full_mat_row)

full_mat_row_count <- 1
test_count <- 1
train_count <- 1

for (i in 1:all_ids_length) {

        while(x_test_matrix[test_count,1] == i & test_count!= x_test_row) {
                full_mat[full_mat_row_count, ] <- x_test_matrix[test_count, ]
                test_count <- test_count +1
                full_mat_row_count <- full_mat_row_count+1
        }
        
        while(x_train_matrix[train_count,1] == i & train_count!= x_train_row) {
                full_mat[full_mat_row_count, ] <- x_train_matrix[train_count, ]
                train_count <- train_count +1
                full_mat_row_count <- full_mat_row_count+1
        }
}



##**********************************************************************************************************
## 1. Arranging the combined matrix in terms of activity
##**********************************************************************************************************
arranged_mat <- matrix(NA, ncol = full_mat_col, nrow = 180)

arranged_mat_count <- 1
full_mat_row_count <- 1

for (each_id in 1:all_ids_length) {
        
        if (full_mat_row_count == 1) {
                start_row <- full_mat_row_count
        } else if (full_mat_row_count - 1 != each_id) {
                start_row <- full_mat_row_count
        }
        
        while (full_mat[full_mat_row_count, 1] == each_id & !is.na(full_mat[full_mat_row_count, 1])) {
                full_mat_row_count <- full_mat_row_count + 1
        }
        
        end_row <- full_mat_row_count - 1
        
        for (activity in 1:6) {
                
                temp_mat <- matrix(ncol = full_mat_col, nrow = full_mat_row)
                temp_mat_count <- 1
                
                for (row_in in start_row:end_row) {
                        
                        if (full_mat[row_in,2] == activity) {
                                temp_mat[temp_mat_count, ] <- full_mat[row_in, ]
                                temp_mat_count <- temp_mat_count + 1
                        }
                        
                }
                
                temp_col_mean <- colMeans(temp_mat[, 3:full_mat_col], na.rm=TRUE)
                temp_col_mean[is.nan(temp_col_mean)] <- " "
                
                arranged_mat[arranged_mat_count, 1] <- temp_mat[1,1]
                arranged_mat[arranged_mat_count, 2] <- act_labs[activity]
                arranged_mat[arranged_mat_count, 3:full_mat_col] <- temp_col_mean

                arranged_mat_count <- arranged_mat_count + 1
                
        }
}

colnames(arranged_mat) <- feats_mat



##**********************************************************************************************************
## 1. Write File: "tidy.txt"\
##**********************************************************************************************************
write.table(arranged_mat, "tidy.txt", row.names = FALSE, quote = FALSE, na = "")



