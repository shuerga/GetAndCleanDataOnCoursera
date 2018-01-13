#This script does the following.

# 1 Merges the training and the test sets to create one data set.
# 2 Extracts only the measurements on the mean and standard deviation for each measurement.
# 3 Uses descriptive activity names to name the activities in the data set
# 4 Appropriately labels the data set with descriptive variable names.
# 5 From the data set in step 4, creates a second, independent tidy data set with the average of each 
#   variable for each activity and each subject.

# Set working directory
setwd("C:/Users/sghue/Documents/R")

# Verifies that the required libraries get installed if needed
verify_lib <- function(...) {
    lapply(list(...), function(lib) {
        if (!lib %in% installed.packages()) 
            install.packages(lib)
    })
}
verify_lib("dplyr", "reshape2")

# Load the libraries
library("dplyr")
library("reshape2")

# The main data directory
ds_dir <- "UCI HAR Dataset"

# Fuction used to load files where filename = name of the file to load, ellipsis = comma delimited list of directories
load_file <- function(filename, ...) {
    file.path(..., filename) %>%
        read.table(header = FALSE)
}

# Loads a training and file
load_train_file <- function(filename) {
    load_file(filename, ds_dir, "train")
}
load_test_file <- function(filename) {
    load_file(filename, ds_dir, "test")
}

# Uses list of activity values to describe test / training labels where
#   ds = label dataset
#   Returns: the original dataset with human readable column name and values
describe_lbl_ds <- function(ds) {
    names(ds) <- activity_col  
    ds$Activity <- factor(ds$Activity, levels = activity_lbl$V1, labels = activity_lbl$V2)
    ds
}

# Takes a dataset capturing results of feature tests and associates columns with individual features
#   ds = activity dataset
#   Returns: the original dataset with columns indicating which feature it describes
describe_act_ds <- function(ds) {
    col_names <- gsub("-", "_", features$V2)
    col_names <- gsub("[^a-zA-Z\\d_]", "", col_names)
    names(ds) <- make.names(names = col_names, unique = TRUE, allow_ = TRUE)
    ds
}

## Adjusts column name in the data set identifying test participants
describe_sub_ds <- function(ds) {
    names(ds) <- subject_col
    ds
}

# Download and extract a zip file with datasets
if (!file.exists(ds_dir)) {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    dest_file <- "Dataset.zip"
    download.file(url, destfile = dest_file, method = "curl")
    unzip(dest_file)
    if (!file.exists(ds_dir)) 
        stop("The downloaded dataset doesn't have the expected structure!")
}

# Custom columns
subject_col <- "Subject"
activity_col <- "Activity"

# Load features as human-readable column names
features <- load_file("features.txt", ds_dir)

# Load activity labels
activity_lbl <- load_file("activity_labels.txt", ds_dir)

# Use descriptive activity names to name the activities in the data set
# Training data
train_set <- load_train_file("X_train.txt") %>% describe_act_ds
train_lbl <- load_train_file("y_train.txt") %>% describe_lbl_ds
train_sub <- load_train_file("subject_train.txt") %>% describe_sub_ds

# Test data
test_set <- load_test_file("X_test.txt") %>% describe_act_ds
test_lbl <- load_test_file("y_test.txt") %>% describe_lbl_ds
test_sub <- load_test_file("subject_test.txt") %>% describe_sub_ds

# Merge the training and the test sets to create one dataset
# Extract only the measurements on the mean and standard deviation for each measurement
merge_data <- rbind(
    cbind(train_set, train_lbl, train_sub),
    cbind(test_set, test_lbl, test_sub)
) %>%
    select(
        matches("mean|std"), 
        one_of(subject_col, activity_col)
    )

# Create a second, independent tidy data set with the average of each variable for each activity and each subject
id_cols <- c(subject_col, activity_col)
tidy_data <- melt(
    merge_data, 
    id = id_cols, 
    measure.vars = setdiff(colnames(merge_data), id_cols)
) %>%
    dcast(Subject + Activity ~ variable, mean)

# Save the result in txt
write.table(tidy_data, file = "data.txt", sep = ",", row.names = FALSE)

# Save the result in csv as it eassy to read in GitHub
write.table(tidy_data, file = "data.csv", sep = ",", row.names = FALSE)
