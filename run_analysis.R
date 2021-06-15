
# load libraries; set WD ------------------------------------------------------
library(dplyr)

setwd("C:/Users/u41030/Coursera/Getting and Cleaning Data/Final Project")


# read in data ----------------------------------------------------------------

# features / activity
features <- read.delim(file = "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/features.txt",
                       sep = " ",
                       header = F)
activity_labels <- read.delim(file = "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt",
                              sep = " ",
                              header = F)

# train data
x_train <- read.fwf(file = "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt",
                    widths = c(17, rep(16, 560)),
                    header = F)
y_train <- read.delim(file = "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt",
                      header = F)
subject_train <- read.delim(file = "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt",
                            header = F)

# test data 
x_test <- read.fwf(file = "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt",
                   widths = c(17, rep(16, 560)),
                   header = F)
y_test <- read.delim(file = "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt",
                     header = F)
subject_test <- read.delim(file = "getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt",
                           header = F)


# combine train and test data -------------------------------------------------

# get index of mean and stdev columns
f_index <- c(grep(c("mean()"), features$V2, fixed = T),
             grep(c("std()"), features$V2, fixed = T)) %>% 
  sort()

# combine train and test
data <- rbind(x_train, x_test) %>% 
  select(all_of(f_index))

# add descriptive column names
names(data) <- features[f_index, 2]

# add activity names and subject IDs
activity <- rbind(y_train, y_test) %>% 
  left_join(activity_labels,
            by = c("V1")) %>% 
  rename(activity_id = V1, 
         activity = V2) %>% 
  select(-"activity_id") 

subject <- rbind(subject_train, subject_test) %>% 
  rename(subject_id = V1)

data <- data %>% 
  cbind(activity, subject) %>% 
  select(-"activity_id") 


# create table of avgs by subject and activity --------------------------------

avgs <- data %>% 
  group_by(activity, subject_id) %>% 
  summarize(across(-c(activity, subject_id), mean))

avgs <- data %>% 
  group_by(activity, subject_id) %>% 
  summarize(across(1:66, mean))

