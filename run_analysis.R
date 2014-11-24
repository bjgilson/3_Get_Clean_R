#  Getting and Cleaning Data Project
#  Brian Gilson, 23 November 2014
#  run_analysis.R
#  This script creates the tidy datasets for the course project

#  Reference Project Description
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a
# data set. The goal is to prepare tidy data that can be used for later analysis. 
# You will be graded by your peers on a series of yes/no questions related to the project. 
# You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github
# repository with your script for performing the analysis, and 3) a code book that describes 
# the variables, the data, and any transformations or work that you performed to clean up the
# data called CodeBook.md. You should also include a README.md in the repo with your scripts. 
# This repo explains how all of the scripts work and how they are connected.  

# One of the most exciting areas in all of data science right now is wearable computing 
# - see for example  this article . Companies like Fitbit, Nike, and Jawbone Up are racing 
# to develop the most advanced algorithms to attract new users. The data linked to from the
# course website represent data collected from the accelerometers from the Samsung Galaxy S 
# smartphone. A full description is available at the site where the data was obtained: 
#   
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 
# 
# Here are the data for the project: 
#   
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 
# You should create one R script called run_analysis.R that does the following. 
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 
# 5.From the data set in step 4, creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.
# 
# Good luck!
  
  
# Step 1 : read in all the key files  
X_test <- read.table("C:/rProj/coursera/03_GetClean/data/project/test/X_test.txt", quote="\"", stringsAsFactors=FALSE)
y_test <- read.table("C:/rProj/coursera/03_GetClean/data/project/test/y_test.txt", quote="\"")
subject_test <- read.table("C:/rProj/coursera/03_GetClean/data/project/test/subject_test.txt", quote="\"")
subject_train <- read.table("C:/rProj/coursera/03_GetClean/data/project/train/subject_train.txt", quote="\"")
X_train <- read.table("C:/rProj/coursera/03_GetClean/data/project/train/X_train.txt", quote="\"", stringsAsFactors=FALSE)
y_train <- read.table("C:/rProj/coursera/03_GetClean/data/project/train/y_train.txt", quote="\"")
activity_labels <- read.table("C:/rProj/coursera/03_GetClean/data/project/activity_labels.txt", quote="\"")
features <- read.table("C:/rProj/coursera/03_GetClean/data/project/features.txt", quote="\"")

#Step2 : set up libraries used
library(tidyr)
library(sqldf)
library(dplyr)
library(Hmisc)

# 
# Exploratory analyis to find 86 variables with mean and std in titles
# This will be used to subset the columns 
# sqldf("
#       select *
#       from features
#       where V2 like '%mean%'or V2 like '%std%'
#       ")
# 
# 

# Step 3: Create X_Test_1 with only 86 Variables containing mean and stddev
# based on results of sqldf query above
X_test_1<-X_test %>%
  select(        
    V1,  V2,  V3,	V4,	V5,	V6,	V41,	V42,	V43,	
    V44,	V45,	V46,	V81,	V82,	V83,	V84,	
    V85,	V86,	V121,	V122,	V123,	V124,	V125,	
    V126,	V161,	V162,	V163,	V164,	V165,	V166,	
    V201,	V202,	V214,	V215,	V227,	V228,	V240,	
    V241,	V253,	V254,	V266,	V267,	V268,	V269,	
    V270,	V271,	V294,	V295,	V296,	V345,	V346,	
    V347,	V348,	V349,	V350,	V373,	V374,	V375,	
    V424,	V425,	V426,	V427,	V428,	V429,	V452,	
    V453,	V454,	V503,	V504,	V513,	V516,	V517,	
    V526,	V529,	V530,	V539,	V542,	V543,	V552,	
    V555,	V556,	V557,	V558,	V559,	V560,	V561
  )  


# Step 4 : Give Columns Their Proper Names
# by Renaming
# template: colnames(df)[colnames(df) == 'oldName'] <- 'newName'
colnames(X_test_1)[colnames(X_test_1)=="V1"] <- "tBodyAcc-mean()-X"
colnames(X_test_1)[colnames(X_test_1)=="V2"] <- "tBodyAcc-mean()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V3"] <- "tBodyAcc-mean()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V4"] <- "tBodyAcc-std()-X"
colnames(X_test_1)[colnames(X_test_1)=="V5"] <- "tBodyAcc-std()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V6"] <- "tBodyAcc-std()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V41"] <- "tGravityAcc-mean()-X"
colnames(X_test_1)[colnames(X_test_1)=="V42"] <- "tGravityAcc-mean()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V43"] <- "tGravityAcc-mean()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V44"] <- "tGravityAcc-std()-X"
colnames(X_test_1)[colnames(X_test_1)=="V45"] <- "tGravityAcc-std()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V46"] <- "tGravityAcc-std()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V81"] <- "tBodyAccJerk-mean()-X"
colnames(X_test_1)[colnames(X_test_1)=="V82"] <- "tBodyAccJerk-mean()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V83"] <- "tBodyAccJerk-mean()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V84"] <- "tBodyAccJerk-std()-X"
colnames(X_test_1)[colnames(X_test_1)=="V85"] <- "tBodyAccJerk-std()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V86"] <- "tBodyAccJerk-std()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V121"] <- "tBodyGyro-mean()-X"
colnames(X_test_1)[colnames(X_test_1)=="V122"] <- "tBodyGyro-mean()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V123"] <- "tBodyGyro-mean()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V124"] <- "tBodyGyro-std()-X"
colnames(X_test_1)[colnames(X_test_1)=="V125"] <- "tBodyGyro-std()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V126"] <- "tBodyGyro-std()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V161"] <- "tBodyGyroJerk-mean()-X"
colnames(X_test_1)[colnames(X_test_1)=="V162"] <- "tBodyGyroJerk-mean()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V163"] <- "tBodyGyroJerk-mean()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V164"] <- "tBodyGyroJerk-std()-X"
colnames(X_test_1)[colnames(X_test_1)=="V165"] <- "tBodyGyroJerk-std()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V166"] <- "tBodyGyroJerk-std()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V201"] <- "tBodyAccMag-mean()"
colnames(X_test_1)[colnames(X_test_1)=="V202"] <- "tBodyAccMag-std()"
colnames(X_test_1)[colnames(X_test_1)=="V214"] <- "tGravityAccMag-mean()"
colnames(X_test_1)[colnames(X_test_1)=="V215"] <- "tGravityAccMag-std()"
colnames(X_test_1)[colnames(X_test_1)=="V227"] <- "tBodyAccJerkMag-mean()"
colnames(X_test_1)[colnames(X_test_1)=="V228"] <- "tBodyAccJerkMag-std()"
colnames(X_test_1)[colnames(X_test_1)=="V240"] <- "tBodyGyroMag-mean()"
colnames(X_test_1)[colnames(X_test_1)=="V241"] <- "tBodyGyroMag-std()"
colnames(X_test_1)[colnames(X_test_1)=="V253"] <- "tBodyGyroJerkMag-mean()"
colnames(X_test_1)[colnames(X_test_1)=="V254"] <- "tBodyGyroJerkMag-std()"
colnames(X_test_1)[colnames(X_test_1)=="V266"] <- "fBodyAcc-mean()-X"
colnames(X_test_1)[colnames(X_test_1)=="V267"] <- "fBodyAcc-mean()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V268"] <- "fBodyAcc-mean()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V269"] <- "fBodyAcc-std()-X"
colnames(X_test_1)[colnames(X_test_1)=="V270"] <- "fBodyAcc-std()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V271"] <- "fBodyAcc-std()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V294"] <- "fBodyAcc-meanFreq()-X"
colnames(X_test_1)[colnames(X_test_1)=="V295"] <- "fBodyAcc-meanFreq()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V296"] <- "fBodyAcc-meanFreq()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V345"] <- "fBodyAccJerk-mean()-X"
colnames(X_test_1)[colnames(X_test_1)=="V346"] <- "fBodyAccJerk-mean()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V347"] <- "fBodyAccJerk-mean()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V348"] <- "fBodyAccJerk-std()-X"
colnames(X_test_1)[colnames(X_test_1)=="V349"] <- "fBodyAccJerk-std()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V350"] <- "fBodyAccJerk-std()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V373"] <- "fBodyAccJerk-meanFreq()-X"
colnames(X_test_1)[colnames(X_test_1)=="V374"] <- "fBodyAccJerk-meanFreq()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V375"] <- "fBodyAccJerk-meanFreq()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V424"] <- "fBodyGyro-mean()-X"
colnames(X_test_1)[colnames(X_test_1)=="V425"] <- "fBodyGyro-mean()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V426"] <- "fBodyGyro-mean()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V427"] <- "fBodyGyro-std()-X"
colnames(X_test_1)[colnames(X_test_1)=="V428"] <- "fBodyGyro-std()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V429"] <- "fBodyGyro-std()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V452"] <- "fBodyGyro-meanFreq()-X"
colnames(X_test_1)[colnames(X_test_1)=="V453"] <- "fBodyGyro-meanFreq()-Y"
colnames(X_test_1)[colnames(X_test_1)=="V454"] <- "fBodyGyro-meanFreq()-Z"
colnames(X_test_1)[colnames(X_test_1)=="V503"] <- "fBodyAccMag-mean()"
colnames(X_test_1)[colnames(X_test_1)=="V504"] <- "fBodyAccMag-std()"
colnames(X_test_1)[colnames(X_test_1)=="V513"] <- "fBodyAccMag-meanFreq()"
colnames(X_test_1)[colnames(X_test_1)=="V516"] <- "fBodyBodyAccJerkMag-mean()"
colnames(X_test_1)[colnames(X_test_1)=="V517"] <- "fBodyBodyAccJerkMag-std()"
colnames(X_test_1)[colnames(X_test_1)=="V526"] <- "fBodyBodyAccJerkMag-meanFreq()"
colnames(X_test_1)[colnames(X_test_1)=="V529"] <- "fBodyBodyGyroMag-mean()"
colnames(X_test_1)[colnames(X_test_1)=="V530"] <- "fBodyBodyGyroMag-std()"
colnames(X_test_1)[colnames(X_test_1)=="V539"] <- "fBodyBodyGyroMag-meanFreq()"
colnames(X_test_1)[colnames(X_test_1)=="V542"] <- "fBodyBodyGyroJerkMag-mean()"
colnames(X_test_1)[colnames(X_test_1)=="V543"] <- "fBodyBodyGyroJerkMag-std()"
colnames(X_test_1)[colnames(X_test_1)=="V552"] <- "fBodyBodyGyroJerkMag-meanFreq()"
colnames(X_test_1)[colnames(X_test_1)=="V555"] <- "angle(tBodyAccMean,gravity)"
colnames(X_test_1)[colnames(X_test_1)=="V556"] <- "angle(tBodyAccJerkMean),gravityMean)"
colnames(X_test_1)[colnames(X_test_1)=="V557"] <- "angle(tBodyGyroMean,gravityMean)"
colnames(X_test_1)[colnames(X_test_1)=="V558"] <- "angle(tBodyGyroJerkMean,gravityMean)"
colnames(X_test_1)[colnames(X_test_1)=="V559"] <- "angle(X,gravityMean)"
colnames(X_test_1)[colnames(X_test_1)=="V560"] <- "angle(Y,gravityMean)"
colnames(X_test_1)[colnames(X_test_1)=="V561"] <- "angle(Z,gravityMean)"

# Step 5:  add subjects, activities, and activity labels to X_test_1
# Test dataset :  add Subjects subjects_test
# Test dataset :  add Add Activity  y_test
# Test dataset :  add Add Activity labels   activity_labels

X_test_1<- cbind(subject=subject_test$V1,
                 activityid=y_test$V1,
                 X_test_1)

# Add activity_labels using sqldf

X_test_1 <-
  sqldf("
        select
        a.*,
        'test' as source,
        b.V2 as activity_label
        from X_test_1 a
        join activity_labels b
        on a.activityid=b.V1
        ")

#  Step 6 : Repeat steps 3, 4 and 5 above for Train Dataset= X_train 
X_train_1<-X_train %>%
  select(        
    V1,  V2,  V3,	V4,	V5,	V6,	V41,	V42,	V43,	
    V44,	V45,	V46,	V81,	V82,	V83,	V84,	
    V85,	V86,	V121,	V122,	V123,	V124,	V125,	
    V126,	V161,	V162,	V163,	V164,	V165,	V166,	
    V201,	V202,	V214,	V215,	V227,	V228,	V240,	
    V241,	V253,	V254,	V266,	V267,	V268,	V269,	
    V270,	V271,	V294,	V295,	V296,	V345,	V346,	
    V347,	V348,	V349,	V350,	V373,	V374,	V375,	
    V424,	V425,	V426,	V427,	V428,	V429,	V452,	
    V453,	V454,	V503,	V504,	V513,	V516,	V517,	
    V526,	V529,	V530,	V539,	V542,	V543,	V552,	
    V555,	V556,	V557,	V558,	V559,	V560,	V561
  )%>%

# now fix the column names in X_train
colnames(X_train_1)[colnames(X_train_1)=="V1"] <- "tBodyAcc-mean()-X"
colnames(X_train_1)[colnames(X_train_1)=="V2"] <- "tBodyAcc-mean()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V3"] <- "tBodyAcc-mean()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V4"] <- "tBodyAcc-std()-X"
colnames(X_train_1)[colnames(X_train_1)=="V5"] <- "tBodyAcc-std()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V6"] <- "tBodyAcc-std()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V41"] <- "tGravityAcc-mean()-X"
colnames(X_train_1)[colnames(X_train_1)=="V42"] <- "tGravityAcc-mean()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V43"] <- "tGravityAcc-mean()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V44"] <- "tGravityAcc-std()-X"
colnames(X_train_1)[colnames(X_train_1)=="V45"] <- "tGravityAcc-std()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V46"] <- "tGravityAcc-std()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V81"] <- "tBodyAccJerk-mean()-X"
colnames(X_train_1)[colnames(X_train_1)=="V82"] <- "tBodyAccJerk-mean()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V83"] <- "tBodyAccJerk-mean()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V84"] <- "tBodyAccJerk-std()-X"
colnames(X_train_1)[colnames(X_train_1)=="V85"] <- "tBodyAccJerk-std()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V86"] <- "tBodyAccJerk-std()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V121"] <- "tBodyGyro-mean()-X"
colnames(X_train_1)[colnames(X_train_1)=="V122"] <- "tBodyGyro-mean()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V123"] <- "tBodyGyro-mean()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V124"] <- "tBodyGyro-std()-X"
colnames(X_train_1)[colnames(X_train_1)=="V125"] <- "tBodyGyro-std()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V126"] <- "tBodyGyro-std()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V161"] <- "tBodyGyroJerk-mean()-X"
colnames(X_train_1)[colnames(X_train_1)=="V162"] <- "tBodyGyroJerk-mean()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V163"] <- "tBodyGyroJerk-mean()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V164"] <- "tBodyGyroJerk-std()-X"
colnames(X_train_1)[colnames(X_train_1)=="V165"] <- "tBodyGyroJerk-std()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V166"] <- "tBodyGyroJerk-std()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V201"] <- "tBodyAccMag-mean()"
colnames(X_train_1)[colnames(X_train_1)=="V202"] <- "tBodyAccMag-std()"
colnames(X_train_1)[colnames(X_train_1)=="V214"] <- "tGravityAccMag-mean()"
colnames(X_train_1)[colnames(X_train_1)=="V215"] <- "tGravityAccMag-std()"
colnames(X_train_1)[colnames(X_train_1)=="V227"] <- "tBodyAccJerkMag-mean()"
colnames(X_train_1)[colnames(X_train_1)=="V228"] <- "tBodyAccJerkMag-std()"
colnames(X_train_1)[colnames(X_train_1)=="V240"] <- "tBodyGyroMag-mean()"
colnames(X_train_1)[colnames(X_train_1)=="V241"] <- "tBodyGyroMag-std()"
colnames(X_train_1)[colnames(X_train_1)=="V253"] <- "tBodyGyroJerkMag-mean()"
colnames(X_train_1)[colnames(X_train_1)=="V254"] <- "tBodyGyroJerkMag-std()"
colnames(X_train_1)[colnames(X_train_1)=="V266"] <- "fBodyAcc-mean()-X"
colnames(X_train_1)[colnames(X_train_1)=="V267"] <- "fBodyAcc-mean()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V268"] <- "fBodyAcc-mean()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V269"] <- "fBodyAcc-std()-X"
colnames(X_train_1)[colnames(X_train_1)=="V270"] <- "fBodyAcc-std()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V271"] <- "fBodyAcc-std()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V294"] <- "fBodyAcc-meanFreq()-X"
colnames(X_train_1)[colnames(X_train_1)=="V295"] <- "fBodyAcc-meanFreq()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V296"] <- "fBodyAcc-meanFreq()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V345"] <- "fBodyAccJerk-mean()-X"
colnames(X_train_1)[colnames(X_train_1)=="V346"] <- "fBodyAccJerk-mean()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V347"] <- "fBodyAccJerk-mean()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V348"] <- "fBodyAccJerk-std()-X"
colnames(X_train_1)[colnames(X_train_1)=="V349"] <- "fBodyAccJerk-std()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V350"] <- "fBodyAccJerk-std()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V373"] <- "fBodyAccJerk-meanFreq()-X"
colnames(X_train_1)[colnames(X_train_1)=="V374"] <- "fBodyAccJerk-meanFreq()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V375"] <- "fBodyAccJerk-meanFreq()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V424"] <- "fBodyGyro-mean()-X"
colnames(X_train_1)[colnames(X_train_1)=="V425"] <- "fBodyGyro-mean()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V426"] <- "fBodyGyro-mean()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V427"] <- "fBodyGyro-std()-X"
colnames(X_train_1)[colnames(X_train_1)=="V428"] <- "fBodyGyro-std()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V429"] <- "fBodyGyro-std()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V452"] <- "fBodyGyro-meanFreq()-X"
colnames(X_train_1)[colnames(X_train_1)=="V453"] <- "fBodyGyro-meanFreq()-Y"
colnames(X_train_1)[colnames(X_train_1)=="V454"] <- "fBodyGyro-meanFreq()-Z"
colnames(X_train_1)[colnames(X_train_1)=="V503"] <- "fBodyAccMag-mean()"
colnames(X_train_1)[colnames(X_train_1)=="V504"] <- "fBodyAccMag-std()"
colnames(X_train_1)[colnames(X_train_1)=="V513"] <- "fBodyAccMag-meanFreq()"
colnames(X_train_1)[colnames(X_train_1)=="V516"] <- "fBodyBodyAccJerkMag-mean()"
colnames(X_train_1)[colnames(X_train_1)=="V517"] <- "fBodyBodyAccJerkMag-std()"
colnames(X_train_1)[colnames(X_train_1)=="V526"] <- "fBodyBodyAccJerkMag-meanFreq()"
colnames(X_train_1)[colnames(X_train_1)=="V529"] <- "fBodyBodyGyroMag-mean()"
colnames(X_train_1)[colnames(X_train_1)=="V530"] <- "fBodyBodyGyroMag-std()"
colnames(X_train_1)[colnames(X_train_1)=="V539"] <- "fBodyBodyGyroMag-meanFreq()"
colnames(X_train_1)[colnames(X_train_1)=="V542"] <- "fBodyBodyGyroJerkMag-mean()"
colnames(X_train_1)[colnames(X_train_1)=="V543"] <- "fBodyBodyGyroJerkMag-std()"
colnames(X_train_1)[colnames(X_train_1)=="V552"] <- "fBodyBodyGyroJerkMag-meanFreq()"
colnames(X_train_1)[colnames(X_train_1)=="V555"] <- "angle(tBodyAccMean,gravity)"
colnames(X_train_1)[colnames(X_train_1)=="V556"] <- "angle(tBodyAccJerkMean),gravityMean)"
colnames(X_train_1)[colnames(X_train_1)=="V557"] <- "angle(tBodyGyroMean,gravityMean)"
colnames(X_train_1)[colnames(X_train_1)=="V558"] <- "angle(tBodyGyroJerkMean,gravityMean)"
colnames(X_train_1)[colnames(X_train_1)=="V559"] <- "angle(X,gravityMean)"
colnames(X_train_1)[colnames(X_train_1)=="V560"] <- "angle(Y,gravityMean)"
colnames(X_train_1)[colnames(X_train_1)=="V561"] <- "angle(Z,gravityMean)"

# Train dataset :  add Subjects subjects_train
# Train dataset :  add Add Activity  y_train
# Train dataset :  add Add Activity labels   activity_labels
X_train_1<- cbind(subject=subject_train$V1,
                  activityid=y_train$V1,
                  X_train_1)

# Train dataset : update Activity Labels  via sql
X_train_1 <-
  sqldf("
        select
        a.*,
        'test' as source,
        b.V2 as activity_label
        from X_train_1 a
        join activity_labels b
        on a.activityid=b.V1
        ")



#--------------------------------------------------------------
# Step 7 Merges the training and the test sets to create one data set. 
#---------------------------------------------------------------
X_train_test<-rbind(X_train_1,X_test_1)
str(X_train_test)
#'data.frame':  10299 obs. of  86 variables:


#--------------------------------------------------------------
#  Step 8  Tidy Data from    X_train_test   
#---------------------------------------------------------------

# first gather i.e. stack the data
# ignoring subject, activityid, source, activity_label 
# using gather from tidyr

g<-gather(X_train_test,var_meas,value,-subject,-activity_label)

View(g)
#---------------------------------------------------
# Step 9 
# From the data set in step 4, creates a second, 
# independent tidy data set with the average of each 
# variable for each activity and each subject.
#-----------------------------------------------

res<-
  sqldf("
      select
subject,activity_label, var_meas, avg(value), count(*)
from g
group by 1,2,3      
      ")

#---------------------------------------------------
# Step 10  : Export out the summarized data
#-----------------------------------------------

write.table(g,"./data/project/tidy_data.txt",row.names=FALSE)
write.table(res,"./data/project/tidy_summary.txt",row.names=FALSE)


