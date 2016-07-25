run_analysis <- function() {
    
    ## Preconditions/assumptions for this function: ##
    #
    # 1.) data set was unzipped into the working directory
    #     folder "UCI HAR Dataset" is in the working directory
    #
    # 2.) columns measuring "meanFreq" were not intended to be included 
    #     in the result set. An alternative grep command to catch that
    #     data immediately follows the grep command if this is incorrect
    
    ##################################################################
    ## load data into variables from files
    ##################################################################    

    #get the column headings and activity categories for both test sets
    tblActyLabels <- read.table("./UCI HAR DataSet/activity_labels.txt")
    tblFeatures <- read.table("./UCI HAR DataSet/features.txt")

    #get training data set
    tblTrainingData <- read.table("./UCI HAR DataSet/train/X_train.txt")
    tblTrainingLabels <-read.table("./UCI HAR DataSet/train/y_train.txt")
    tblTrainingSubjects <-read.table("./UCI HAR DataSet/train/subject_train.txt")

    #get test data set
    tblTestData <- read.table("./UCI HAR DataSet/test/X_test.txt")
    tblTestLabels <-read.table("./UCI HAR DataSet/test/y_test.txt")
    tblTestSubjects <-read.table("./UCI HAR DataSet/test/subject_test.txt")

    ##################################################################
    ## Format Test Data and Merge
    ##################################################################    
    
    ## Add Column Titles to the test and training data tables
    colnames(tblTrainingData) = tblFeatures[,2]
    colnames(tblTestData) = tblFeatures[,2]
    
    ## subset the columns to only retain the "mean" and "std" columns
    
    #Note: if assumption #2 is incorrect, switch commenting on below two lines
    lstSelectedColumns <- tblFeatures[grep("mean\\(|std\\(", tblFeatures[,2]),]
    #lstSelectedColumns <- tblFeatures[grep("mean|std", tblFeatures[,2]),]
    
    # subset the data according to objective 2: "extract only the measurements 
    # on the mean and standard deviation for each measurement"
    tblTrainSubset <- tblTrainingData[,lstSelectedColumns[,1]]
    tblTestSubset  <- tblTestData[,lstSelectedColumns[,1]]
    
    ## add the columns from "labels" and "subjects" tables with cbind
    ## apologies to code reviewers, this is an inelegent way of accomplishing 
    ## the task, but it does appear to work for me. 
    
    #step 1: take the training subjects and their activity labels and bind them
    tblTmpTrain <- cbind(tblTrainingSubjects, tblTrainingLabels)
    tblTmpTest <- cbind(tblTestSubjects, tblTestLabels)
    
    #step 2: change column names to be descriptive
    #satisfies "apprpriately labels the data set with descriptive variable
    # names (obj. 3)
    colnames(tblTmpTrain) <- c("TestSubjectID", "ActivityCategory")
    colnames(tblTmpTest) <- c("TestSubjectID", "ActivityCategory")
    
    #step 3: bind new columns to larger data sets
    tblTmpTestStage2 <- cbind(tblTmpTest, tblTestSubset)
    tblTmpTrainStage2 <- cbind(tblTmpTrain, tblTrainSubset)
    
    #step 4: add lookup table data
    tblTestTidy <- merge(tblTmpTestStage2, tblActyLabels, by.x="ActivityCategory", by.y="V1")
    tblTrainTidy <- merge(tblTmpTrainStage2, tblActyLabels, by.x="ActivityCategory", by.y="V1")
    
    #step 5: change column name for the lookup table data
    # satisfies "appropriately labels the data set with descriptive variable 
    # names" and "uses descriptive activity names to name the activities in the 
    # data set" (obj. 3, obj. 4)
    names(tblTestTidy)[names(tblTestTidy) == "V2"] = "ActivityDescription"
    names(tblTrainTidy)[names(tblTrainTidy) == "V2"] = "ActivityDescription"
    
    ## merge the test and training data sets 
    ## satisfies "merge the test and training data sets to 
    ## create one data set (obj. 1)
    tblTidy <- rbind(tblTestTidy,tblTrainTidy)
    
    ## return tidy data set
    tblTidy
    
    ##################################################################    
}


    
    