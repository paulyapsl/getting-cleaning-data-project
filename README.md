getting-cleaning-data-project
=============================
Objectives: 
This readme file provides information on the run_analysis.R script, a requirement of Coursera’s Getting and Cleaning Data course project work. The reader is encouraged to read the accompanying analysis_run.R source codes and the code book describing the variables.

Background information of the study:
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details.

Original Source Data:
The source data is extracted from the zip files below:
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

Source: Human Activity Recognition Using Smartphones Dataset, Version 1.0, Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto (extracted from the Readme.txt of the above original source data).

Data:

The below data from the zip files are used in the following data transformative process:
-	activity_labels.txt (numeric labels of 1-6 to the six activities) 
-	features.txt (list of 561 features vector (with its numeric label), with time and frequency domain available)
-	/train/X_train.txt (training set, 7352 observations of 561 variables of features vector)
-	/train/y_train.txt (training labels, 7352 observations of numeric activity labels)
-	/train/subject_train.txt (each row identifies the subject who performed the activity for each window sample, range from 1-30, 7352 observations)
-	/test/X_test.txt (test set)
-	/test/y_test.txt (test labels)
-	/test/subject_test.txt (each row identifies the subject who performed the activity for each window sample, range from 1-30)

Transformation / Work to clean up the data
Below describes the transformative process and work to clean up the data, note that they are not in the numeric orders below (please see the source code for details):

1)	Merges the training and the test sets to create one data set

-	Train and Test “X” files specified above from the zip file are read to a specified directory, with their variables (headers) labeled from the features.txt file. These are combined with their respective “Y” and “subject” files, which are the activity and subject data respectively. New files train_data (and test_data) are created using the cbind funtion. 

At this stage, the activity_labels file (which contains the activity mapping) is not mapped as it does not allow the correct sequence of the data to be mapped to either the x_train or x_test data.
-	The train_data and test_data are then combined using rbind to create a new dataframe: combine_data, as they follow the same data frame structure. Recap that train and test datasets are generated by 70% and 30% of the volunteers respectively
-	The activity_labels are then mapped to the combine_data to provide a description of the activities, defined as combine_data2 (see #3)
-	To create this combine_data2 set in the specified directory, a write.table function is used

2) Extracts only the measurements on the mean and standard deviation for each measurement. 

-	From the earlier combine_data2 dataset, the column headers are extracted and defined as the combine_data_colnames object
-	To extract variables (columns) that contain mean and std that will also include essential information such as activity, activity_id and subject, the grepl function is used.  The grepl function will return TRUE if it satisfies the condition in the argument, and they are returned to a newly defined combine_data_meanstd object. Note that the “meanFreq” variable has been excluded as it is strictly neither a mean nor a frequency per say.
-	The relevant columns containing the desired “mean” and “std” are extracted by defining combine_data_me=TRUE into a new object extract_meanstd

3) Uses descriptive activity names to name the activities in the data set
-	Note: The descriptive activities (eg. walking, sitting) are merged to the combined datasets in the more advanced stages of the data transformation to ensure that the subject, activity and variables structure are intact.
-	After the combination of the train and test datasets in #1, the activity_labels dataset is mapped to the combine_data dataset to create the combine_data2 object.

4) Appropriately labels the data set with descriptive variable names. 
-	From the extract_meanstd dataset defined above, the columns names are extracted into the column_names object. The For looping and gsub functions are used to find and replace the entire column_names with one that is more descriptive (for example, std = StdDev, t=Time, Mag=Magnitude, f=Frequency).
-	The renamed column_names are then returned back to the column headers of the extract_meanstd dataset.

5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
-	From the extract_meanstd dataset from #4, the melt function is first used to restructure the column variables of the datasets into the rows first. Only subject_id, activity and activity_id are defined in the id argument (the rest are assumed to be the variables to be “melted”). This new object is created as melt_data.
-	Now, to get the mean of the variables columns melted earlier, the dcast function will recast the variables back into the columns, with the mean function applied to the recast dataset defined as cast_data.
-	To create the cast_data dataset as a text file, we use a write.table function specifying the directory in which the file will be created.
