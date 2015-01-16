################################################################################
#  Written by Paul Baecker on 1/15/15
#
#  	This program takes two files and merges them into a single dataframe. 
#  It then subsets this dataframe by extracting only the fields referring to 
#  'mean' and 'std' (not meanFreq), labeled appropriately based off of  
#  variable names in 'features.txt'. The average of all columns are then computed 
#  and stored in a second dataset containing the average and the activity names.
#  This dataset is than written to 'averages.csv'. 
#
#	I am aware it is generally bad practice, but I chose to omit any error 
#  checking at this point for the lack of available time.  
#
#  run()		:  Main function.  Call directly to run program.
#
#  get_merge()		:  Merges the files 'X_test.txt' and 'X_train.txt' 
#			   returning dataset
# 
#  name_cols(df)	:  Extracts 'mean' and 'std' variables from the dataset
#			   subsetting the original and applying descriptive names.
#			   Takes a dataset as a parameter.
#
#  label_activities(df)	:  Appropriately replaces numeric value in Activity with
#			   It's proper string value.
#			   Takes a dataset as a parameter.
################################################################################

run <- function()
{
	
	library(dplyr)	     # For tbl_df(), ddply()
	library(data.table)  # for fread(), rbindlist()

	# get merged data set
	combined <- get_merge()

	# Select columns with only 'mean' and 'row' and label appropriately
	combined <- name_cols(combined) 

	# Label all activities appropriately
	combined <- label_activities(combined)

	# Fix minor error with double 'Body' in column names
	names(combined) <- sub("BodyBody", "Body", names(combined))	

	# Crunch dataset down to unique 'Subject' and calculate means
	combined <- ddply(combined, .(Subject,Activity), numcolwise(mean))

	# Calculate averages on all rows(except 'Subject' & 'Activity')
	# and store in new column 'Averages'
	combined <- cbind(combined, "Averages" = rowMeans(combined[3:68], na.rm=T))

	# Write file out to 'averages.csv'
	write.table(combined, file="./averages.csv", sep=",", row.names=FALSE)
}

# function to load two data sets and return the merged version
get_merge <- function()
{
	# load 'X_train.txt': Training set.
	train_set <- fread("./train/X_train.txt")
	train_set[,"Activity"] <- fread("./train/y_train.txt")
	train_set[,"Subject"] <- fread("./train/subject_train.txt")

	# load 'X_test.txt'  : Test set.
	test_set <- fread("./test/X_test.txt")
	test_set[,"Activity"] <- fread("./test/y_test.txt")
	test_set[,"Subject"]  <- fread("./test/subject_test.txt")


	# merge both files together
	combined <- tbl_df(rbindlist(list(train_set,test_set)))

	# return merged dataset
	combined
}

# Subset merged array and apply descriptive column names
name_cols <- function(combined = NULL)
{
	# read file with all 561 column names from 'features.txt'
	features <- read.table("./features.txt")
	
	## Extract only the measurements on the mean and standard deviation for each measurement. 
	# (Based off of given criteria it seemed to me meanFreq() is neither of the two so 
	# I omitted that from my subset. e.g. '\\(')
   	valid_names <- subset(features, grepl(paste(c("mean\\(","std\\("), collapse="|"), V2))	

	## Format 'patt' vars into regEx for exact match of 'V(valid_names$V1)'
        patt <- sprintf('^V%i$', valid_names$V1)
	valid_names <- rbind(valid_names, data.frame(V1=c(562,563),V2=c("Activity", "Subject")))

	## Create a single string of arguments separated by OR - '|"
	patt <- paste(as.character(c("Subject", "Activity", patt)), collapse="|")

	# Match 'patt' with the column names of 'combined' and subset 'combined'
	# appropriately on the right columns
	col_ids <- grepl(patt,colnames(combined))
	combined <- combined[,col_ids]

	# Change Column names to the ones provided in 'features.txt'
	names(combined) <- valid_names$V2
	
	#  Order columns so 'Subject', 'Activity' and then the rest
	combined[,c(68,67,1:66)]
}

# Function to match 'Activity' numbers to their proper string (e.g. - '1' to 'WALKING')
label_activities <- function(combined = NULL)
{
	activities <- read.table("./activity_labels.txt")
	
	# tried a few ways but was unable to figure this part out from lectures so 
	# resorted to a for loop so it is functional albeit probably slower.
	for(i in 1:nrow(combined))
	{
		# Basic test for all possible cases. 
		switch(combined$Activity[i],
			"1" = (combined$Activity[i] = as.character(activities$V2[1])),
			"2" = (combined$Activity[i] = as.character(activities$V2[2])),
			"3" = (combined$Activity[i] = as.character(activities$V2[3])),
			"4" = (combined$Activity[i] = as.character(activities$V2[4])),
			"5" = (combined$Activity[i] = as.character(activities$V2[5])),
			"6" = (combined$Activity[i] = as.character(activities$V2[6]))
			)  # End switch
	}

	combined
}
