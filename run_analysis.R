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
#  run()		:  Main function.  Call directly to run program.
#
#  get_merge()		:  Merges the files 'X_test.txt' and 'X_train.txt' 
#			   returning dataset
# 
#  name_cols(df)	:  Extracts 'mean' and 'std' variables from the dataset
#			   subsetting the original and applying descriptive names.
#			   Takes a dataset as a parameter.
#
#  write_file(df)	:  Writes file to disk.
#			   Takes a dataset as a parameter.
################################################################################

run <- function()
{
	
	library(dplyr)	     # For tbl_df()
	library(data.table)  # for fread(), rbindlist()

	# get merged data set
	combined <- get_merge()

	# Select columns with only 'mean' and 'row' and label appropriately
	combined <- name_cols(combined) 

	# Fix minor error with double 'Body' in column names
	names(combined) <- sub("BodyBody", "Body", names(combined))	

	# Calculate averages on all columns
	averages <- cbind(Averages = colMeans(combined, na.rm=T), 
			  "Activity Names"=names(combined)) 

	# Write file out to 'averages.csv'
	write_file(averages)
}

# Save to file 'averages.csv'		
write_file <- function(averages = NULL)
{
	write.table(averages, file="./averages.csv", sep=",", row.names=FALSE)
}

# function to load two data sets and return the merged version
get_merge <- function()
{
	# load 'train/X_train.txt': Training set.
	train_set <- fread("UCI HAR Dataset/train/X_train.txt")
	# load 'test/X_test.txt'  : Test set.
	test_set <- fread("UCI HAR Dataset/test/X_test.txt")
	
	# merge both files together
	combined <- tbl_df(rbindlist(list(train_set,test_set)))

	# return merged dataset
	combined
}

# Subset merged array and apply descriptive column names
name_cols <- function(combined = NULL)
{
	# read file with all 561 column names from 'features.txt'
	features <- read.table("UCI HAR Dataset/features.txt")
	
	## Extract only the measurements on the mean and standard deviation for each measurement. 
	# (Based off of given criteria it seemed to me meanFreq() is neither of the two so 
	# I omitted that from my subset. e.g. '\\(')
   	valid_names <- subset(features, grepl(paste(c("mean\\(","std\\("), collapse="|"), V2))	
	
	## Format 'patt' vars into regEx for exact match of 'V(valid_names$V1)'
        patt <- sprintf('^V%i$', valid_names$V1)

	## Create a single string of arguments separated by OR - '|"
	patt <- paste(as.character(patt), collapse="|")

	# Match 'patt' with the column names of 'combined' and subset 'combined'
	# appropriately on the right columns
	col_ids <- grepl(patt,colnames(combined))
	combined <- combined[,col_ids]

	# Change Column names to the ones provided in 'features.txt'
	names(combined) <- valid_names$V2
	
	combined	
}
