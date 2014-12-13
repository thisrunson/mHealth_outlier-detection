# Go through each sorted week file and make a data frame from all weeks with thresholds in it
# Set the threshold in this file!

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/output_data/"
input_dir <- paste(output_dir, "sliding_window-weeks/", sep="")


#####
# Start here
#####
#loop through a file with all output weeks data and read into variables
setwd(input_dir)

threshold_df <- data.frame(week = character(0), chw = character(0), score = numeric(0), rank = numeric(0), week_mean = numeric(0), week_sd = numeric(0), week_threshold = numeric(0))

i <- 1

#go through the sorted output files by week, compute the threshold for that week and stick the data into a single data frame
for (file_name in list.files()) {
	one_week <- read.csv(file_name, head=F, sep="", skip=1)
		
	#files are already sorted - the row is the CHW's rank for that week
	rank <- c(1:nrow(one_week))
	
	colnames(one_week) <- c("chw", "score")
	
	week_mean <- mean(one_week$score) 
	week_sd <- sd(one_week$score)
	
	#set the threshold for this week
	threshold <- week_mean + week_sd*2
	
	#add row to cohort_df	
	one_week <- cbind(i, one_week, rank, week_mean, week_sd, threshold)		
	threshold_df <- rbind(threshold_df, one_week)

	i <- i + 1
}

num_weeks <- i

colnames(threshold_df) <- c("week", "chw", "score", "rank", "week_mean", "week_sd", "threshold")

print(threshold_df)
		
output_csv <- paste(output_dir, "threshold_df.csv", sep="")
write.table(threshold_df, output_csv)	

