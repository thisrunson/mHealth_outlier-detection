# Go through each sorted week file and make a data frame from all weeks with thresholds in it - also attach the number of questions answered that week
# to the data frame for computing ratio of MMA score later
# Set the threshold in this file!

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/output_data/"
input_file <- paste(output_dir, "questions-chws-weeks-all.csv", sep="")


#####
# Start here
#####
all_df <- read.csv(input_file, sep="")

#look only at questions with score values greater than 0 (actually completed) and not NA
all_df[all_df == 0] <- NA
all_df <- na.omit(all_df)

threshold_df <- data.frame(date = character(0), chw = character(0), question = numeric(0), 
				score = numeric(0), week_mean = numeric(0), week_sd = numeric(0), week_threshold = numeric(0), num_questions = numeric(0))

i <- 1

weeks <- unique(all_df$date)

for (w in weeks){
	
	one_week <- all_df[all_df$date == w,]
	
	week_mean <- mean(one_week$score, na.rm=T) 
	week_sd <- sd(one_week$score, na.rm=T)
	
	#set the threshold for this week
	threshold <- week_mean + week_sd*2
	
	num_questions <- nrow(one_week)
		
	#add row to threshold_df	
	one_week <- cbind(one_week, week_mean, week_sd, threshold, num_questions)		
	threshold_df <- rbind(threshold_df, one_week)
	
	i <- i + 1
}
		
output_csv <- paste(output_dir, "threshold_df-questions_weeks.csv", sep="")
write.table(threshold_df, output_csv)	

