# Go through each sorted months file and make a data frame from all months with thresholds in it - also attach the number of questions answered that months
# to the data frame for computing ratio of MMA score later
# Set the threshold in this file!

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/pvalues/output_data/"
input_file <- paste(output_dir, "questions_with_months.csv", sep="")


#####
# Start here
#####
all_df <- read.csv(input_file, sep="")

#look only at questions with score values greater than 0 (actually completed) and not NA
all_df[all_df == 0] <- NA
all_df <- na.omit(all_df)

threshold_df <- data.frame(date = character(0), chw = character(0), question = numeric(0), 
				score = numeric(0), month_mean = numeric(0), month_mean = numeric(0), threshold = numeric(0), num_questions = numeric(0))

i <- 1

months <- unique(all_df$month)

for (m in months){
	
	one_month <- all_df[all_df$month == m,]
	
	print(one_month)
	
	month_mean <- mean(one_month$score, na.rm=T) 
	month_sd <- sd(one_month$score, na.rm=T)
	
	#set the threshold for this week
	threshold <- month_mean + month_sd*2
	
	num_questions <- nrow(one_month)
		
	#add row to threshold_df	
	one_month <- cbind(one_month, month_mean, month_sd, threshold, num_questions)		
	threshold_df <- rbind(threshold_df, one_month)
	
	i <- i + 1
}
		
output_csv <- paste(output_dir, "find_outliers/threshold_df-questions_months.csv", sep="")
write.table(threshold_df, output_csv)	

