# Go through the total output file and make a data frame from all data with thresholds in it - also attach the number of questions answered
# Set the threshold in this file!

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/normalized_data/output_data/"
input_file <- paste(output_dir, "all_data-normalized.csv", sep="")


#####
# Start here
#####
all_df <- read.csv(input_file, sep="")

#look only at questions with score values greater than 0 (actually completed) and not NA
all_df[all_df == 0] <- NA
all_df <- na.omit(all_df)

threshold_df <- data.frame(chw = character(0), question = numeric(0), 
				score = numeric(0), all_mean = numeric(0), all_sd = numeric(0), threshold = numeric(0), num_questions = numeric(0))

all_mean <- mean(all_df$score, na.rm=T) 
all_sd <- sd(all_df$score, na.rm=T)

#set the threshold for this week
threshold <- all_mean + all_sd*2

num_questions <- nrow(all_df)

threshold_df <- cbind(all_df, all_mean, all_sd, threshold, num_questions)

print(threshold_df)
		
output_csv <- paste(output_dir, "threshold_df-total.csv", sep="")
write.table(threshold_df, output_csv)	

