# Create a correlation matrix from different files

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/pvalues/output_data/"


#####
# Start here
#####
df <- read.csv(input_file, sep="")

#read in the necessary files
total_scores_file <- read.csv(paste(output_dir, "cumulative_total_scores-chws.csv"))
cumulative_scores_file <- read.csv(paste(output_dir, "cumulative_total_scores-chws.csv"))
overall_rank_file <- read.csv(paste(output_dir, "sorted_scores-all_collection.csv"))
anomalous_questions_file <- read.csv(paste(output_dir, "find_outliers/top_outliers-total_questions_over.csv"))
anomalous_qs_percent_file <- read.csv(paste(output_dir, "find_outliers/top_outliers-total_questions_over.csv"))
anomalous_weeks_file <- read.csv(paste(output_dir, "find_outliers/over_per_week.csv"))
anomalous_weeks_percent_file <- read.csv(paste(output_dir, "find_outliers/over_per_week.csv"))
anomalous_months_file <- read.csv(paste(output_dir, "find_outliers/over_per_month.csv"))
anomalous_months_percent_file <- read.csv(paste(output_dir, "find_outliers/over_per_month.csv"))

#pull out the needed columns

#A good way to organize all of these correlations is through a correlation matrix.  In other words, if we have M different scores that we are able to compute for each CHW, create an MxM matrix (just fill in the top-right half) where each cell is populated by the correlation between the scores in the row and column.  For now, the different scores are (add more if I'm missing some):
#X - naive MMA score (total) -- range roughly 0 to [700?]
#X - cumulative MMA score, formed by summing MMA scores for each week -- range roughly 0 to [700?]
#X - overall rank by naive MMA score -- range 0 to 228
#X - # of questions found to be anomalous -- range roughly 0 to 38 x 75
#X - percent of questions found to be anomalous -- range roughly 0 to 1 [same as above, but divide by number of questions with data]
#X - # of weeks CHW is found to be anomalous -- range roughly 0 to 38
#X - percent of weeks CHW is found to be anomalous -- range roughly 0 to 1 [same as above, but divide by number of weeks when CHW has data]
#X - # of months CHW is found to be anomalous -- range roughly 0 to 9
#X - percent of months CHW is found to be anomalous -- range roughly 0 to 1 [same as above, but divide by number of months when CHW has data]



# output_csv <- paste(output_dir, "correlation_matrix.csv", sep="")
# write.table(over_df, output_csv)	
# 
# dev.off()
  
