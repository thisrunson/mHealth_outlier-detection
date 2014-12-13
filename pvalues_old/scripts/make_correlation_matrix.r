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

total_scores <- read.csv(paste(output_dir, "cumulative_total_scores-chws.csv"))
cumulative_scores <- read.csv(paste(output_dir, "cumulative_total_scores-chws.csv"))
overall_rank <- read.csv(paste(output_dir, "sorted_scores-all_collection.csv"))
anomalous_questions <- read.csv(paste(output_dir, "top_outliers-total_questions_over.csv"))
anomalous_qs_percent <- read.csv(paste(output_dir, "top_outliers-total_questions_over.csv"))

anomalous_weeks <- read.csv(paste(output_dir, "cumulative_total_scores-chws.csv"))
anomalous_weeks_percent <- read.csv(paste(output_dir, "cumulative_total_scores-chws.csv"))
anomalous_months <- read.csv(paste(output_dir, "cumulative_total_scores-chws.csv"))
anomalous_months_percent <- read.csv(paste(output_dir, "cumulative_total_scores-chws.csv"))



#X - naive MMA score (total) -- range roughly 0 to [700?]
#X - cumulative MMA score, formed by summing MMA scores for each week -- range roughly 0 to [700?]
#X - overall rank by naive MMA score -- range 0 to 228
#X - # of questions found to be anomalous -- range roughly 0 to 38 x 75
#X - percent of questions found to be anomalous -- range roughly 0 to 1 [same as above, but divide by number of questions with data]
# - # of weeks CHW is found to be anomalous -- range roughly 0 to 38
# - percent of weeks CHW is found to be anomalous -- range roughly 0 to 1 [same as above, but divide by number of weeks when CHW has data]
# - # of months CHW is found to be anomalous -- range roughly 0 to 9
# - percent of months CHW is found to be anomalous -- range roughly 0 to 1 [same as above, but divide by number of months when CHW has data]



# output_csv <- paste(output_dir, "correlation_matrix.csv", sep="")
# write.table(over_df, output_csv)	
# 
# dev.off()
  
