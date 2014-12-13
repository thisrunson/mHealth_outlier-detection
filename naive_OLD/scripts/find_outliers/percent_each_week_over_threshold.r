# Open the threshold_df.csv, go through it for each CHW, determine the percent of weeks she is above threshold score

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/output_data/"
input_file <- paste(output_dir, "find_outliers/threshold_df.csv", sep="")


#####
# Start here
#####
df <- read.csv(input_file, sep="")

percentover_df<- data.frame(chw = character(0), percent_over = numeric(0), nweeks_reporting = numeric(0))

unique_chws <- unique(df$chw)

#for each CHW, find percent weeks over the threshold, put this in the percentover_df
for (c in unique_chws){
	
	this_chw <- df[df$chw == c,]
	
	#make a new col - over_counts - with TRUE or FALSE values each week for whether that week's score > threshold
	this_chw <- transform(this_chw, new = (this_chw$score > this_chw$threshold))
		
	#find the percent of time this happens and add it to this_chw$percent_over
	num_over <- sum(this_chw$new)
	percent_over <- num_over / nrow(this_chw) 
	
	nweeks_reporting <- nrow(this_chw)
	
	one_row <- cbind(c, percent_over, nweeks_reporting)
	percentover_df <- rbind(percentover_df, one_row)
}

print(percentover_df)
	
output_csv <- paste(output_dir, "top_outliers-percent_weeks_over.csv", sep="")
write.table(percentover_df, output_csv)	
 
