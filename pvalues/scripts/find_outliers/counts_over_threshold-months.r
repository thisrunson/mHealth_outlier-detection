# Open the threshold_df-questions_weeks.csv, go through it for each CHW, determine the counts of scores above threshold score
# Also compute ratio of questions over score / all questions answered by that CHW

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/pvalues/output_data/"
input_file <- paste(output_dir, "find_outliers/threshold_df-questions_months.csv", sep="")

plot_df <- function(percentover_df){
	
	percentover_df$over_ratio <- as.numeric(as.character(percentover_df$over_ratio))
	percentover_df$over_count <- as.numeric(as.character(percentover_df$over_count))
	
	#compute summary stats using count
	count_mean <- mean(percentover_df$over_count)
	count_sd <- sd(percentover_df$over_count)

	one_sd <- count_mean + count_sd
	two_sd <- count_mean + 2*count_sd

	pdf(output_pdf <- paste(output_dir, "over_counts-number_over_per_chw-months", ".pdf", sep=""), height=12, width=12, onefile=TRUE)
			
	#plot a histogram of the CHW raw count scores
	plot <- ggplot(data=percentover_df, aes(over_count), environment = environment()) + geom_histogram(binwidth=.5) + 
			ggtitle("Score Counts - Number of Questions per CHW over Threshold - 'cumulative' months data") + 
			geom_vline(aes(xintercept = count_mean), 	colour="blue") + geom_vline(aes(xintercept = one_sd), colour="red") + 
			geom_vline(aes(xintercept = two_sd), color="red", linetype="longdash") 
			#	+ coord_cartesian(xlim = c(0, 30), ylim = c(0, 2500)) 

	print(plot)	
}

#####
# Start here
#####
df <- read.csv(input_file, sep="")

percentover_df<- data.frame(chw = character(0), over_count = numeric(0), over_ratio = numeric(0), nweeks_reporting = numeric(0))

unique_chws <- unique(df$chw)

#for each CHW, find score counts and ratios over the threshold, put this in the percentover_df
for (c in unique_chws){
	
	this_chw <- df[df$chw == c,]
		
	#make a new col - "new" - with TRUE or FALSE values each week for whether that question's score > threshold
	this_chw <- transform(this_chw, new = (this_chw$score > this_chw$threshold))
			
	#find the number of times a CHW's questions are over the threshold
	over_count <- sum(this_chw$new)
	
	#find the "ratio" for the scores - the raw counts number / number of questions asked by that CHW
	over_ratio <- over_count / nrow(this_chw)
	
	nmonths_reporting <- nrow(this_chw)
	
	one_row <- cbind(c, over_count, over_ratio, nmonths_reporting)

	percentover_df <- rbind(percentover_df, one_row)
}

plot_df(percentover_df)
	
output_csv <- paste(output_dir, "find_outliers/top_outliers-questions_months_over.csv", sep="")
write.table(percentover_df, output_csv)	

dev.off()
  
