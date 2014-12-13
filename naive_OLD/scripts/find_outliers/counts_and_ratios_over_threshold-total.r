# Open the threshold_df-total.csv, go through it for each CHW, determine the percent of questions for which she is above threshold score

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/output_data/"
input_file <- paste(output_dir, "find_outliers/threshold_df-total.csv", sep="")

plot_df <- function(percentover_df){
	
	colnames(percentover_df) <- c("c", "over_count", "over_ratio", "nweeks_reporting")
	
	percentover_df$over_ratio <- as.numeric(as.character(percentover_df$over_ratio))
	percentover_df$over_count <- as.numeric(as.character(percentover_df$over_count))
		
	ratio_mean <- mean(percentover_df$over_ratio)
	ratio_sd <- sd(percentover_df$over_ratio)

	two_sd <- ratio_mean + 2*ratio_sd

	pdf(output_pdf <- paste(output_dir, "over_ratios-percent_over_per_chw-total", ".pdf", sep=""), height=12, width=12, onefile=TRUE)

	#plot a histogram of the CHW ratio scores
	plot <- ggplot(data=percentover_df, aes(over_ratio), environment = environment()) + geom_histogram() + 
			ggtitle("Ratio Counts - % Questions per CHW over Threshold - 'total' data") + geom_vline(aes(xintercept = ratio_mean), colour="blue") + 
			geom_vline(aes(xintercept = ratio_sd), colour="red") + geom_vline(aes(xintercept = two_sd), color="red", linetype="longdash") 
			#	+ coord_cartesian(xlim = c(0, 30), ylim = c(0, 2500)) 

	print(plot)	
}

#####
# Start here
#####
df <- read.csv(input_file, sep="")

percentover_df<- data.frame(chw = character(0), num_over = numeric(0), percent_over = numeric(0), n_questions = numeric(0))

unique_chws <- unique(df$chw)

#for each CHW, find percent weeks over the threshold, put this in the percentover_df
for (c in unique_chws){
	
	this_chw <- df[df$chw == c,]
	
	#make a new col - over_counts - with TRUE or FALSE values each week for whether that week's score > threshold
	this_chw <- transform(this_chw, new = (this_chw$score > this_chw$threshold))
		
	#find the percent of time this happens and add it to this_chw$percent_over
	num_over <- sum(this_chw$new)
	percent_over <- num_over / nrow(this_chw) 
	
	nquestions <- nrow(this_chw)
	
	one_row <- cbind(c, num_over, percent_over, nquestions)
	percentover_df <- rbind(percentover_df, one_row)
}

print(percentover_df)

plot_df(percentover_df)
	
output_csv <- paste(output_dir, "top_outliers-total_questions_over.csv", sep="")
write.table(percentover_df, output_csv)	
 
