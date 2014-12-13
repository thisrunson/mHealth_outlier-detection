# Open the over_per_week.csv, go through it for each CHW, plot a histogram - % of weeks each CHW is an outlier

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/naive/output_data/"
input_file <- paste(output_dir, "find_outliers/over_per_week.csv", sep="")

plot_df <- function(over_df){
	
	over_df$over_weeks <- as.numeric(as.character(over_df$over_weeks))
	over_df$over_ratio <- as.numeric(as.character(over_df$over_ratio))	
		
	print(over_df)
			
	#compute summary stats for weeks over
	ratio_mean <- mean(over_df$over_ratio)
	ratio_sd <- sd(over_df$over_ratio)

	one_sd <- ratio_mean + ratio_sd
	two_sd <- ratio_mean + 2*ratio_sd

	pdf(output_pdf <- paste(output_dir, "find_outliers/over_per_week-ratios", ".pdf", sep=""), height=12, width=12, onefile=TRUE)
			
	#plot a histogram of the % of weeks over threshold
	plot <- ggplot(data=over_df, aes(over_ratio), environment = environment()) + geom_histogram(binwidth=0.01) + 
			ggtitle("Percent Counts - % of Weeks Each CHW is over Threshold - 'cumulative' weeks data") + 
			geom_vline(aes(xintercept = ratio_mean), colour="blue") + geom_vline(aes(xintercept = one_sd), colour="red") + 
			geom_vline(aes(xintercept = two_sd), color="red", linetype="longdash") 
			#	+ coord_cartesian(xlim = c(0, 30), ylim = c(0, 2500)) 

	print(plot)	
}


#####
# Start here
#####
over_df <- read.csv(input_file, sep="")

plot_df(over_df)

dev.off()
  
