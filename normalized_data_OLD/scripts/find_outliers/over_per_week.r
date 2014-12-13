# Open the threshold_df-questions_weeks.csv, go through it for each CHW, determine each week the CHW is "an outlier"
# Need to determine what makes the CHW an outlier - for now, tell each week the CHW has ANY scores above the threshold
# Plot a histogram - number of weeks each CHW is an outlier

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/normalized_data/output_data/"
input_file <- paste(output_dir, "find_outliers/threshold_df-questions_weeks.csv", sep="")

plot_df <- function(over_df){
	
	over_df$over_weeks <- as.numeric(as.character(over_df$over_weeks))
		
	#compute summary stats for weeks over
	ratio_mean <- mean(over_df$over_weeks)
	ratio_sd <- sd(over_df$over_weeks)

	one_sd <- ratio_mean + ratio_sd
	two_sd <- ratio_mean + 2*ratio_sd

	pdf(output_pdf <- paste(output_dir, "find_outliers/over_per_week", ".pdf", sep=""), height=12, width=12, onefile=TRUE)
			
	#plot a histogram of the number of weeks each CHW is an outlier
	plot <- ggplot(data=over_df, aes(over_weeks), environment = environment()) + geom_histogram(binwidth=1) + 
			ggtitle("Number of Weeks each CHW is an Outlier - has one score > threshold") + geom_vline(aes(xintercept = ratio_mean), colour="blue") + 
			geom_vline(aes(xintercept = one_sd), colour="red") + geom_vline(aes(xintercept = two_sd), color="red", linetype="longdash") 
			#	+ coord_cartesian(xlim = c(0, 30), ylim = c(0, 2500)) 

	print(plot)	
}

# compute_weeks_over <- function(one_chw){
# 	unique_weeks <- unique(one_chw$week_num)
# 		
# 	over_weeks <- 0
# 		
# 	for (week in unique_weeks){
# 		if (TRUE %in% one_chw$is_over) over_weeks <- over_weeks + 1
# 	}
# 	
# 	print(one_chw)
# 	print(paste("over weeks: ", over_weeks))
# 	
# 	return(over_weeks)
# }

#####
# Start here
#####
df <- read.csv(input_file, sep="")

over_df<- data.frame(chw = character(0), over_count = numeric(0), over_weeks = numeric(0), over_ratio = numeric(0), nweeks_reporting = numeric(0))

unique_chws <- unique(df$chw)

#for each CHW, find score counts and ratios over the threshold, put this in the over_df
for (c in unique_chws){
	
	this_chw <- df[df$chw == c,]	
		
	#make a new col - "is_over" - with TRUE or FALSE values each week for whether that question's score > threshold
	this_chw <- transform(this_chw, is_over = (this_chw$score > this_chw$threshold))
		
	#find the number of weeks reporting data total
	unique_weeks <- unique(this_chw$week_num)
	nweeks_reporting <- length(unique_weeks)
	
	#find the number of weeks with at least one outlying value
	weeks_with_overs <- this_chw[this_chw$is_over == TRUE,]	
	unique_overs <- unique(weeks_with_overs$week_num)
	
	#store number of unique weeks over as over_weeks
	over_weeks <- length(unique_overs)
	
	#find the "ratio" for the weeks - the number of weeks with an outlier / number of weeks reported by that CHW, or % of weeks > threshold
	over_ratio <- length(over_weeks) / nweeks_reporting	
	
	print(paste("over_weeks: ", over_weeks, "over_ratio: ", over_ratio, "nweeks reporting: ", nweeks_reporting, sep=" "))
	
	# #find the number of times a CHW's questions are over the threshold across all weeks
	over_count <- sum(this_chw$is_over)
	 
	# #send these scores to compute_weeks to determine for how many weeks that CHW has an outlier score
	# over_weeks <- compute_weeks_over(this_chw)
	
	one_row <- cbind(c, over_count, over_weeks, over_ratio, nweeks_reporting)
	
	over_df <- rbind(over_df, one_row)
}

#print(over_df)

plot_df(over_df)
	
output_csv <- paste(output_dir, "find_outliers/over_per_week.csv", sep="")
write.table(over_df, output_csv)	

dev.off()
  
