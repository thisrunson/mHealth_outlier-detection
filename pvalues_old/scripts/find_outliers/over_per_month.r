# Open the threshold_df-questions_months.csv, go through it for each CHW, determine each month the CHW is "an outlier"
# Need to determine what makes the CHW an outlier - for now, tell each month the CHW has ANY scores above the threshold
# Plot a histogram - number of months each CHW is an outlier

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/pvalues_old/output_data/"
input_file <- paste(output_dir, "find_outliers/threshold_df-questions_months.csv", sep="")

plot_df <- function(over_df){
	
	over_df$over_months <- as.numeric(as.character(over_df$over_months))
		
	#compute summary stats for months over
	ratio_mean <- mean(over_df$over_months)
	ratio_sd <- sd(over_df$over_months)

	one_sd <- ratio_mean + ratio_sd
	two_sd <- ratio_mean + 2*ratio_sd

	pdf(output_pdf <- paste(output_dir, "find_outliers/over_per_month", ".pdf", sep=""), height=12, width=12, onefile=TRUE)
			
	#plot a histogram of the number of months each CHW is an outlier
	plot <- ggplot(data=over_df, aes(over_months), environment = environment()) + geom_histogram(binwidth=1) + 
			ggtitle("Number of Months each CHW is an Outlier - has one score > threshold") + geom_vline(aes(xintercept = ratio_mean), colour="blue") + 
			geom_vline(aes(xintercept = one_sd), colour="red") + geom_vline(aes(xintercept = two_sd), color="red", linetype="longdash") 
			#	+ coord_cartesian(xlim = c(0, 30), ylim = c(0, 2500)) 

	print(plot)	
}

# compute_months_over <- function(one_chw){
# 	unique_months <- unique(one_chw$month_num)
# 		
# 	over_months <- 0
# 		
# 	for (month in unique_months){
# 		if (TRUE %in% one_chw$is_over) over_months <- over_months + 1
# 	}
# 	
# 	print(one_chw)
# 	print(paste("over months: ", over_months))
# 	
# 	return(over_months)
# }

#####
# Start here
#####
df <- read.csv(input_file, sep="")

over_df<- data.frame(chw = character(0), over_count = numeric(0), over_months = numeric(0), over_ratio = numeric(0), nmonths_reporting = numeric(0))

unique_chws <- unique(df$chw)

#for each CHW, find score counts and ratios over the threshold, put this in the over_df
for (c in unique_chws){
	
	this_chw <- df[df$chw == c,]			
		
	#make a new col - "is_over" - with TRUE or FALSE values each month for whether that question's score > threshold
	this_chw <- transform(this_chw, is_over = (this_chw$score > this_chw$threshold))
		
	#find the number of months reporting data total
	unique_months <- unique(this_chw$month_num)
	nmonths_reporting <- length(unique_months)
	
	#find the number of months with at least one outlying value
	months_with_overs <- this_chw[this_chw$is_over == TRUE,]	
	unique_overs <- unique(months_with_overs$month_num)
	
	#store number of unique months over as over_months
	over_months <- length(unique_overs)
	
	#find the "ratio" for the months - the number of months with an outlier / number of months reported by that CHW, or % of months > threshold
	over_ratio <- length(over_months) / nmonths_reporting	
	
	print(paste("over_months: ", over_months, "over_ratio: ", over_ratio, "nmonths reporting: ", nmonths_reporting, sep=" "))
	
	# #find the number of times a CHW's questions are over the threshold across all months
	over_count <- sum(this_chw$is_over)
	 
	# #send these scores to compute_months to determine for how many months that CHW has an outlier score
	# over_months <- compute_months_over(this_chw)
	
	one_row <- cbind(c, over_count, over_months, over_ratio, nmonths_reporting)
	
	over_df <- rbind(over_df, one_row)
}

#print(over_df)

plot_df(over_df)
	
output_csv <- paste(output_dir, "find_outliers/over_per_month.csv", sep="")
write.table(over_df, output_csv)	

dev.off()
  
