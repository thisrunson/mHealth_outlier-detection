##Add a column name for start_date with the proper formatting, pull unique CHW names from file, make a subset of data for each CHW, and plot a histogram of the number of reports collected each day for each CHW

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/"
input_dir <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/"


load_data <- function(file) {
	CHW_file <- read.csv(file, sep=" ")
	
	return (CHW_file)
}

sort_and_plot <- function(scores, window) {
	#sort outliers by score, highest to lowest
	sorted_scores <- scores[order(-scores$score),]
	
	print(window)
	
	output_csv <- paste(output_dir, "sorted_scores_", window, ".csv", sep="")

	write.table(sorted_scores, output_csv, row.names=F)
	plot(sorted_scores$score, main=window, ylab="score", xlab="CHWs - all questions")
}

##########
# Start
##########

##Run months, block of months (July - May)

##go through scores file
#	aggregate scores per CHW
# 	plot these
#	do this for week, month, all data
#	compare the outliers for each of these

time_windows <- c("first_week", "first_month", "all_collection")

pdf(paste(output_dir, "sorted_scores_ALL", ".pdf", sep=""), height=16, width=8, onefile=TRUE)

for (window in time_windows){
	
	input_file <- paste(input_dir, "scores_output-", window, ".csv", sep="")
	
	scores_file <- load_data(input_file)

	#test to see if the scores are "Inf", and replace these with 0
	if("Inf" %in% scores_file$score){
		scores_file$score[scores_file$score == "Inf"] <- 0
	}

	summed_scores <- aggregate(score ~ chw, data = scores_file, FUN = sum)

	#call function to sort and plot scores
	sort_and_plot(summed_scores, window)

}

dev.off()


