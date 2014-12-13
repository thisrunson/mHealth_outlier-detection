##Add a column name for start_date with the proper formatting, pull unique CHW names from file, make a subset of data for each CHW, and plot a histogram of the number of reports collected each day for each CHW

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/naive/output_data/"
input_file <- paste(output_dir, "all_scores_data/scores_output-all_collection-three_removed.csv", sep="")


sort_and_plot <- function(scores) {
	#sort outliers by score, highest to lowest
	sorted_scores <- scores[order(-scores$score),]
	
	print(sorted_scores)
	
	output_csv <- paste(output_dir, "sorted_scores-all_collection.csv", sep="")

	write.table(sorted_scores, output_csv, row.names=F)
	plot(sorted_scores$score, main="Distribution of ranked scores - one per CHW", ylab="score", xlab="CHWs - all questions")
}

##########
# Start
##########

scores_file <- read.csv(input_file, sep="")

summed_scores <- aggregate(score ~ chw, data = scores_file, FUN = sum)

pdf(paste(output_dir, "sorted_scores-all_collection", ".pdf", sep=""), height=16, width=8, onefile=TRUE)

#call function to sort and plot scores
sort_and_plot(summed_scores)

dev.off()


