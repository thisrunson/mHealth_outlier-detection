#plot histograms of MMA scores for each week/month

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#TODO: choose "weeks" or "months"
window <- "weeks"

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/"
input_dir <- paste("/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/sliding_window-", window, sep="")

pdf(paste(output_dir, "hists_MMA_scores-", window, ".pdf", sep=""), height=16, width=8, onefile=TRUE)

#loop through a file with all output weeks data and plot scores
setwd(input_dir)

min_score <- 0
max_score <- 0

#find xlims
for (file in list.files()) {
	window_data <- read.csv(file, head=T, sep="")
	
	#find the highest score to set as xlim
	if (max(window_data$score) > max_score) {
		max_score <- max(window_data$score)
	}
}

#plot a histogram for the scores each week
for (file in list.files()) {
	window_data <- read.csv(file, head=T, sep="")
			
	plot <- ggplot(data=window_data, aes(score), environment = environment(), colour=score) + geom_histogram() + 
	scale_x_continuous(limits = c(min_score, max_score))+ ggtitle(file)	
	
	print(plot)
	
	#hist(window_data$score, xlab="score", ylab="freq", main=file)
}

dev.off()

