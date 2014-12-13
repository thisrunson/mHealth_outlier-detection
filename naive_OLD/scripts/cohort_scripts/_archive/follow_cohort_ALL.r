#identify the top n outliers from the first x weeks' data
#follow these outliers to see how their rank among outliers changes over the collection period

#compare outliers per week (and month, etc.)
#compare top outlier across time frames and see if they're the same/where their ranking falls

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/"
input_dir <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/sliding_window-weeks"

input_all_file <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/all_scores_data/sorted_scores_all_collection.csv"

####
# Variables to be set
####

#TODO: set n to be the size of cohort we want to follow
n <- 10

#TODO: set output method to be "points" or "line"
method <- "line"

#plot a time series of all means and SDs per week across the collection period
plot_ranks <- function(data, min, max, init_rank){

	#draw a line across where the outlier threshold is
	hline <- n
	
	plot_title <- paste(data$chw, " - Initial Rank: ", init_rank, sep="")
	
	#compute a ribbon for mean +/- SD
	#error_line <- aes(ymax = (data$mean + data$sd), ymin = (data$mean - data$sd))
		
	if (method == "line"){
		# NOTE: added environment=environment() because of https://github.com/hadley/ggplot2/issues/743
		return(ggplot(data=data, aes(week), environment = environment()) + geom_line(aes(y=rank, colour = rank)) + geom_hline(aes(yintercept=hline)) + ylim(min, max) + ggtitle(plot_title) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) )
	}
	
	if (method == "points"){
		return(ggplot(data=data, aes(week), environment = environment()) + geom_point(aes(y=rank, colour = rank)) + geom_hline(aes(yintercept=hline)) + ylim(min, max) + ggtitle(plot_title) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) )
	}

}

pull_top_outliers <- function(){	
	#subset the top n outlier for all the data to make a cohort
	cohort <- read.csv(input_all_file, head=F, sep="", skip=1)
	
	rank <- c(1:nrow(cohort))
	
	cohort <- cbind(cohort, rank)
	
	colnames(cohort) <- c("chw", "score", "rank")
		
	top_n <- cohort[cohort$rank <= n,]
	top_chws <- as.character(top_n$chw)
		
	return(top_chws)
}

#find the rank of each member of the cohort to follow across weeks
follow_cohort <- function(cohort_df){
	
	top_chws <- pull_top_outliers()
	
	print(top_chws)
	
	min <- 1
	max <- 10
	
	i <- 1
	
	for (this_chw in top_chws){
			
		chw_ranks <- cohort_df[cohort_df$chw == this_chw,]
		
		if(max(chw_ranks$rank) > max){
			max <- max(chw_ranks$rank)
		}
				
		initial_rank <- i
			
		print(plot_ranks(chw_ranks, min, max, initial_rank))
		
		i <- i + 1
		}		
}


#####
# Start here
#####
#loop through a file with all output weeks data and read into variables
setwd(input_dir)

cohort_df <- data.frame(week = character(0), chw = character(0), score = numeric(0), rank = numeric(0))

i <- 1

#go through the first x scores output files, pull out the top n CHWs in these, and then find these same CHWs ranks for subsequent weeks
for (file_name in list.files()) {
	one_week <- read.csv(file_name, head=F, sep="", skip=1)
	
	rank <- c(1:nrow(one_week))
	
	one_week <- cbind(i, one_week, rank)
	cohort_df <- rbind(cohort_df, one_week)

	i <- i + 1
}
	
#print(cohort_df)

colnames(cohort_df) <- c("week", "chw", "score", "rank")
	
pdf(output_pdf <- paste(output_dir, "top_outliers-ALL", "_N=", n, "_", method, ".pdf", sep=""), height=16, width=8, onefile=TRUE)
	
output_csv <- paste(output_dir, "top_outliers-ALL", "_N=", n, ".csv", sep="")
write.table(cohort_df, output_csv)	
	
#call follow_cohort function to find ranks across time for top n outliers in first weeks
follow_cohort(cohort_df)

dev.off()
