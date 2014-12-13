###This is plotting the means differently each week - this needs to be fixed, but I'm putting it off for right now. The means are computed just fine - the problem comes in taking the subset of the data. Need to take a subset of the CHWs and put them in a df with NAs for the weeks when there is no data for that CHW.

#identify the top n outliers from all weeks' data
#follow these outliers to see how their rank among outliers changes over the collection period

#compare outliers per week (and month, etc.)
#compare top outlier across time frames and see if they're the same/where their ranking falls

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

####
# Variables to set
####

#TODO: set n to be the size of cohort we want to follow
n <- 10

#TODO: set output method to be "points" or "line"
method <- "points"

#TODO: analyzing "weeks" or "months"?
window <- "months"


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier Detection/pvalues/output_data/"
input_dir <- paste(output_dir, "sliding_window-", window, "/", sep="")

input_all_file <- paste(output_dir, "sorted_scores-all_collection.csv", sep="")


#plot a time series of all means and SDs per week across the collection period
plot_rank <- function(one_chw, num_weeks){
		
	# one_chw <- na.omit(one_chw)
	print(one_chw)	
	
	if(method == "line"){
		way_to_print <- geom_line(aes(y=score, colour = one_chw$score))
	}
	
	if(method == "points"){
		way_to_print <- geom_point(aes(y=score, colour = one_chw$score))
	}
		
	plot_title <- paste(one_chw$chw, "Initial Rank: ", one_chw$total_rank, sep="")
	
	xmin <- 1
	xmax <- num_weeks
	
	#print(paste(data[,window], data$week_mean))
	
	#compute a ribbon for mean +/- SD
	error_ribbon <- aes(ymax = (one_chw$week_mean + one_chw$week_sd), ymin = (one_chw$week_mean - one_chw$week_sd), alpha=0.2)
	
	plot <- ggplot(data=one_chw, aes(x=one_chw[,window]), environment = environment()) + way_to_print + geom_line(aes(y=one_chw$week_mean)) +
				geom_ribbon(error_ribbon) + xlab(window) + ylab("score") + coord_cartesian(ylim = c(0, 500)) +
				ggtitle(plot_title) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
	
	return(plot)

}

pull_top_outliers <- function(){	
	#subset the top n outlier for all the data to make a cohort
	cohort <- read.csv(input_all_file, head=F, sep="", skip=1)
		
	rank <- c(1:nrow(cohort))
	
	cohort <- cbind(cohort, rank)
	
	colnames(cohort) <- c("chw", "score", "rank")
		
	top_n <- cohort[cohort$rank <= n,]	
		
	return(top_n)
}

#find the rank of each member of the cohort to follow across weeks
follow_cohort <- function(cohort_df, num_weeks){
		
	top_chw <- pull_top_outliers()
	
	top_names <- top_chw$chw
	
	# print("top chws")
	# print(top_chws)
	
	#compute mean and SD across all scores
	all_mean <- mean(cohort_df$score, na.rm=T)
	all_sd <- sd(cohort_df$score, na.rm=T)
	
	# minimum <- 1
	# maximum <- 10

	i <- 1
			
	for (c in top_names){
			
		one_chw <- cohort_df[cohort_df$chw == c,]
						
		# if(max(chw_ranks$rank, na.rm=T) > maximum){
		# 	maximum <- max(chw_ranks$rank, na.rm=T)
		# }
		
		print("top chw")
		print(top_chw)
		
		top_chw$chw = as.character(top_chw$chw)
		#top_chw$chw = as.numeric(top_chw$rank)
		
		print(class(top_chw$rank))
		print(class(one_chw))
		print(class(c))
		
		print("one chw")
		print(one_chw)
				
		total_rank <- top_chw[top_chw$chw == c,]$rank
		
		print("rank")
		print(total_rank)
	
		one_chw <- cbind(one_chw, total_rank)
					
		print(plot_rank(one_chw, num_weeks))
		
		i <- i + 1
	}	
}


#####
# Start here
#####
#loop through a file with all output weeks data and read into variables
setwd(input_dir)

cohort_df <- data.frame(week = character(0), chw = character(0), score = numeric(0), rank = numeric(0), week_mean = numeric(0), week_sd = numeric(0))

i <- 1

#go through the first x scores output files, pull out the top n CHWs in these, and then find these same CHWs' ranks for subsequent weeks
for (file_name in list.files()) {
	one_week <- read.csv(file_name, head=F, sep="", skip=1)
		
	rank <- c(1:nrow(one_week))
	
	colnames(one_week) <- c("chw", "score")
	
	week_mean <- mean(one_week$score) 
	week_sd <- sd(one_week$score)
	
	#add row to cohort_df	
	one_week <- cbind(i, one_week, rank, week_mean, week_sd)		
	cohort_df <- rbind(cohort_df, one_week)

	i <- i + 1
}

num_weeks <- i

colnames(cohort_df) <- c(window, "chw", "score", "rank", "week_mean", "week_sd")
	
pdf(output_pdf <- paste(output_dir, "top_outliers-all-scores-ylim=500", "_N=", n, "_", method, "_", window, ".pdf", sep=""), height=12, width=12, onefile=TRUE)
	
# output_csv <- paste(output_dir, "top_outliers-all-scores", "_N=", n, "_", window, ".csv", sep="")
# write.table(cohort_df, output_csv)	
	
#call follow_cohort function to find ranks across time for top n outliers in first weeks
follow_cohort(cohort_df, num_weeks)

dev.off()
