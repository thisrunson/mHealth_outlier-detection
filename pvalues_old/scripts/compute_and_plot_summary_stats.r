#compare CHWs per week (and month, etc.)
#compute summary stats of these - mean and SD
#plot the summary stats / week on a time series across the collection period

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#TODO: choose "months" or "weeks" for data
window <- "months"

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/pvalues/output_data/"
input_dir <- paste(output_dir, "sliding_window-", window, "/", sep="")


#compute the mean and SD for the scores data
compute_summary_stats <- function(unique_weeks){
	summaries_df <- data.frame(weekcol = character(0), meancol = numeric(0), sdcol = numeric(0))
	
	for (one_week in unique_weeks){
		this_week <- subset(df, df[,window] == one_week)
		
		mean_score <- mean(this_week$score)
		sd_score <- sd(this_week$score)
		
		this_week <- substr(one_week, 22, 31)
		week_row <- cbind(this_week, mean_score, sd_score)
			
		summaries_df <- rbind(summaries_df, week_row)
	}
	
	colnames(summaries_df) <- c(window, "mean", "sd")
	
	summaries_df$mean <- as.numeric(levels(summaries_df$mean))
	summaries_df$sd <- as.numeric(levels(summaries_df$sd))
	
	return(summaries_df)
}


#plot a time series of all means and SDs per week across the collection period
plot_means <- function(data){
	#hard coded range for scores - fix later if using other data
	min <- 0
	max <- 3
	
	#compute a ribbon for mean +/- SD
	error_line <- aes(ymax = (data$mean + data$sd), ymin = (data$mean - data$sd))
	
	# NOTE: added environment=environment() because of https://github.com/hadley/ggplot2/issues/743
	return(ggplot(data=data, aes(data[,window]), environment = environment()) + geom_point(aes(y=mean, colour = mean)) + 
			geom_errorbar(error_line, alpha=0.5, colour="red") + ggtitle(paste("Scores - means and SDs - per ", window)) + #ylim(min, max) 
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab(window) )
}

#####
# Start
#####

#loop through a file with all output weeks data and read into variables
setwd(input_dir)

df <- data.frame(week = character(0), chw = character(0), score = numeric(0))

for (file in list.files()) {
	a <- read.csv(file, head=F, sep="", skip=1)
	
	#break if file contains NA values
	if(is.na(a)) break
	
	mini_df <- cbind(file, a)
	
	df <- rbind(df, mini_df)
}

colnames(df) <- c(window, "chw", "score")

unique_week_data <- unique(df[,window])

summaries <- compute_summary_stats(unique_week_data)
print(summaries)

pdf(output_pdf <- paste(output_dir, "means_and_SDs/means_SDs-", window, ".pdf", sep=""))

# output_csv <- paste(output_dir, "means_SDs-", window, ".csv", sep="")
# write.table(summaries, output_csv)

print(plot_means(summaries))

dev.off()

