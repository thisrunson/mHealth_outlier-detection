#compare CHWs per week (and month, etc.)
#compute summary stats of these - mean and SD
#plot the summary stats / week on a time series across the collection period

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/output_data/"
input_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/output_data/sliding_window-months/"


#compute the mean and SD for the scores data
compute_summary_stats <- function(unique_weeks){
	summaries_df <- data.frame(weekcol = character(0), meancol = numeric(0), sdcol = numeric(0))
	
	for (one_week in unique_weeks){
		this_week <- subset(df, df$week == one_week)
		
		mean_score <- mean(this_week$score)
		sd_score <- sd(this_week$score)
		
		this_week <- substr(one_week, 20, 29)
		week_row <- cbind(this_week, mean_score, sd_score)
			
		summaries_df <- rbind(summaries_df, week_row)
	}
	
	colnames(summaries_df) <- c("week", "mean", "sd")
	
	summaries_df$mean <- as.numeric(levels(summaries_df$mean))
	summaries_df$sd <- as.numeric(levels(summaries_df$sd))
	
	return(summaries_df)
}

#plot a time series of all means and SDs per week across the collection period
plot_means <- function(data){
	#hard coded range for scores - fix later if using other data
	min <- 0
	max <- 200
	
	#compute a ribbon for mean +/- SD
	error_line <- aes(ymax = (data$mean + data$sd), ymin = (data$mean - data$sd))
	
	# NOTE: added environment=environment() because of https://github.com/hadley/ggplot2/issues/743
	return(ggplot(data=data, aes(week), environment = environment()) + geom_point(aes(y=mean, colour = mean)) + 
			geom_errorbar(error_line, alpha=0.5, colour="red") + ylim(min, max) + ggtitle("Scores - means and SDs - per month") + 
			theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("months") )
}

#####
# Start
#####

#loop through a file with all output weeks data and read into variables
setwd(input_dir)

df <- data.frame(week = character(0), chw = character(0), score = numeric(0))

for (file in list.files()) {
	a <- read.csv(file, head=F, sep="", skip=1)
	
	mini_df <- cbind(file, a)
	
	df <- rbind(df, mini_df)
}

colnames(df) <- c("week", "chw", "score")

unique_week_data <- unique(df$week)

summaries <- compute_summary_stats(unique_week_data)
print(summaries)

output_csv <- paste(output_dir, "means_SDs-months", ".csv", sep="")
pdf(output_pdf <- paste(output_dir, "means_SDs-months", ".pdf", sep=""))

write.table(summaries, output_csv)

print(plot_means(summaries))

dev.off()

