#compare outliers per week (and month, etc.)
#compare top outlier across time frames and see if they're the same/where their ranking falls

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	


#plot a time series of all means and SDs per week across the collection period
plot_means <- function(data){
	#hard coded range for scores - fix later if using other data
	min <- 0
	max <- 200
	
	#compute a ribbon for mean +/- SD
	error_line <- aes(ymax = (data$mean + data$sd), ymin = (data$mean - data$sd))
	
	# NOTE: added environment=environment() because of https://github.com/hadley/ggplot2/issues/743
	return(ggplot(data=data, aes(week), environment = environment()) + geom_point(aes(y=mean, colour = mean)) + geom_errorbar(error_line, alpha=0.5, colour="red") + ylim(min, max) + ggtitle("Scores - means and SDs - per week") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) )
}


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/"
input_dir <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/sliding_window-weeks"

#loop through a file with all output weeks data and read into variables
setwd(input_dir)
df <- data.frame(week = character(0), chw = character(0), score = numeric(0))

for (file_name in list.files()) {
	file <- read.csv(file_name, head=F, sep="", skip=1)
	
	mini_df <- cbind(file_name, file)
	
	df <- rbind(df, mini_df)
}

colnames(df) <- c("week", "chw", "score")

unique_weeks <- unique(df$week)

# look at first two weeks, pull out cohort of top n outliers, see what happens to them for month and all data
#	- can plot these with color to stand out among others?
#	- or rank them - see where their rank falls later

outlier_df <- data.frame(week = character(0), chw = character(0), score = numeric(0))

#set n, the number of top entries/week we want to see
n = 10

#find top outlier per week, combine these into a data frame
for (one_week in unique_weeks){
	this_week <- subset(df, df$week == one_week)
	
	#week data is already sorted - take top n entries from each week to find top n outliers
	top_outliers <- this_week[1:n,]
	
	outlier_df <- rbind(outlier_df, top_outliers)
}

#find duplicate CHWs from among the outlier_df
duplicates <- duplicated(outlier_df$chw)

#find frequency counts for number of times each outlier appeared in top n outliers for each week
outliers_only <- as.data.frame(table(outlier_df[duplicates,]$chw))

colnames(outliers_only) <- c("chw", "freq")

outliers_only <- subset(outliers_only, freq>0)

#sort them in decreasing order by freq
outliers_only <- outliers_only[order(-outliers_only$freq),]

print(outliers_only)

output_csv <- paste(output_dir, "top_N_outliers_", "N=", n, ".csv", sep="")

write.table(outliers_only, output_csv)

#print(plot_means(summaries))

