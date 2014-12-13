# Compare outliers found via "aggregate" method - computing a ratio of each CHW's questions > threshold to all questions answered by her per week - and the
# "total" method - computing the ratio on data computed by running MMA on all data for all CHWs at once

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/pvalues/output_data/find_outliers/"

input_aggregate <- paste(output_dir, "top_outliers-questions_weeks_over.csv", sep="")
input_total <- paste(output_dir, "top_outliers-total_questions_over.csv", sep="")

plot_outliers <- function(outlier_df){
	
	pdf(output_pdf <- paste(output_dir, "aggregate_vs_total-outlier_scores", ".pdf", sep=""), height=12, width=12, onefile=TRUE)

	#plot a histogram of the CHW count scores
	plot <- ggplot(outlier_df, aes(x=aggregate_count, y=total_count)) + geom_point() + ggtitle("Question/week outliers vs. total outliers - Counts") +
			xlab("Aggregate Count") + ylab("Total Count") #+ geom_abline(slope=correlation)

	print(plot)	
	
	#plot a histogram of the CHW ratio scores
	plot <- ggplot(outlier_df, aes(x=aggregate_percent, y=total_percent)) + geom_point() + ggtitle("Question/week outliers vs. total outliers - Percents") +
			xlab("Aggregate Percent") + ylab("Total Percent") #+ geom_abline(slope=correlation)


	print(plot)	
	
}


#####
# Start here
#####
aggregate_df <- read.csv(input_aggregate, sep="")
total_df <- read.csv(input_total, sep="")

outliers_df<- data.frame(chw = character(0), aggregate_count = numeric(0), aggregate_percent = numeric(0), 
				total_count = numeric(0), total_percent = numeric(0))

unique_chws <- unique(aggregate_df$c)

print(aggregate_df)

#for each CHW in aggregate_df, find score counts for the same CHW in total_df, put these into one df
for (c in unique_chws){
	
	total_chw <- total_df[total_df$c == c,]
	
	# print(c)
	# print(aggregate_df[aggregate_df$c == c,]$c)
	# print(aggregate_df[aggregate_df$c == c,]$over_count) 
	# print(aggregate_df[aggregate_df$c == c,]$over_ratio)
	# print(total_df[total_df$c == c,]$over_count) 
	# print(total_df[total_df$c == c,]$over_ratio)
	
	one_row <- cbind(aggregate_df[aggregate_df$c == c,], total_df[total_df$c == c,])
		
	#one_row <- cbind(c, aggregate_df[aggregate_df$c == c,], aggregate_df[aggregate_df$c == c,]$over_count, aggregate_df[aggregate_df$c == c,]$over_ratio,
	#		 		total_chw$over_count, total_chw$over_ratio)
	
	print(one_row)
	colnames(one_row) <- c("chw", "aggregate_count", "aggregate_percent", "agg_weeks", "chw2", "total_count", "total_percent", "tot_weeks")
	
	print(one_row)
	one_row <- subset(one_row, select=c(chw, aggregate_count, aggregate_percent, total_count, total_percent))
	
	print(one_row)
	outliers_df <- rbind(outliers_df, one_row)
}

colnames(outliers_df) <- c("chw", "aggregate_count", "aggregate_percent", "total_count", "total_percent")
	
print("Counts correlation:")
print(cor.test(outliers_df$aggregate_count, outliers_df$total_count))

print("Percents correlation:")
print(cor.test(outliers_df$aggregate_percent, outliers_df$total_percent))

plot_outliers(outliers_df)
	
output_csv <- paste(output_dir, "aggregate_vs_total.csv", sep="")
write.table(outliers_df, output_csv)	

dev.off()
  
