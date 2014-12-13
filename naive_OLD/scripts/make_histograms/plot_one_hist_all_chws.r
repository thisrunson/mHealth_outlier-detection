#plot histogram - one score per CHW (take in all data) - scores summed across each CHW's questions

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#upper_x <- 15

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier Detection/output_data/MMA_score_histograms/"

input_file <- "/Users/tedmccarthy/Dropbox/Outlier Detection/output_data/all_scores_data/scores_output-all_collection-three_removed.csv"

plot_all <- function(df){
	#compute min, max, and summary stats
	# min_score <- min(df$score, na.rm=TRUE)
	# max_score <- max(df$score, na.rm=TRUE)
	
	df$score <- as.numeric(df$score)
	
	mean_score <- as.numeric(mean(df$score, na.rm=TRUE))
	sd_score <- sd(df$score, na.rm=TRUE)
	
	print(mean_score)
	
	one_sd <- mean_score + sd_score
	two_sd <- mean_score + sd_score*2
		
	title <- "MMA Scores - All CHWs, scores for each summed, using 'total' data "
	
	#plot data
	plot <- ggplot(data=df, aes(score), environment = environment()) + geom_histogram() + ggtitle(title) +
	geom_vline(aes(xintercept = mean_score), colour="blue") + geom_vline(aes(xintercept = one_sd), colour="red") + 
	geom_vline(aes(xintercept = two_sd), color="red", linetype="longdash") + xlab("question score")

	print(plot)

}

####
#Start
####
#open all_data file
df <- read.csv(input_file, sep="")

pdf(output_pdf <- paste(output_dir, "CHWs-one_histogram-all_CHWs.pdf", sep=""), height=12, width=12, onefile=TRUE)

chw_df <- data.frame(chw = character(0), score = numeric(0))

chws <- unique(df$chw)

for (c in chws){
	one_chw <- df[df$chw == c,]
	
	#sum all the scores for the one CHW
	cum_score <- sum(one_chw$score, na.rm=T)
	
	#add new score to the chw_df
	row <- cbind(c, cum_score)	
	chw_df <- rbind(chw_df, row)
}

colnames(chw_df) <- c("chw", "score")

#get around tricky time converting score (a factor) to numeric, then sort the values
chw_df$score <- as.numeric(levels(chw_df$score))[chw_df$score]
sorted_df <- chw_df[with(chw_df, order(score)),]
print(sorted_df)

plot_all(chw_df)

dev.off()

