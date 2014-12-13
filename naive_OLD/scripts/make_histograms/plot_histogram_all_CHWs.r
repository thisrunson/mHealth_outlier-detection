#plot histogram - one score per CHW (take in all data)

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
	min_score <- min(df$score, na.rm=TRUE)
	max_score <- max(df$score, na.rm=TRUE)
	
	mean_score <- mean(df$score, na.rm=TRUE)
	sd_score <- sd(df$score, na.rm=TRUE)
	
	one_sd <- mean_score + sd_score
	two_sd <- mean_score + sd_score*2
		
	title <- paste("MMA Scores - One CHW, all questions, all weeks, using 'total' data - ", df$chw)
	
	#plot data
	plot <- ggplot(data=df, aes(score), environment = environment()) + geom_histogram(binwidth=1, aes(fill=..count..))
	
	plot + ggtitle(title) + xlim(min_score, max_score) + scale_y_continuous(expand = c(0,0), limits=c(0,max(ggplot_build(plot)$df[[1]]$count)*1.1)) +
	geom_vline(aes(xintercept = mean_score, colour="blue")) + geom_vline(aes(xintercept = one_sd, colour="red")) + 
	geom_vline(aes(xintercept = two_sd, color="red", linetype="dash")) + xlab("question score")

	print(plot)

}

####
#Start
####
#open all_data file
df <- read.csv(input_file, sep="")

pdf(output_pdf <- paste(output_dir, "CHWs-one_histogram_per_CHW", ".pdf", sep=""), height=12, width=12, onefile=TRUE)

chws <- unique(df$chw)

for (c in chws){
		
	one_chw <- df[df$chw == c,]
	print(one_chw)
	
	#plot each subset of chw data
	plot_all(one_chw)
}

dev.off()

