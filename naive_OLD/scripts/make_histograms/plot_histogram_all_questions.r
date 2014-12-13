#plot histograms of MMA scores for each week/month

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

#TODO: choose "weeks" or "months"
window <- "weeks"

upper_x <- 15

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/"
input_dir <- paste("/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/scores_questions-", window, sep="")

plot_all <- function(df){
	
	# remove questions where score = 0
	df <- df[df$score != 0,]
 	
	min_score <- min(df$score, na.rm=TRUE)
	max_score <- max(df$score, na.rm=TRUE)
	mean_score <- mean(df$score, na.rm=TRUE)
	sd_score <- sd(df$score, na.rm=TRUE)
	
	one_sd <- mean_score + sd_score
	two_sd <- mean_score + sd_score*2
	
	print(paste("mean: ", mean_score, "min: ", min_score, "max: ", max_score))
	
	title <- paste("Scores per CHW per Question per Week - limits = ", upper_x)
	
	#plot all data
	#plot <- ggplot(data=df, aes(score), environment = environment()) + geom_histogram() + ggtitle("Scores per CHW per Question per Week") + geom_vline(aes(xintercept = mean_score, colour="blue")) + geom_vline(aes(xintercept = mean_score + sd_score, colour="red")) + geom_vline(aes(xintercept = mean_score - sd_score, colour="red"))	
	
	#plot data with bins of .5, xlims = (0, upper_x) - only 1 score over 15, only 21 over 5
	plot <- ggplot(data=df, aes(score), environment = environment()) + geom_histogram() + ggtitle(title) + xlim(0, upper_x) + geom_vline(aes(xintercept = mean_score, colour="blue")) + geom_vline(aes(xintercept = one_sd, colour="red")) + geom_vline(aes(xintercept = two_sd, color="red", linetype="dash"))

	print(plot)

}

####
#Start
####
#loop through a file with all output weeks data and plot scores
setwd(input_dir)

#uncomment (and change for loop) to run on first two csvs only
#list <- c("scores_output-questions_by_week-2012-08-06.csv", "scores_output-questions_by_week-2012-08-13.csv")
list <- list.files()

#make a dataframe to hold all the question scores per week
questions_df <- data.frame(date = character(0), chw = character(0), question = character(0), score = numeric(0))

#go through week question-score files, add to the questions_df dataframe, send this to be plotted
for (file in list) {
	
	window_data <- read.csv(file, head=T, sep="")
		
	date <- substr(file, 33, 42)
		
	window_data <- cbind(date, window_data)
	
	questions_df <- rbind(questions_df, window_data)	
}

pdf(output_pdf <- paste(output_dir, "histograms-questions-all-", window, "-xlim=", upper_x, ".pdf", sep=""), height=12, width=12, onefile=TRUE)
output_csv <- paste(output_dir, "questions-chws-weeks-all-", window, ".csv", sep="")

write.table(questions_df, output_csv)	

plot_all(questions_df)

dev.off()

