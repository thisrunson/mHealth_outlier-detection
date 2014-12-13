#################
## IMPORTANT
#################
# You must set the WD before running the script. E.g.:
#
# setwd("/Users/bderenzi/CSEDropbox/Dropbox/Outlier Detection/")
# source('/Users/bderenzi/CSEDropbox/Dropbox/Outlier Detection/scripts/MMA/plot_histograms_per_question.r')

#plot histograms of MMA scores for each week/month
rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	
require(yaml)
	
#TODO: choose "weeks" or "months"
window <- "weeks"

#####
# Directories
#####
# my = yaml.load_file(paste(getwd(),"/settings/settings.yaml",sep=""))
# output_dir <- paste(my$settings$od_root, my$settings$output_dir,sep="")
# input_dir <- paste(output_dir,"scores_questions-", window, "/", sep="")

output_dir <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/"
input_file <- paste(output_dir, "question-scores.csv", sep="")

plot_questions <- function(one_question, maximum){

	#compute mean and sd, at to each plot
	mean_score <- mean(one_question$score, na.rm=TRUE)
	sd_score <- sd(one_question$score, na.rm=TRUE)
	
	one_sd <- mean_score + sd_score
	two_sd <- mean_score + sd_score*2

	plot <- ggplot(data=one_question, aes(score), environment = environment()) + geom_histogram() + 
	coord_cartesian(xlim = c(1, maximum), ylim = c(1,15)) + ggtitle(one_question$question) +
	geom_vline(aes(xintercept = mean_score, colour="blue")) + geom_vline(aes(xintercept = one_sd, colour="red")) +
	geom_vline(aes(xintercept = two_sd, color="red", linetype="dash"))	
	
	print(plot)	
}

#####
# Start 
#####

data <- read.csv(input_file, head=T, sep=" ")

pdf(output_pdf <- paste(output_dir, "aggregate_question_scores.pdf", sep=""), height=12, width=12, onefile=TRUE)

questions <- unique(data$question)

maximum <- max(data$score)
print(maximum)

for (q in questions){
	one_question <- data[data$question == q,]
	
	#send data for one question to be plotted
	plot_questions(one_question, maximum)
}

output_csv <- paste(output_dir, "aggregate_question_scores.csv")
write.table(questions, output_csv)

dev.off()

