# Make one plot per question, using data weekly scores computed per CHW per question (so each hist contains num_CHWs * num_weeks points)

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


#####
# Directories
#####
# my = yaml.load_file(paste(getwd(),"/settings/settings.yaml",sep=""))
# output_dir <- paste(my$settings$od_root, my$settings$output_dir,sep="")
# input_dir <- paste(output_dir,"scores_questions-", window, "/", sep="")

output_dir <- "/Users/tedmccarthy/Dropbox/Outlier Detection/output_data/"
input_file <- paste(output_dir, "questions-chws-weeks-all.csv", sep="")

plot_questions <- function(one_question){

	#compute mean and sd, at to each plot
	mean_score <- mean(one_question$score, na.rm=TRUE)
	sd_score <- sd(one_question$score, na.rm=TRUE)
	
	one_sd <- mean_score + sd_score
	two_sd <- mean_score + sd_score*2

	title <- (one_question$question[1])

	#plot data
	plot <- ggplot(data=one_question, aes(score), environment = environment()) + geom_histogram(binwidth=0.1) + ggtitle(title) +
	geom_vline(aes(xintercept = mean_score), colour="blue") + geom_vline(aes(xintercept = one_sd), colour="red") + 
	geom_vline(aes(xintercept = two_sd), color="red", linetype="longdash") #	+ coord_cartesian(xlim = c(0, 30), ylim = c(0, 2500)) 
	
	print(plot)	
}

#####
# Start 
#####

data <- read.csv(input_file, head=T, sep=" ")

pdf(output_pdf <- paste(output_dir, "MMA_values-score-week-question", ".pdf", sep=""), height=12, width=12, onefile=TRUE)

questions <- unique(data$question)

#i <- 1
for (q in questions){
	one_question <- data[data$question == q,]
	
	print(one_question)
		
	#send data for one question to be plotted - only plot if all scores for question are not NA
	if(!is.na(one_question$score)){
		plot_questions(one_question)
	}
	 
#	 i <- i + 1
#	 if (i > 10) break
}

output_csv <- paste(output_dir, "MMA_values-score-week-question.csv")
write.table(questions, output_csv)

dev.off()

