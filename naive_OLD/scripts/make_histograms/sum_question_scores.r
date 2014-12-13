#sums all week scores for each question and puts together a dataframe

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

output_dir <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/"
input_file <- paste(output_dir, "questions_with_weeks.csv", sep="")

#####
# Start 
#####

data <- read.csv(input_file, head=T, sep=" ")

question_df <- data.frame(week = numeric(0), question = character(0), summed_score = numeric(0))

questions <- unique(data$question)

for (q in questions){
	#subset all data by question
	one_question <- data[data$question == q,]
	
	weeks <- unique(one_question$week_num)
	
	for (w in weeks){
		#subset question data by week
		one_week <- one_question[one_question$week_num == w,]
		
		score <- sum(one_week$score, na.rm=T)
		
		#append score to question_df
		row <- cbind(w, q, score)			
		question_df <- rbind(question_df, row)	
	}
}

colnames(question_df) <- c("week", "question", "score")

output_csv <- paste(output_dir, "question-scores.csv")
write.table(question_df, output_csv)

