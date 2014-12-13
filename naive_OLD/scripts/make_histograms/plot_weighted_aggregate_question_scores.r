#normalize aggregate question scores by number of questions answered

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
input_file <- paste(output_dir, "questions-chws-dates-all.csv", sep="")

plot_questions <- function(one_question){

	##compute mean and sd, at to each plot
	#mean_score <- mean(one_question$norm_scores, na.rm=T)
	#sd_score <- sd(one_question$norm_scores, na.rm=T)
	#
	#print(paste(mean_score, sd_score))
	#
	#one_sd <- mean_score + sd_score
	#two_sd <- mean_score + sd_score*2
	
	title <- one_question$q[1]

	plot <- ggplot(data=one_question, aes(norm_scores), environment = environment()) + geom_histogram(binwidth=0.01) + ggtitle(title) +
			coord_cartesian(xlim = c(0, 1)) + coord_cartesian(ylim = c(0, 50)) 
			# + theme(axis.text.x = element_text(angle = 90, hjust = 1))			
			#geom_vline(aes(xintercept = mean_score, colour="blue")) + geom_vline(aes(xintercept = one_sd, colour="red")) +
			#geom_vline(aes(xintercept = two_sd, color="red", linetype="dash"))
	
	print(plot)	
}

#####
# Start 
#####

data <- read.csv(input_file, head=T, sep="")

#look only at questions with score values greater than 0 (actually completed) and not NA
data[data == 0] <- NA
data <- na.omit(data)

norm_df <- data.frame("question" = character(0), "norm_scores" = numeric(0))

pdf(output_pdf <- paste(output_dir, "normalized_question_scores.pdf", sep=""), height=12, width=12, onefile=TRUE)

questions <- unique(data$question)
#questions <- c("form.bp1.anc1.anc1_abdominal_exam")

print(questions)

for (q in questions){
	one_question <- data[data$question == q,]
	
	#normalize scores - divide each question score by the sum of scores for that question
	total <- sum(one_question$score)
	norm_scores <- one_question$score / total
	
	#add normalized scores to norm_df
	one_question <- cbind(q, norm_scores)
	norm_df <- rbind(norm_df, one_question)
			
	one_question <- as.data.frame(one_question)
	
	print(one_question)
	
	#send data for one question to be plotted
	plot_questions(one_question)
}

output_csv <- paste(output_dir, "normalized_question_scores.csv")
write.table(norm_df, output_csv)

dev.off()

