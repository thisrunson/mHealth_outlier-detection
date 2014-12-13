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
my = yaml.load_file(paste(getwd(),"/settings/settings.yaml",sep=""))
output_dir <- paste(my$settings$od_root, my$settings$output_dir,sep="")
input_dir <- paste(output_dir,"scores_questions-", window, "/", sep="")

#loop through a file with all output weeks data and plot scores
setwd(input_dir)

pdf(output_pdf <- paste(output_dir, "histograms-questions_each_", window, ".pdf", sep=""), height=16, width=8, onefile=TRUE)

#uncomment (and change for loop) to run on first two csvs only
#list <- c("scores_output-questions_by_week-2012-08-06.csv", "scores_output-questions_by_week-2012-08-13.csv")

#open each week file and append data to a dataframe, send to plot
for (file in list.files()) {
	window_data <- read.csv(file, head=T, sep="")
	
	questions <- unique(window_data$question)
		
	for (q in questions){
		# print(q)
		
		one_question <- window_data[window_data$question == q,]
		
		# print(one_question)
					
		#skip plotting if scores are NA or 0
		if (NA %in% one_question$score | sum(one_question$score) == 0) {
			#print(paste("skipping ", q, sep=""))
			next
		}
		
		plot <- ggplot(data=one_question, aes(score), environment = environment()) + geom_histogram() + ggtitle(title) #+ geom_vline(aes(xintercept = mean))	
	
		print(plot)
	}
	
}

dev.off()

