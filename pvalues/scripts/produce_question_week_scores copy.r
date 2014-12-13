#loops through folder of question scores by week and aggregates into a single dataframe
#takes a while to run!

output_dir <- "/Users/tedmccarthy/Dropbox/Outlier Detection/pvalues/output_data/"
input_dir <- paste(output_dir, "scores_questions-weeks", sep="")

#####
# Start
#####

#loop through a file with all output weeks data and plot scores
setwd(input_dir)

questions_df <- data.frame(week = character(0), question = character(0), score = numeric(0))

week_num <- 1

#open each week file and append data to a dataframe 
for (file in list.files()) {
	one_week <- read.csv(file, head=T, sep="")
	
	#add week number to questions_df	
	one_week <- cbind(week_num, one_week)			
	questions_df <- rbind(questions_df, one_week)
	
	print(questions_df)
	
	week_num <- week_num + 1
}
	
output_csv <- paste(output_dir, "questions_with_weeks.csv", sep="")
write.table(questions_df, output_csv)	