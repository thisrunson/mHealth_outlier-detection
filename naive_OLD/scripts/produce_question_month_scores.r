#loops through folder of question scores by month and aggregates into a single dataframe
#takes a while to run!

output_dir <- "/Users/tedmccarthy/Dropbox/Outlier Detection/naive/output_data/"
input_dir <- paste(output_dir, "scores_questions-months", sep="")

#####
# Start
#####

#loop through a file with all output weeks data and plot scores
setwd(input_dir)

questions_df <- data.frame(month = character(0), question = character(0), score = numeric(0))

month_num <- 1

#open each week file and append data to a dataframe 
for (file in list.files()) {
	one_month <- read.csv(file, head=T, sep="")
	
	#add week number to questions_df	
	one_month <- cbind(month_num, one_month)			
	questions_df <- rbind(questions_df, one_month)
	
	print(questions_df)
	
	month_num <- month_num + 1
}
	
output_csv <- paste(output_dir, "questions_with_months.csv", sep="")
write.table(questions_df, output_csv)	