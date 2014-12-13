# Normalize the weekly windows scores

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/normalized_data/output_data/"
input_dir <- "/Users/tedmccarthy/Dropbox/Outlier Detection/output_data/scores_questions-weeks"

#####
# Start here
#####

#loop through a file with all output weeks data
setwd(input_dir)

#open each week file and append data to a dataframe 
for (file in list.files()) {
	
	print(file)
	one_week <- read.csv(file, head=T, sep="")
	
	unique_chws <- unique(one_week$chw)

	date_label <- substr(file, 33, 42)
	print(date_label)

	#for each CHW, find percent weeks over the threshold, put this in the percentover_df
	for (c in unique_chws){
	
		this_chw <- one_week[one_week$chw == c,]
	
		nquestions <- nrow(this_chw)
	
		norm_score <- this_chw$score / nquestions
	
		#replace score col with norm_score to use normalized scores as base data
		one_week[one_week$chw == c,]$score <- norm_score
		
	}
	
	output_file <- paste(output_dir, "scores_output-questions_by_week-", date_label, ".csv", sep="")
	write.table(one_week, output_file, row.names=F)
	
	print(one_week)
}


