##Read in weeks and months CSVs, sum these per CHW, and output a sorted list of CHW scores

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	

window <- "months"

#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier Detection/normalized_data/output_data/"
input_dir <- paste(output_dir, "scores_questions-", window, "/", sep="")

##########
# Start
##########

#loop through a file with all output weeks data
setwd(input_dir)

#open each week file and append data to a dataframe 
for (file in list.files()) {
	
	one_week <- read.csv(file, head=T, sep="")	
	
	date_label <- substr(file, 34, 43)
	print(date_label)
	
	chws_df <- data.frame(chw=character(0), score=numeric(0))
	
	unique_chws <- unique(one_week$chw)

	#for each CHW, find percent weeks over the threshold, put this in the percentover_df
	for (c in unique_chws){

		this_chw <- one_week[one_week$chw == c,]
		
		summed_score <- sum(this_chw$score, na.rm=T)
	
		one_row <- cbind(c, summed_score)
	
		chws_df <- rbind(chws_df, one_row)
	}

	colnames(chws_df) <- c("chw", "score")

	chws_df$score <- as.numeric(as.character(chws_df$score))
	
	#sort outliers by summed score, highest to lowest
	chws_df <- chws_df[with(chws_df, order(-score)), ]
	
	print(chws_df)
	
	output_file <- paste(output_dir, "sorted_scores_", window, "-", date_label, ".csv", sep="")
	write.table(chws_df, output_file, row.names=F)

}




