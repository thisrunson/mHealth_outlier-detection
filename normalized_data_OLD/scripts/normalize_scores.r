# Normalize the "total" data scores, weekly windows scores, and monthly windows scores

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	


#####
# Directories
#####
output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/normalized_data/output_data/"
input_file <- paste("/Users/tedmccarthy/Dropbox/Outlier\ Detection/all_scores_data/scores_output-all_collection-three_removed.csv", sep="")

weeks_indir <- 


#####
# Start here
#####
df <- read.csv(input_file, sep="")

normalized_df <- data.frame(chw = character(0), num_over = numeric(0), percent_over = numeric(0), n_questions = numeric(0))

unique_chws <- unique(df$chw)

#for each CHW, find percent weeks over the threshold, put this in the percentover_df
for (c in unique_chws){
	
	this_chw <- df[df$chw == c,]
	
	nquestions <- nrow(this_chw)
	
	norm_score <- this_chw$score / nquestions
	
	#replace score col with norm_score to use normalized scores as base data
	df[df$chw == c,]$score <- norm_score
		
	print(this_chw)
}

output_csv <- paste(output_dir, "all_data-normalized.csv", sep="")
write.table(df, output_csv)	
 
