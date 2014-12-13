#sum weekly chw scores into a single "cumulative" score

output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/normalized_data/output_data/"
input_file <- paste(output_dir, "questions_with_weeks.csv", sep="")

#####
# Start
#####

#loop through a file with all output weeks data and plot scores
df <- read.csv(input_file, sep="")

chws_df <- data.frame(chw = character(0), score = numeric(0))

chws <- unique(df$chw)

for (c in chws){
	
	one_chw <- df[df$chw == c,]
	
	cum_score <- sum(one_chw$score, na.rm=T)
	
	#add new score to the questions_df
	row <- cbind(c, cum_score)	
	
	print(row)
	
	chws_df <- rbind(chws_df, row)
}

colnames(chws_df) <- c("chw", "score")

output_csv <- paste(output_dir, "cumulative_scores/cumulative_week_scores-chws.csv", sep="")
write.table(chws_df, output_csv)	

