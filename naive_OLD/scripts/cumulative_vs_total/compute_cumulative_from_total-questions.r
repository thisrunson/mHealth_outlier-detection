#sum question scores in all_collection file into a single "cumulative" score

output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/output_data/"
input_file <- paste(output_dir, "all_scores_data/scores_output-all_collection-three_removed.csv", sep="")

#####
# Start
#####

#loop through a file with all output weeks data and plot scores
df <- read.csv(input_file, sep="")

questions_df <- data.frame(question = character(0), score = numeric(0))

questions <- unique(df$question)

for (q in questions){
	
	one_q <- df[df$question == q,]
	
	cum_score <- sum(one_q$score, na.rm=T)
	
	#add new score to the questions_df
	row <- cbind(q, cum_score)	
	questions_df <- rbind(questions_df, row)
}

colnames(questions_df) <- c("question", "score")

output_csv <- paste(output_dir, "cumulative_scores/cumulative_total_scores-questions.csv", sep="")
write.table(questions_df, output_csv)	


