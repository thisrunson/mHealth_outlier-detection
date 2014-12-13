#sum chw scores in all_collection file into a single "cumulative" score

output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/normalized_data/output_data/"
input_file <- paste(output_dir, "all_data-normalized.csv", sep="")

#####
# Start
#####

#loop through a file with all output weeks data and plot scores
df <- read.csv(input_file, sep="")

chw_df <- data.frame(chw = character(0), score = numeric(0))

chws <- unique(df$chw)

for (c in chws){
	
	one_chw <- df[df$chw == c,]
	
	cum_score <- sum(one_chw$score, na.rm=T)
	
	#add new score to the chw_df
	row <- cbind(c, cum_score)	
	chw_df <- rbind(chw_df, row)
}

colnames(chw_df) <- c("chw", "score")

output_csv <- paste(output_dir, "cumulative_scores/cumulative_total_scores-chws.csv", sep="")
write.table(chw_df, output_csv)	


