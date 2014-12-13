#read in "cumulative" and "total" scores and plot one against the other

require(ggplot2)
require(scales)
require(gdata)	

output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/naive/output_data/cumulative_scores/"

input_file_cum <- paste(output_dir, "cumulative_week_scores-questions.csv", sep="")
input_file_total <- paste(output_dir, "cumulative_total_scores-questions.csv", sep="")

#####
# Start
#####

#loop through a file with all output weeks data and plot scores
df_cum <- read.csv(input_file_cum, sep="")
df_total <- read.csv(input_file_total, sep="")

#combine the total and cumulative scores into a single data frame
all_df <- df_cum
all_df <- cbind(df_cum, df_total$score)

colnames(all_df) <- c("question", "cumulative", "total")

print(all_df)

correlation <- cor.test(all_df$cumulative, all_df$total)
print(correlation)

pdf(output_pdf <- paste(output_dir, "cum_and_total_scores-questions.pdf", sep=""), height=12, width=12, onefile=TRUE)

plot <- ggplot(all_df, aes(x=cumulative, y=total)) + geom_point() + ggtitle(paste("Total vs Cumulative Scores for Questions, r = ", correlation)) + 
		xlab("cumulative scores - summed across weeks") + ylab("total scores - direct output from MMA") #+ geom_abline(slope=correlation)

print(plot)

output_csv <- paste(output_dir, "cum_and_total_scores-questions.csv", sep="")
write.table(all_df, output_csv)	

dev.off()
