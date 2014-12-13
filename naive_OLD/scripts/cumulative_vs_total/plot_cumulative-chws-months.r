#read in "cumulative" and "total" scores and plot one against the other

require(ggplot2)
require(scales)
require(gdata)	

output_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/naive/output_data/cumulative_scores/"

input_file_cum <- paste(output_dir, "cumulative_months_scores-chws.csv", sep="")
input_file_total <- paste(output_dir, "cumulative_total_scores-chws.csv", sep="")

#####
# Start
#####

#loop through a file with all output months data and plot scores
df_cum <- read.csv(input_file_cum, sep="")
df_total <- read.csv(input_file_total, sep="")

#shared is the intersection of CHWs in the two df's - make sure they have the same CHWs
shared <- intersect(df_cum$chw, df_total$chw)

a <- data.frame(chw=character(0), score=numeric(0))

#make sure this chw is in both the months/weeks and total data - remove from df if not
for (c in df_cum$chw){
	if (!(c %in% shared)){
		print(c)
		a <- rbind()
	}
	
	#fill a == c with NA, then remove??
	
# #the following puts the three NON shared CHWs into a...	
# 	for (c in df_cum$chw){
# 	    if ((c %in% shared)){
# 	        print(c)
# 	        a <- subset(df_cum, df_cum$chw != c)
# 	    }}
# 		
	
	# print(not_shared)
	# 
	# if (!(c %in% not_shared)){
	# 	print(c)
	# 	df_cum <- df_cum[df_cum$chw == c,]
	# }
}

#convert to char for sorting chw rows
df_cum$chw <- as.character(df_cum$chw)
df_total$chw <- as.character(df_total$chw)

#sort each df by chw so they can stay aligned, even if we remove some CHW rows
df_total$chw <- sort(df_total$chw)
df_cum$chw <- sort(df_cum$chw)

#combine the total and cumulative scores into a single data frame
all_df <- df_cum
all_df <- cbind(df_cum, df_total)

colnames(all_df) <- c("chw", "cumulative", "chw2", "total")
print(all_df)

correlation <- cor.test(all_df$cumulative, all_df$total)
print(correlation)

pdf(output_pdf <- paste(output_dir, "cum_and_total_scores-chws-months.pdf", sep=""), height=12, width=12, onefile=TRUE)

plot <- ggplot(all_df, aes(x=cumulative, y=total)) + geom_point() + ggtitle(paste("Total vs Cumulative Scores for Questions, r = ", correlation)) + 
		xlab("cumulative scores - summed across weeks") + ylab("total scores - direct output from MMA") #+ geom_abline(slope=correlation)

print(plot)

output_csv <- paste(output_dir, "cum_and_total_scores-chws-months.csv", sep="")
write.table(all_df, output_csv)	

dev.off()
