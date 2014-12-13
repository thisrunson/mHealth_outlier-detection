#################
## IMPORTANT
#################
# You must set the WD before running the script. E.g.:
#
# setwd("/Users/bderenzi/CSEDropbox/Dropbox/Outlier Detection/")
# source('/Users/bderenzi/CSEDropbox/Dropbox/Outlier Detection/scripts/MMA/compute_anomalous.r')


#set threshold value (for question) - 90% percentile

#for each CHW
#	subset that CHW's data from questions-chws-weeks-all.csv
#	
#	for each week
#		for each question
#			is score > threshold?
#				compute yesses vs no's


#check how often a CHW has an anomalous score

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	
require(yaml)

#TODO: choose "weeks" or "months"
window <- "weeks"

set_threshold <- function(df){
	
	# remove questions where score = 0
	df <- df[df$score != 0,]
	
	#set outlier threshold to be more than two st. devs away from the mean
	mean_score <- mean(df$score, na.rm=TRUE)
	sd_score <- sd(df$score, na.rm=TRUE)
	
	threshold <- mean_score + sd_score * 2  
	# threshold <- mean_score + sd_score
	#print(threshold)
	
	return(threshold)
}

#####
# Directories
#####
my = yaml.load_file(paste(getwd(),"/settings/settings.yaml",sep=""))
output_dir <- paste(my$settings$od_root, my$settings$output_dir,sep="")
input_file <- paste(output_dir,"questions-chws-weeks-all-.csv", sep="")

#open chw-week-question file and pull out chw subsets
data <- read.csv(input_file, head=T, sep="")

threshold <- set_threshold(data)

chws <- unique(data$chw)


histdata <- data.frame(date=NA,count=NA)
i <- 1
for (c in chws){
	
	this_chw <- data[data$chw == c,]
	
	#print(this_chw)

	
	weeks <- unique(this_chw$date)
	for (w in weeks){
		this_week <- this_chw[this_chw$date == w,]
		
		anom_count <- 0
		
		for (q in this_week){
			
			if(is.na(this_week$score[q])) {
				next
			}
			
			if(this_week$score[q] > threshold){
				anom_count <- anom_count + 1		
			}
		}
		
		histdata[i,] <- c(w,anom_count)
		i <- i + 1
	}
}

# pdf(output_pdf <- paste(output_dir, "anomalous-counts.pdf", sep=""), height=8, width=8, onefile=TRUE)
# 
# print(
# 	ggplot(data=histdata, aes(count), environment = environment()) + geom_histogram(binwidth = 1)
# )
# 
# dev.off()

histdata$date <- as.Date(histdata$date)
pdf(output_pdf <- paste(output_dir, "weekly-anomalous-counts.pdf", sep=""), height=8, width=8, onefile=TRUE)

print(
	ggplot(data=histdata, aes(x=date,y=count), environment = environment()) + geom_point()
)

dev.off()

