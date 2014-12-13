##Add a column name for start_date with the proper formatting, pull unique CHW names from file, make a subset of data for each CHW, and plot a histogram of the number of reports collected each day for each CHW

# Scores column is now x2, not MMA

# Good resource for plotting and labeling time data: https://stat.ethz.ch/pipermail/r-help/2004-May/051251.html

rm(list = ls())

require(ggplot2)
require(scales)
require(gdata)	
require(zoo)

#####
# Directories
#####
top_dir <- "/Users/bderenzi/CSEDropbox/Dropbox/Outlier\ Detection/Organizations/CRS\ -\ India/"
output_dir <- paste(top_dir, "output_data/pvalues/", sep="")
input_file <- paste(top_dir, "crs_clean.csv", sep="")


big_df <- data.frame(chw = character(0), question = character(0), score = numeric(0))

load_data <- function() {
	CHW_file <- read.csv(input_file)
	
	#convert date/time to proper format, strip time to only have date and add a col named start_date
	start_date_time <- strptime(as.character(CHW_file$formatted_timeStart), "%Y-%m-%d %H:%M:%S")
	start_date_only <- substr(start_date_time, 1, 10)
	
	CHW_file <- data.frame(start_date_only, CHW_file)
	
	# Convert it to numeric/date so that we can process it more quickly
	date_number <- as.Date(CHW_file$start_date_only)
	CHW_file <- data.frame(date_number, CHW_file)
	
	return (CHW_file)
}


get_frequencies <- function(data, col, col_vals, agg_col, chw) {
	frequencies <- data.frame(chw=rep(NA,length(col_vals)))
	
	sub <- data[data[agg_col] == chw,][col]
	i <- 1
	for(col_val in col_vals) {
		frequencies[paste(col,"_",col_val, sep="")] <- rep(NA,length(col_vals))
		frequencies[i,"chw"] <- chw
		frequencies[i,paste(col,"_",col_val, sep="")] <- length(sub[sub[col] == col_val,])
		i <- i + 1
	}	
	return(frequencies)
}

compute_x2_stat <- function(expected, actuals, rng, col) {
	# print("compute_x2")
	# error checking
	num_observations = 0
	for (r in rng) {num_observations = num_observations + as.numeric(expected[r])}
	# error check
	ret = 0
	for(r in rng) {
		local = actuals[which(actuals[,paste(col,"_",r, sep="")] != 'NA'),]
		ret <- as.numeric(ret + (
			(as.numeric(local[paste(col,"_",r, sep="")]) - as.numeric(expected[r]))^2 / max(as.numeric(expected[r]), 1)
		))
	}
	return(ret)
}

sum_frequencies <- function(agg_unit, frequencies, rng, col, unique_CHWs) {
	# print("sum_freq")
	all_frequencies <- vector(mode="list")
	other_data = frequencies[ which(frequencies[,"chw"] != agg_unit), ]
	
	for (r in rng) {
		curr = paste(col,"_",r, sep="") 
		other_data[,curr] = as.numeric(other_data[,curr])
		all_frequencies[r] = colSums(other_data[curr], na.rm=TRUE)
	}
	
	return(all_frequencies)
}

normalize_counts <- function(counts, rng, val){
	n = 0
	# print("normalize")
	for(r in rng) { n = n + as.numeric(counts[r])}
	freq <- vector(mode="list", length=length(rng))
	for(r in rng) {
		freq[r] <- val * as.numeric(counts[r]) / n
	}
	return(freq)
}

mma_compute_outlier_scores <- function(frequencies, rng, unique_CHWs, col) {
	# error check that frequencies has more that 2 rows
	# print("mma")
	# print(frequencies)
	
	#remove rng's that have NA as a value
	rng <- na.omit(rng)
	
	#test print to make sure NA's have been removed from rng
	printrng <- paste("RNG: ", rng)
	print(printrng)
	
	outlier_scores <- data.frame(chw=NA,question=NA,score=NA)
		
	for(chw in unique_CHWs) {
		intermediate_sum <- 0
		
		this_data <- frequencies[frequencies["chw"] == chw]
		this_data <- frequencies[ which(frequencies[,"chw"] == chw), ]
		for(r in rng){
			local = this_data[which(this_data[,paste(col,"_",r, sep="")] != 'NA'),]
			intermediate_sum <- intermediate_sum + as.numeric(local[paste(col,"_",r, sep="")])
		}
		
		expected_counts <- normalize_counts(
			sum_frequencies(chw, frequencies, rng, col, unique_CHWs),
			rng,
			intermediate_sum
		)
		
		x2 = compute_x2_stat(expected_counts, this_data, rng, col)
		
		#compute the x2 p-value using pchisq
		pvalue <- pchisq(x2, length(rng)-1, lower.tail = FALSE)
		
		# this is the negative log of the survival fn, which is defined as (1-cdf)
		# use the lower.tail=FALSE to get the upper tail of the distribution.
		# outlier_scores[chw] <- -log(1-pchisq(x2,length(rng)-1))
		# outlier_scores[chw] <- -log(pchisq(x2,length(rng)-1, lower.tail = FALSE))
		
		outlier_scores[chw] <- pvalue
		
		print(paste("p-value", pvalue))
		print(paste("score?", outlier_scores[chw]))
		
	}
		
	return(outlier_scores)
}

run_mma <- function(data, agg_col, cat_cols) {
	#find all unique users in file
	unique_CHWs <- unique(data[,agg_col])
	outlier_scores <- data.frame(chw=NA, question=NA, score=NA)
	j <- 1
	for( col in cat_cols ) {
		# print("run: ")
		col_vals <- unique(data[,col])
		
		#make sure col_vals isn't of type "integer"
		col_vals <- as.character(col_vals)
		
		print(col)
		
		# frequencies <- vector(mode="list", length=length(unique_CHWs))
		# names(frequencies) <- unique_CHWs
		frequencies <- data.frame(chw=rep(NA,length(col_vals)))
		for(col_val in col_vals) frequencies[paste(col,"_",col_val, sep="")] <- rep(NA,length(col_vals))
			
		frequencies <- data.frame(chw=NA)
		for(col_val in col_vals) frequencies[paste(col,"_",col_val, sep="")] <- NA
		
		rng <- 0
		i <- 1
		for (chw in unique_CHWs) {
			sub <- data[data[agg_col] == chw,][col]
			for(col_val in col_vals) {
				# fill it in
				frequencies[i,"chw"] <- chw
				frequencies[i,paste(col,"_",col_val, sep="")] <- length(sub[sub[col] == col_val,])
				i <- i + 1
			}
		}
		
		outlier_scores_for_col <- mma_compute_outlier_scores(frequencies, col_vals, unique_CHWs, col)
		
		for (chw in unique_CHWs) {
			outlier_scores[j,] = c(chw,col,outlier_scores_for_col[chw])
			j <- j + 1
		}
	}
	
	print(outlier_scores)
	
	return(outlier_scores)
}

##########
# Start
##########

CHW_file <- load_data()

# starts with "form."
cols = grep("^form\\..*$", colnames(CHW_file), value=T)
# but not "form.."
cols = grep("^.*\\.\\..*$", cols, value=T, invert=T)
# no form.meta*
cols = grep("^form\\.meta.*$", cols, value=T, invert=T)
# nothing that contains the word date.
cols = grep("^.*date.*$", cols, value=T, invert=T)
# some hardcoded skips. (all blanks)
cols = grep("form.bp2.counsel_home_delivery", cols, value=T, invert=T)
cols = grep("form.bp2.referral_prompt", cols, value=T, invert=T)

#more hardcoded skips - un-skip these later - integer/number answers, need to be binned to run in MMA
cols = grep("form.bp1.ifa_total_previous_received", cols, value=T, invert=T)
cols = grep("form.bp1.ifa_tablets_issued", cols, value=T, invert=T)
cols = grep("form.ifa_tablets_total", cols, value=T, invert=T)
cols = grep("form.del_fup", cols, value=T, invert=T)
cols = grep("total_ifa", cols, value=T, invert=T)
cols = grep("anc_display", cols, value=T, invert=T)

#make time windows - set start and end dates
week_start <- "2012-10-31" 
week_end <- "2012-11-08"

month_start <- "2012-07-31" 
month_end <- "2012-09-01"

all_collection_start <- "2012-07-31"
all_collection_end <- "2013-08-01" 

#make subsets for different time windows exclusive of the start and end dates
week_in_november <- subset(CHW_file, CHW_file$date_number > week_start & CHW_file$date_number < week_end)
first_week <- subset(CHW_file, CHW_file$date_number > week_start & CHW_file$date_number < week_end)
first_month <- subset(CHW_file, CHW_file$date_number > month_start & CHW_file$date_number < month_end)
all_collection <- subset(CHW_file, CHW_file$date_number > all_collection_start & CHW_file$date_number < all_collection_end)

columns <- vector()

#uncomment to test only a couple cols
#cols <- c("form.bp2.not_yet_maternal_emergency", "form.bp1.counsel_ifa")

#TODO: change this depending on the time window looking at
data <- all_collection

i <- 1

for (col_name in cols){

	#test to see if the column contains empty string elements or "---" elements
	if("" %in% data[,col_name] | "---" %in% data[,col_name]){
		
		col <- data[,col_name]
    
		#replace empty strings and "---"'s with NA
   	 	col<- as.character(col)
		col[col==""] <- NA
		col[col=="---"] <- NA
		
		col <- as.factor(col)
	
		data[,col_name] <- col
	}
	
	#print(data[,col_name])
	
  	columns <- append(columns, col_name)
	
	# i <- i + 1
	# if (i > 1) break
}

#remove scientific notation from printing of scores
options(scipen = 1000)

#print(columns)

scores <- (run_mma(data, "form.meta.userID", columns))

print(scores)

output_file <- paste(output_dir, "scores_output-all.csv", sep="")
write.table(scores, output_file, row.names=F)


