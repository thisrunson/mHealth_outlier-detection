#loop through a file with all output weeks data and read into variables
	input_dir <- "/Users/tedmccarthy/Documents/Research\ Projects/CHW\ Outlier\ Detection/output_data/sliding_window-weeks/"
	
	setwd(input_dir)

	cohort_df <- data.frame(week = character(0), chw = character(0), score = numeric(0), rank = numeric(0))

	i <- 1

	#go through the first x scores output files, pull out the top n CHWs in these, and then find these same CHWs ranks for subsequent weeks
	for (file_name in list.files()) {
		one_week <- read.csv(file_name, head=F, sep="", skip=1)
	
		rank <- c(1:nrow(one_week))
	
		one_week <- cbind(i, one_week, rank)
		cohort_df <- rbind(cohort_df, one_week)

		i <- i + 1
		
		colnames(cohort_df) <- c("week", "chw", "score", "rank")
		
		
	}
	
	return(cohort_df)