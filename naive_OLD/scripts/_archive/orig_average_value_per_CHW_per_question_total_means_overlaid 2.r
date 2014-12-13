##Add a column name for start_date with the proper formatting, pull unique CHW names from file, make a subset of data for each CHW, and plot a histogram of the number of reports collected each day for each CHW

# Good resource for plotting and labeling time data: https://stat.ethz.ch/pipermail/r-help/2004-May/051251.html


require(ggplot2)
require(scales)
require(gdata)	


#####
# Directories
#####
# output_dir <- "/Users/tedmccarthy/Documents/Research Projects/CHW Outlier Detection/graphs_output/"
# input_file <- "/Users/tedmccarthy/Dropbox/Outlier Detection/Organizations/CARE - Bihar/ASHAs_only.csv"
output_dir <- "/Users/bderenzi/CSEDropbox/Dropbox/Outlier Detection/graphs_output/average_value/"
input_file <- "/Users/bderenzi/CSEDropbox/Dropbox/Outlier Detection/Organizations/CARE - Bihar/ASHAs_only.csv"

######
# From the R cookbook:
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
######
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots <- length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

load_data <- function() {
	CHW_file <- read.csv(input_file)
	
	#convert date/time to proper format, strip time to only have date and add a col named start_date
	start_date_time <- strptime(as.character(CHW_file$formatted_timeStart), "%Y-%m-%d %H:%M:%S")
	start_date_only <- substr(start_date_time, 1, 10)
	CHW_file <- data.frame(start_date_only, CHW_file)
	
	# Convert it to numeric/date so that we can process it more quickly
	date_number <- as.Date(CHW_file$start_date_only)
	CHW_file <- data.frame(date_number, CHW_file)
	
	#find all unique CHWs in file
	unique_CHWs <- unique(CHW_file$form.meta.userID)
	return (CHW_file)
}

process_data <- function(unique_dates, this_question, question, method) {
	N <- length(unique_dates)
	df <- data.frame(date=rep(NA,N), mean=rep(NA,N), stdev=rep(NA,N), min=rep(NA,N), max=rep(NA,N), stringsAsFactors=FALSE)
 
	previous_day <- as.Date('2012-06-30', "%Y-%m-%d")
	days <- seq(from=as.Date('2012-07-01', "%Y-%m-%d"), to=as.Date("2013-05-01", "%Y-%m-%d"),by=method)

	# previous_day <- '2012-09-10'
	# days <- seq(from=as.Date('2012-09-11'), to=as.Date("2012-09-14"),by='days')
	max <- 0
	min <- 0
	for ( i in seq_along(days) ) {
		# print(days[i])
		now_data <- subset(this_question, 
						this_question$date_number > previous_day & 
						this_question$date_number <= days[i])
	
		###
		# clean
		now_data <- now_data[ which(now_data[,question] != 'NA'), ]
	
		# get the mean
		now_mean <- mean(now_data[,question])
		max <- max(max, max(now_data[,question]))
		min <- min(min, min(now_data[,question]))
		
		stdev <- sd(now_data[,question])
	
		df[i,] <- c(toString(days[i]), now_mean, stdev, min, max)
		previous_day <- days[i]
	}
	####
	# clean
	df <- df[ which(df$date != 'NA'), ]
	df$date <- as.Date(df$date, "%Y-%m-%d")
	df$mean <- as.numeric(df$mean)
	df$stdev <- as.numeric(df$stdev)	
	
	df$min <- as.numeric(df$min)
	df$max <- as.numeric(df$max)

	return(df)
}


plot_everything <- function(all_data_df, min, max, plot_method){	
	#compute a ribbon for mean +/- SD
	error_line <- aes(ymax = (all_data_df$whole_mean + all_data_df$whole_SD), ymin = (all_data_df$whole_mean - all_data_df$whole_SD))
	
	return(ggplot(data=all_data_df, aes(date)) + geom_line(aes(y=whole_mean, colour="whole_mean")) + geom_line(aes(y=individual_mean, colour="individual_mean")) + geom_ribbon(error_line, alpha=0.5) + ylim(min, max) + ggtitle(plot_method))
}


process_question <- function(input_data, question, plot_method) {
	# inputs
	keep_cols = c(question, "start_date_only", "date_number")
	
	#this_question is now the df of cols we want to keep in the input data
	this_question <- input_data[keep_cols]
	unique_dates <- unique(this_question$start_date_only)

	levs <- levels(this_question[,question])
	if( !is.null(levs) ) {
		levs = levs[levs!=""]
		levs = levs[levs!="---"]
	
		this_question[,question] = factor(this_question[,question], levs, seq(1,length(levs)))
		this_question[,question] = as.numeric(this_question[,question])
	}
	
	processed <- process_data(unique_dates, this_question, question, plot_method)
	#p2 <- process_data(unique_dates, this_question, question, "weeks")
	#p3 <- process_data(unique_dates, this_question, question, "months")
	
	return (processed)
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

for(col in cols ) {

	print(col)
	
	if(col != "form.anc_latest_num") next
	
	#loop through the unique_CHWs, make a subset for each, and call process_question for each subset
	for(CHW in unique_CHWs){
		CHW_subset <- subset(CHW_file, CHW_file$form.meta.userID == CHW)
		
		#test prints to see some of the data
		#printout <- paste(CHW, CHW_subset$start_date_only, CHW_subset[,col], sep="--")
		#print(printout)
		
		pdf(paste(output_dir, col, CHW, ".pdf", sep=""), height=16, width=8, onefile=TRUE)
		print(CHW)
		
        #add in processed_whole_days, processed_whole_weeks, processed_whole_months
        #enclose the following (up to multiplot call) in a for() loop, 
        methods <- c("days", "weeks", "months")
        
        p <- list()
        #i <- 1
		
		p1 <- 1
		p2 <- 1
		p3 <- 1
		
        for (method in methods){                        
                #process the question for both sets of data (subset for individual CHW, and whole file for all CHWs)
                processed_subset <- process_question(CHW_subset, col, method)
                processed_whole <- process_question(CHW_file, col, method)
                
                #get the min and max from the bottom row of the data frame
                min <- tail(processed_whole$min, 1)
                max <- tail(processed_whole$max, 1)
                
                #create a data frame from all the data processed in the other functions
                all_data_df <- data.frame(processed_whole$date, processed_whole$mean, processed_whole$stdev, processed_subset$mean, processed_subset$stdev)        
                colnames(all_data_df) <- c("date", "whole_mean", "whole_SD", "individual_mean", "individual_SD")
                
                #send all processed subset and whole_file data to plot method, store plot in a var for plotting later
                
                #assigning a ggplot variable to var p and sending p to multiplot WORKS
                p <- plot_everything(all_data_df, min, max, method)
				
				p1 <- plot_everything(all_data_df, min, max, method)
				p2 <- plot_everything(all_data_df, min, max, method)
				p3 <- plot_everything(all_data_df, min, max, method)
		
                #assigning it to p1 and sending p1 to multiplot DOES NOT work
                if(method == "days"){ 
					#p[i] <- plot_everything(all_data_df, min, max, method)
                   	p1 <- plot_everything(all_data_df, min, max, method)
                }
                if(method == "weeks"){
					#p[i] <- plot_everything(all_data_df, min, max, method)	
					p2 <- plot_everything(all_data_df, min, max, method)
                        
                }
                if(method == "months"){
				    #p[i] <- plot_everything(all_data_df, min, max, method)
           		   	p3 <- plot_everything(all_data_df, min, max, method)
                }
                
				i <- i + 1
        }
        
        #call multiplot function with the list of plots
        multiplot(p1, p2, p3, cols=1)
		
		dev.off()
	}
	
}
 