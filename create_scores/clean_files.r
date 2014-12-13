#Clean and parse data from initial files
#Remove AWWs so final file is ASHAs only
#Reformat date and time to be yyyy-mm-dd hh:mm:ss
#Compute collection times as diff between timeEnd and timeStart

#name input path here
input_dir <- "/Users/tedmccarthy/Dropbox/Outlier\ Detection/Organizations/CARE\ -\ Bihar/" 

#read in data
dataset <- read.csv("/Users/tedmccarthy/Dropbox/Outlier\ Detection/Organizations/CARE\ -\ Bihar/bihar_cleaned_15JUL2013_brian.csv")
		
ASHAs_only <- subset(dataset, dataset$role == "ASHA")
		
ASHAs_only$formatted_timeStart <- gsub("[a-zA-Z]", " ", ASHAs_only$form.meta.timeStart)
ASHAs_only$formatted_timeEnd <- gsub("[a-zA-Z]", " ", ASHAs_only$form.meta.timeEnd)

ASHAs_only$formatted_timeEnd <- strptime(ASHAs_only$formatted_timeEnd, "%Y-%m-%d %H:%M:%S")
ASHAs_only$formatted_timeStart <- strptime(ASHAs_only$formatted_timeStart, "%Y-%m-%d %H:%M:%S")
         
collection_times <- ASHAs_only$formatted_timeEnd - ASHAs_only$formatted_timeStart
ASHAs_only$collection_times <- as.numeric(collection_times)

#output the file to input_dir/filename
write.table(ASHAs_only, file=paste(input_dir, "bihar_cleaned_15JUL2013-ASHAs_only.csv", sep=""), sep=",", row.names=F)