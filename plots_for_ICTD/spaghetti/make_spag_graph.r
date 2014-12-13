#makes a graph of the top three CHWs' (for CHWs with >20 weeks of data) ranks for each week over the entire collection period

require(ggplot2)

df <- read.csv("/Users/tedmccarthy/Dropbox/Outlier\ Detection/plots_for_ICTD/spaghetti/source_data.csv")

pdf("/Users/tedmccarthy/Dropbox/Outlier\ Detection/plots_for_ICTD/spaghetti/top_three_CHWs-final.pdf")

p <- ggplot(data = df, aes(x=weeks)) + geom_line(aes(y=top_1, linetype="top_1")) + geom_line(aes(y=top_2, linetype="top_2")) + geom_line(aes(y=top_3, linetype="top_3")) + xlab("Week") + ylab("CHW percentile rank by week") + scale_linetype_manual(values=c(1,5,3), name="", labels=c("1st", "2nd", "3rd")) + ggtitle("3 Most Anomalous CHWs - Percentile Anomaly Rank by Week") + theme(axis.title=element_text(size=18), axis.text=element_text(size=18), legend.text=element_text(size=18), plot.title=element_text(size=24))

print(p)

dev.off()

