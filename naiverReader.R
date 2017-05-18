

library(reshape2)
library(lubridate)


##read in

hist <- as.data.frame(read.csv("C:/Users/sspinetto/Desktop/transposeLapaz.csv", stringsAsFactors = FALSE, header=TRUE))
hist2 <- as.data.frame(read.csv("C:/Users/sspinetto/Desktop/transposeLapaz2.csv", stringsAsFactors = FALSE))




##set Dates
myDates <- as.character(seq(ymd_h(2000010100), ymd_h(2013082923), by="hours"))
##tranpose raw csv convert into single vector
vec1 <- as.vector(t(hist2))
## check to see if dates  and load_act match
soritechRdy<- length(myDates)-length(vec1)
##prepare data for framing
###changes date output and hour outputs to get rid of symbols
myDates<- gsub(":","",myDates)
myDates<- gsub("-","",myDates)
#split columns
myDates2<- substr(myDates,1,8)
myHours<- substr(myDates,10,11)
#my frame
myFrame <- data.frame(myDates2,myHours,vec1)
### print it
colnames(myFrame) <- c("date","time","load_act")
##myFrame<- myFrame[complete.cases(myFrame),]

write.csv(myFrame, "C:/Users/sspinetto/Desktop/lapazHistory2000_2013.csv", row.names=F)



### for fun/practice custom days of dealing with timechange, creating a series with just timechange days
# 
# values<- c(
# ymd_h(2000040201)+ weeks (30),
# ymd_h(2001050601)+ weeks (21),
# ymd_h(2002040701) +weeks (29),
# ymd_h(2003040601) + weeks(29),
# ymd_h(2004040401)+ weeks(30),
# ymd_h(2005040301)+ weeks(30),
# ymd_h(2006040201)+ weeks(30),
# ymd_h(2007040101)+ weeks(30),
# ymd_h(2008040601)+ weeks(29),
# ymd_h(2009040501)+ weeks(29),
# ymd_h(2010040401)+ weeks(30),
# ymd_h(2011040301)+ weeks(30))
# 
# f<- function(x){ x + days(1)}
# valuesX <-lapply(values, f)
# 
# 
# 
# 
# timeChange<- function (x,y){hello<- seq(ymd_h(values[[1]]), ymd_h(valuesX[[1]], by="hours")  }
# 
# loop_clipped_data <- vector("list", length(values))
# 
# for(x in seq_along(values)){
#   
#   loop_clipped_data[[x]] <- timeChange(x=values[[x]], y=valuesX[[x]])
# }