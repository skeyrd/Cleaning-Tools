#Libraries
library(tidyverse)
library(rmarkdown)
library(RODBC)
library(RODBCext)
library(optparse)
library(data.table)
library(Matrix)
library(outliers)
library(plyr)
library(dplyr)


server <- "usdbsvr6"
database <-"cenace"
fileName1 <- "mx_cenace_noroeste_industrial_l2_load_act"
#startDate <- "20160101" 
#endDate <- "20170202"  
type <- "extreme"  ## select type, can me mild or extreme -- ie. mild removes even 'mild' outliers, extreme, just very big ones
directory<- "C:/Users/sspinetto/Desktop/ForClip.csv"  ##local location for  file creation




#### Don't touch anything below this, just select all & run!! ##################################################################################
################################################################################################################################################
#This is a script that outputs parameters for clipping for any given dataset-- it is based off of summary stats per month
################################################################################################################################################


#set connections
setServ <- paste0("driver={SQL Server};server=",server,";database=",database,";trusted_connection=true")
myServer2 <- odbcDriverConnect(setServ)


## select file



## perform query
query1 <- paste0("select date,time,load_act from ",fileName1
 #     ," where date > ",startDate," AND date < ", endDate   #uncomment this line if using custom date set 
                 )
vector1 <-  sqlQuery(myServer2,query1)

### plot data


## group data & summarize by month + adding some features

vector1$month <- substr(vector1$date, 5,6)
vector1$year <- substr(vector1$date, 1,4)
vector1$dateyear<- paste0(vector1$date, sprintf("%02d",(vector1$time/100)))
monthlySummary <- as.list(tapply(vector1$load_act, vector1$month, summary))
monthly <- split(vector1, vector1$month)


identifyOutliers<- function(data,type){
myType <- type

lowerq = quantile(data[[3]])[2]
upperq = quantile(data[[3]])[4]
iqr = upperq - lowerq 

#Compute the bounds for a mild outlier:
  
mild.threshold.upper = (iqr * 1.5) + upperq
mild.threshold.lower = lowerq - (iqr * 1.5)

#Any data point outside (> mild.threshold.upper or < mild.threshold.lower) these values is a mild outlier

#To detect extreme outliers do the same, but multiply by 3 instead:
  
extreme.threshold.upper = (iqr * 3) + upperq
extreme.threshold.lower = lowerq - (iqr * 3)
if(myType == "mild"){
result <- subset(data[[3]],data [[3]] > mild.threshold.upper | data[[3]] < mild.threshold.lower)} else 
  {result <- subset(data[[3]],data [[3]] > extreme.threshold.upper | data[[3]] < extreme.threshold.lower)}
return(result)
}

loop_clipped_data <- vector("list", length(monthly))
 
for(x in seq_along(monthly)){
 
    loop_clipped_data[[x]] <- identifyOutliers(monthly[[x]], type)
}
 
### generate CLIP file to be inserted into load.in

monthly$`01`$load_act<-car::recode(monthly$`01`$load_act,'loop_clipped_data[[1]]="BAD DATA"')
monthly$`02`$load_act<-car::recode(monthly$`02`$load_act,'loop_clipped_data[[2]]="BAD DATA"')
monthly$`03`$load_act<-car::recode(monthly$`03`$load_act,'loop_clipped_data[[3]]="BAD DATA"')
monthly$`04`$load_act<-car::recode(monthly$`04`$load_act,'loop_clipped_data[[4]]="BAD DATA"')
monthly$`05`$load_act<-car::recode(monthly$`05`$load_act,'loop_clipped_data[[5]]="BAD DATA"')
monthly$`06`$load_act<-car::recode(monthly$`06`$load_act,'loop_clipped_data[[6]]="BAD DATA"')
monthly$`07`$load_act<-car::recode(monthly$`07`$load_act,'loop_clipped_data[[7]]="BAD DATA"')
monthly$`08`$load_act<-car::recode(monthly$`08`$load_act,'loop_clipped_data[[8]]="BAD DATA"')
monthly$`09`$load_act<-car::recode(monthly$`09`$load_act,'loop_clipped_data[[9]]="BAD DATA"')
monthly$`10`$load_act<-car::recode(monthly$`10`$load_act,'loop_clipped_data[[10]]="BAD DATA"')
monthly$`11`$load_act<-car::recode(monthly$`11`$load_act,'loop_clipped_data[[11]]="BAD DATA"')
monthly$`12`$load_act<-car::recode(monthly$`12`$load_act,'loop_clipped_data[[12]]="BAD DATA"')


### replace outliers with MEDIAN -- this is not used currently
#monthly$`01`$load_act<-car::recode(monthly$`01`$load_act,'loop_clipped_data[[1]]=monthlySummary$`01`[[3]]')
#monthly$`02`$load_act<-car::recode(monthly$`02`$load_act,'loop_clipped_data[[2]]=monthlySummary$`02`[[3]]')
#monthly$`03`$load_act<-car::recode(monthly$`03`$load_act,'loop_clipped_data[[3]]=monthlySummary$`03`[[3]]')
#monthly$`04`$load_act<-car::recode(monthly$`04`$load_act,'loop_clipped_data[[4]]=monthlySummary$`04`[[3]]')
#monthly$`05`$load_act<-car::recode(monthly$`05`$load_act,'loop_clipped_data[[5]]=monthlySummary$`05`[[3]]')
#monthly$`06`$load_act<-car::recode(monthly$`06`$load_act,'loop_clipped_data[[6]]=monthlySummary$`06`[[3]]')
#monthly$`07`$load_act<-car::recode(monthly$`07`$load_act,'loop_clipped_data[[7]]=monthlySummary$`07`[[3]]')
#monthly$`08`$load_act<-car::recode(monthly$`08`$load_act,'loop_clipped_data[[8]]=monthlySummary$`08`[[3]]')
#monthly$`09`$load_act<-car::recode(monthly$`09`$load_act,'loop_clipped_data[[9]]=monthlySummary$`09`[[3]]')
#monthly$`10`$load_act<-car::recode(monthly$`10`$load_act,'loop_clipped_data[[10]]=monthlySummary$`10`[[3]]')
#monthly$`11`$load_act<-car::recode(monthly$`11`$load_act,'loop_clipped_data[[11]]=monthlySummary$`11`[[3]]')
#monthly$`12`$load_act<-car::recode(monthly$`12`$load_act,'loop_clipped_data[[12]]=monthlySummary$`12`[[3]]')

### 
compositeTable <- ldply(monthly , data.frame)

##### creating excel for easy copy pasta into load.in
tempTable<- subset(compositeTable,  compositeTable$load_act == "BAD DATA", select= "dateyear")

list1 <- length(tempTable$dateyear)
list2 <- rep("CLIP",length(list1))
list3 <- rep("! bad load",length(list1))
finalTable <- data.frame(clip = list2, dates = tempTable, explanation = list3)

finalTable$clips <- paste( list2, tempTable$dateyear, list3)
### variety of sanity checks ### 
#View(finalTable)
#spread before
plot.ts(vector1$load_act)
#spread after
plot.ts(compositeTable$load_act)
### outputs file to designated location
write.csv(finalTable$clips, directory, row.names=F)
print(paste0("Output written to:  ",directory))