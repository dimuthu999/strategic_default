rm(list=ls())
library(data.table)
library(zoo)
library(sqldf)
library(plyr)
library(lubridate)

setwd("C:/Dim/strategic_default/Freddie_Performance")
filelist = list.files(pattern = ".*.txt")

file_name ="Apr_foreclosure_time.csv"
pb <- txtProgressBar(min = 1, max =length(filelist), initial = 1,style=3,char = ".")
i=1
for(fn in filelist) {
  tryCatch({
    setTxtProgressBar(pb, i)
    pefdata <- read.table(fn, sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE)
    names(pefdata)<-c("loanid","reportingperiod_t","currentupb_t","delstatus_t","age_t","monthstomaturity_t","repurchase_t","modification_t","zbcode_t","zbdate_t","interestrate_t","differedupb_t","lastduedate_t","insrecovery_t","salesproceeds_t","noninsrecovery_t","expenses_t","legal_cost","main_cost","tax_n_insurance","miss_ins","act_loss")

    pefdata <- pefdata[pefdata$zbcode_t %in% c(3,6,9),]

    pefdata$current_date <- as.Date(paste(substr(pefdata$reportingperiod_t,1,4),"-",substr(pefdata$reportingperiod_t,5,6),"-01",sep=""))
    pefdata$lastduedate_t <- as.character(pefdata$lastduedate_t)
    pefdata <- pefdata[!is.na(pefdata$lastduedate_t),]
    pefdata$default_date <- as.Date(paste(substr(pefdata$lastduedate_t,1,4),"-",substr(pefdata$lastduedate_t,5,6),"-01",sep=""))

    if(fn==filelist[1])  {
      write.table(t(names(pefdata)),file=file_name,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)
    }
    write.table(pefdata,file=file_name,append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE,na="")
    
  })
  i = i+1
}

pefdata <- read.table(file_name,sep = "|",row.names = NULL,header = TRUE)
pefdata$current_date <- as.Date(as.character(pefdata$current_date))
pefdata$default_date <- as.Date(as.character(pefdata$default_date))
pefdata['current_year']<-as.integer(format(pefdata$current_date,"%Y"))
pefdata$loanid <- as.character(pefdata$loanid)
pefdata['months_in_foreclosure']<- floor(as.numeric(pefdata$current_date - pefdata$default_date)/30)
pefdata <- as.data.table(pefdata)
setkeyv(pefdata,'loanid')

allloans <- readRDS("C:/Dim/strategic_default/Freddie_Origination/freddie_orgination_data_upto_Q32015.rds")
allloans <- data.table(allloans)
setkeyv(allloans,'loanid')

pefdata<- merge(pefdata,allloans,by="loanid")
summary <- pefdata[, list(months_in_foreclosure=mean(months_in_foreclosure)), by=list(zip,current_year)]
saveRDS(summary,file="foreclosure_months_summary.rds")
