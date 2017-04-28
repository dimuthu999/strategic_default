rm(list=ls())
library(data.table)
library(zoo)
library(sqldf)
library(plyr)
library(lubridate)

hpi <- read.csv("C:/Dim/strategic_default/Zip_Zhvi_SingleFamilyResidence.csv")
hpi <- data.frame(hpi[2], stack(hpi[8:ncol(hpi)]))
names(hpi)<-c("zip","current_hpi","month")
hpi$zip <- floor(hpi$zip/100)*100
hpi$month <- as.Date(paste(substr(hpi$month,2,5),substr(hpi$month,7,8),"01",sep = "-"))
hpi$current_hpi <- as.double(hpi$current_hpi)
hpi <- data.table(hpi)
setkeyv(hpi,c('month','zip'))
hpi<-hpi[,list(hpi=median(current_hpi,na.rm = TRUE)),by=list(zip,month)]


allloans <- readRDS("C:/Dim/strategic_default/Freddie_Origination/freddie_orgination_data_upto_Q32015.rds")
allloans <- allloans[allloans$occupancy=="O" & allloans$purpose=="P" & allloans$propertytype=="SF" & allloans$loanterm==360 & allloans$mtgtype=="FRM" & allloans$noofunits==1,]
allloans <- allloans[allloans$zip %in% unique(hpi$zip),]
loanids <- unique(allloans$loanid)
allloans <- data.table(allloans)
setkeyv(allloans,'loanid')

setwd("C:/Dim/strategic_default/Freddie_Performance")
filelist = list.files(pattern = ".*.txt")

for(fn in filelist) {
  tryCatch({
    cat("\n File",fn,"\n")
    pefdata <- read.table(fn, sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE)
    names(pefdata)<-c("loanid","reportingperiod_t","currentupb_t","delstatus_t","age_t","monthstomaturity_t","repurchase_t","modification_t","zbcode_t","zbdate_t","interestrate_t","differedupb_t","lastduedate_t","insrecovery_t","salesproceeds_t","noninsrecovery_t","expenses_t","legal_cost","main_cost","tax_n_insurance","miss_ins","act_loss")
   
    pefdata <- pefdata[pefdata$loanid %in% loanids,]
    modified_loanids <- unique(pefdata[pefdata$modification_t=="Y",]$loanid)
    pefdata <- pefdata[!pefdata$loanid %in% modified_loanids,]
    pefdata$reportingperiod_t <- as.Date(paste(substr(pefdata$reportingperiod_t,1,4),"-",substr(pefdata$reportingperiod_t,5,6),"-01",sep=""))
  

    # pefdata$delstatus_t <- ifelse(pefdata$delstatus_t=="1","0",pefdata$delstatus_t)
    # pefdata$delstatus_t <- ifelse(pefdata$delstatus_t=="2","1",pefdata$delstatus_t)
    
    pefdata['current_date']<- pefdata$reportingperiod_t

    pefdata <- data.table(pefdata)
    setkeyv(pefdata,c('loanid','current_date'))

    pefdata<-merge(pefdata,allloans,by=c("loanid"),all.x = TRUE)
    pefdata$orgdate <- pefdata$current_date %m-% months(pefdata$age_t+1)

    pefdata <- pefdata[pefdata$zip %in% unique(hpi$zip),]  
    
    names(hpi) <- c("zip","month","org_hpi")
    pefdata <- merge(pefdata,hpi, by.x = c("zip","orgdate"), by.y =c('zip','month'))

    names(hpi) <- c("zip","month","current_hpi")
    pefdata <- merge(pefdata,hpi, by.x = c("zip","current_date"), by.y =c('zip','month'),all.x = TRUE)

    pefdata <- as.data.frame(pefdata)

    pefdata$ltv <- ifelse(pefdata$cltv>pefdata$ltv,pefdata$cltv,pefdata$ltv)


    pefdata <- pefdata[!is.na(pefdata$currentupb_t),]
    pefdata['current_housevalue'] <- (pefdata$upb*100/pefdata$ltv)*(pefdata$current_hpi)/pefdata$org_hpi
    pefdata['current_equity'] <- (pefdata$current_housevalue - (pefdata$currentupb_t))/(pefdata$current_housevalue)
    pefdata<-pefdata[is.finite(pefdata$current_equity),]
    pefdata<-pefdata[!is.na(pefdata$current_equity),]
    
    keep_names <- c("zip","current_date","orgdate","loanid","currentupb_t","delstatus_t","age_t","zbcode_t","zbdate_t","fico","msa","pmi","cltv","dti","upb","ltv","interestrate","channel","prepaypanelty","state","org_hpi","current_hpi","current_housevalue","current_equity")
    pefdata <- pefdata[,names(pefdata) %in% keep_names]
    
    saveRDS(pefdata,file = paste(substr(fn,1,nchar(fn)-4),".rds",sep = ""))
    
  })
}