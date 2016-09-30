rm(list=ls())
library(data.table)
library(zoo)
library(sqldf)
setwd("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay")

#common columns in both Freddie and Fannie
colnames <-c('loanid','reportingperiod_t','currentupb_t','delstatus_t','age_t','monthstomaturity_t','modification_t','zbcode_t','interestrate_t')

allloans <- readRDS("CombinedFreddieFannieAct_Nov18.rds")
nonrecourse=c('AK','AZ','CA','IA','MN','MT','NC','ND','OR','WA','WI')
#allloans <- allloans[(allloans$state %in% nonrecourse) & (allloans$gse=="freddie"),]
allloans <- allloans[(allloans$gse=="freddie"),]
loanids <- as.vector(allloans$loanid)

allloans <- data.table(allloans)
setkeyv(allloans,'loanid')


hpi <- read.csv("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay/HPI3zip.csv")
hpi$zip <- hpi$zip/10
hpi['current_q']<- as.yearqtr(as.Date(hpi$yearquarter,origin="1900-01-01"))
hpi <- hpi[,c('current_q','zip','index')]
names(hpi) <- c('current_q','zip','current_hpi')
hpi <- data.table(hpi)
setkeyv(hpi,c('current_q','zip'))

hpiorg <- as.data.frame(hpi)
names(hpiorg) <- c('org_q','zip','org_hpi')
hpiorg <- data.table(hpiorg)
setkeyv(hpiorg,c('org_q','zip'))



# Freddie Logit Sample Selection current_equity<0 ------------------------------------------------
# 
# gc()
# setwd("C:/Dim/Freddie Per")
# filelist = list.files(pattern = ".*.txt")
# i=1
# for(fn in filelist) {
#   tryCatch({
#     cat("File",fn,"-",i,"\n")
#     pefdata <- read.table(fn, sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE)
#     names(pefdata)<-c("loanid","reportingperiod_t","currentupb_t","delstatus_t","age_t","monthstomaturity_t","repurchase_t","modification_t","zbcode_t","zbdate_t","interestrate_t","differedupb_t","lastduedate_t","insrecovery_t","salesproceeds_t","noninsrecovery_t","expenses_t")
#     pefdata <- pefdata[,colnames(pefdata) %in% colnames]
#     pefdata <- pefdata[pefdata$loanid %in% loanids,]
#     pefdata$reportingperiod_t <- as.Date(paste(substr(pefdata$reportingperiod_t,1,4),"-",substr(pefdata$reportingperiod_t,5,6),"-01",sep=""))
#     pefdata <- pefdata[pefdata$reportingperiod_t>='2003-01-01',]
#     
#     pefdata['current_q']<- as.yearqtr(((pefdata$reportingperiod_t)))
#     
#     pefdata <- data.table(pefdata)
#     setkeyv(pefdata,c('loanid','current_q'))
#     
#     pefdata<-merge(pefdata,allloans,by=c("loanid"),all.x = TRUE)
#     
#     pefdata <- merge(pefdata,hpi, by=c('current_q','zip'),all.x = TRUE)
#     pefdata$current_q<-NULL
#     
#     pefdata <- as.data.frame(pefdata)
#     pefdata['org_q']<-as.yearqtr(pefdata$orgdate)
#     pefdata <- data.table(pefdata)
#     setkeyv(pefdata,c('loanid','org_q'))
#     
#     pefdata <- merge(pefdata,hpiorg, by=c('org_q','zip'),all.x = TRUE)
#     pefdata$org_q<-NULL
# 
#     pefdata$cltv <- ifelse(pefdata$cltv>pefdata$ltv,pefdata$cltv,pefdata$ltv)
#     pefdata$cltv <- ifelse(is.na(pefdata$cltv),pefdata$ltv,pefdata$cltv)
#     
#     pefdata <- as.data.frame(pefdata)
#     pefdata['current_housevalue'] <- (pefdata$upb*100/pefdata$ltv)*(pefdata$current_hpi)/pefdata$org_hpi
#     pefdata['current_equity'] <- (pefdata$current_housevalue - (pefdata$currentupb_t*pefdata$cltv/pefdata$ltv))/(pefdata$currentupb_t*pefdata$cltv/pefdata$ltv)
#     lids <- unique(pefdata[pefdata$current_equity<0,]$loanid)
#     lids <- lids[!is.na(lids)]
#     pefdata <- pefdata[pefdata$loanid %in% lids, ]
#     
#     lids <- sqldf("select pd1.loanid,pd1.delstatus_t from pefdata as pd1
#                       JOIN
#                         (select loanid,min(reportingperiod_t) as start,min(reportingperiod_t)+200 as end from pefdata where current_equity<0 group by loanid) as pd2
#                       ON pd1.loanid=pd2.loanid AND pd1.reportingperiod_t=pd2.start")
#                   
#     pefdata <- sqldf("select pd1.* from pefdata as pd1 
#                         JOIN
#                           (select loanid,min(reportingperiod_t) as start,min(reportingperiod_t)+200 as end from pefdata where current_equity<0 group by loanid) as pd2
#                         ON pd1.loanid=pd2.loanid AND pd1.reportingperiod_t>=pd2.start AND pd1.reportingperiod_t <= pd2.end 
#                where pd1.delstatus_t in (0,1,2,3,4,5)")
#     
#     pefdata<-pefdata[pefdata$loanid %in% lids[lids$delstatus_t==0,'loanid'],]
#     
#     pefdata<-sqldf("select *,max(delstatus_t)<>'0' as def from pefdata group by loanid")
#           # first_ne$start <-as.Date(first_ne$start,origin="1970-01-01")
#           # first_ne$end <-as.Date(first_ne$end,origin="1970-01-01")
#           # temp <- merge(pefdata,first_ne,by=c('loanid'),all.x = TRUE)
#           # temp <- temp[temp$reportingperiod_t>=temp$start & temp$reportingperiod_t<=temp$end,]
#     
#     if(i==1)  {
#       write.table(t(names(pefdata)),file="PMT_vs_Rent_freddie_sample_July272016.csv",append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)
#     }
#     write.table(pefdata,file="PMT_vs_Rent_freddie_sample_July272016.csv",append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE,na="")
#     i= i+1
#     gc()
#   },error=function(e){
#     cat(fn,"Skipped\n")
#   })
# }



# Freddie Cox Sample Selection current_equity <0 all states--------------------------

gc()
setwd("C:/Dim/Freddie Per")
filelist = list.files(pattern = ".*.txt")
i=1
for(fn in filelist) {
  tryCatch({
    cat("\n File",fn,"-",i,"\n")
    pefdata <- read.table(fn, sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE)
    names(pefdata)<-c("loanid","reportingperiod_t","currentupb_t","delstatus_t","age_t","monthstomaturity_t","repurchase_t","modification_t","zbcode_t","zbdate_t","interestrate_t","differedupb_t","lastduedate_t","insrecovery_t","salesproceeds_t","noninsrecovery_t","expenses_t")
    pefdata <- pefdata[,colnames(pefdata) %in% colnames]
    pefdata <- pefdata[pefdata$loanid %in% loanids,]
    pefdata$reportingperiod_t <- as.Date(paste(substr(pefdata$reportingperiod_t,1,4),"-",substr(pefdata$reportingperiod_t,5,6),"-01",sep=""))
    pefdata <- pefdata[pefdata$reportingperiod_t>='2000-01-01',]
    
    pefdata['current_q']<- as.yearqtr(((pefdata$reportingperiod_t)))
    
    pefdata <- data.table(pefdata)
    setkeyv(pefdata,c('loanid','current_q'))
    
    pefdata<-merge(pefdata,allloans,by=c("loanid"),all.x = TRUE)
    
    pefdata <- merge(pefdata,hpi, by=c('current_q','zip'),all.x = TRUE)
    pefdata$current_q<-NULL
    
    pefdata <- as.data.frame(pefdata)
    pefdata['org_q']<-as.yearqtr(pefdata$orgdate)
    pefdata <- data.table(pefdata)
    setkeyv(pefdata,c('loanid','org_q'))
    
    pefdata <- merge(pefdata,hpiorg, by=c('org_q','zip'),all.x = TRUE)
    pefdata$org_q<-NULL
    
    pefdata$cltv <- ifelse(pefdata$cltv>pefdata$ltv,pefdata$cltv,pefdata$ltv)
    pefdata$cltv <- ifelse(is.na(pefdata$cltv),pefdata$ltv,pefdata$cltv)
    
    pefdata <- as.data.frame(pefdata)
    pefdata <- pefdata[!is.na(pefdata$currentupb_t),]
    pefdata['current_housevalue'] <- (pefdata$upb*100/pefdata$ltv)*(pefdata$current_hpi)/pefdata$org_hpi
    pefdata['current_equity'] <- (pefdata$current_housevalue - (pefdata$currentupb_t*pefdata$cltv/pefdata$ltv))/(pefdata$current_housevalue)
    pefdata<-pefdata[is.finite(pefdata$current_equity),]
    pefdata<-pefdata[!is.na(pefdata$current_equity),]
    
    lids <- unique(pefdata[pefdata$current_equity<=0,]$loanid)
    lids <- lids[!is.na(lids)]
    pefdata <- pefdata[pefdata$loanid %in% lids, ]
    
    lids <- sqldf("select pd1.loanid,pd1.delstatus_t from pefdata as pd1
                  JOIN
                  (select loanid,min(reportingperiod_t) as start from pefdata where current_equity<=0 group by loanid) as pd2
                  ON pd1.loanid=pd2.loanid AND pd1.reportingperiod_t=pd2.start")
    
    pefdata<-pefdata[pefdata$loanid %in% lids[lids$delstatus_t==0,'loanid'],]
    
    pefdata <- sqldf("select pd1.* from pefdata as pd1 
                     JOIN
                     (select loanid,min(reportingperiod_t) as start,min(reportingperiod_t)+366 as end from pefdata where current_equity<=0 group by loanid) as pd2
                     ON pd1.loanid=pd2.loanid AND pd1.reportingperiod_t>=pd2.start AND pd1.reportingperiod_t <= pd2.end 
                     where pd1.delstatus_t in (0,1)")
    
    nondefaulted <- pefdata[!(pefdata$loanid %in% unique(pefdata[pefdata$delstatus_t==1,'loanid'])),]
      nondefaulted<-sqldf("select *,min(reportingperiod_t) as periodstart,max(delstatus_t)<>'0' as def,count(*) as duration from nondefaulted group by loanid")
    
    defaulted <- pefdata[pefdata$loanid %in% unique(pefdata[pefdata$delstatus_t==1,'loanid']),]
      defaulted <- sqldf("select pd1.* from defaulted as pd1 
                                JOIN 
                                (select min(reportingperiod_t) as end,loanid from defaulted where delstatus_t==1 group by loanid) as pd2
                                ON pd1.reportingperiod_t<=pd2.end AND pd1.loanid = pd2.loanid")
      defaulted<-sqldf("select *,min(reportingperiod_t) as periodstart,max(delstatus_t)<>'0' as def,count(*) as duration from defaulted group by loanid")
      
    pefdata <- rbind(defaulted,nondefaulted)  
      # first_ne$start <-as.Date(first_ne$start,origin="1970-01-01")
    # first_ne$end <-as.Date(first_ne$end,origin="1970-01-01")
    # temp <- merge(pefdata,first_ne,by=c('loanid'),all.x = TRUE)
    # temp <- temp[temp$reportingperiod_t>=temp$start & temp$reportingperiod_t<=temp$end,]
    
    if(i==1)  {
      write.table(t(names(pefdata)),file="PMT_vs_Rent_freddie_cox_sample_Sep292016_0pct.csv",sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)
    }
    write.table(pefdata,file="PMT_vs_Rent_freddie_cox_sample_Sep292016_0pct.csv",append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE,na="")
    cat(nrow(pefdata),sep="-")
    i= i+1
    gc()
  },error=function(e){
    cat(fn,"Skipped\n")
  })
}


