rm(list=ls())
setwd("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay")

rent <- read.csv("zillowrent.csv")
rent['zip']<-(rent$RegionName %/% 100)*100
zillow_zips <- unique(rent$zip)
rent <- NULL

allloans <- readRDS("CombinedFreddieFannieAct_Nov18.rds")
allloans <- allloans[allloans$gse=="freddie",]
loanids <- allloans[allloans$zip %in% zillow_zips,'loanid']
allloans <- NULL

setwd("C:/Dim/Freddie Per")
gc()
filelist = list.files(pattern = ".*.txt")


i=1
skipped = 0
colnames <- c("loanid","reportingperiod_t","currentupb_t","delstatus_t","age_t","monthstomaturity_t","zbcode_t","interestrate_t")
write.table(t(colnames),file="FreddiePrePaySample_Sep232016_loanmonth.csv",append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)

for(fn in filelist) {
  cat(fn,i,"\n")
  i=i+1
  pefdata <- read.table(fn, sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE) 
  names(pefdata)<-c("loanid","reportingperiod_t","currentupb_t","delstatus_t","age_t","monthstomaturity_t","repurchase_t","modification_t","zbcode_t","zbdate_t","interestrate_t","differedupb_t","lastduedate_t","insrecovery_t","salesproceeds_t","noninsrecovery_t","expenses_t")
  pefdata <- pefdata[pefdata$loanid %in% loanids,]
  pefdata$reportingperiod_t <- as.Date(paste(substr(pefdata$reportingperiod_t,1,4),"-",substr(pefdata$reportingperiod_t,5,6),"-01",sep=""))
  pefdata <- pefdata[pefdata$reportingperiod_t>='2010-01-01',]
  pefdata <- pefdata[,c("loanid","reportingperiod_t","currentupb_t","delstatus_t","age_t","monthstomaturity_t","zbcode_t","interestrate_t")]
  write.table(pefdata,file="FreddiePrePaySample_Sep232016_loanmonth.csv",append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)
}

rm(list=ls())
require(data.table)
require(zoo)


# setwd("C:/Dim/Freddie Per")
# pefdata <- read.csv("FreddiePrePaySample_Sep232016_loanmonth.csv",sep = "|",stringsAsFactors = FALSE)
# setwd("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay")
# saveRDS(pefdata,file = "FreddiePrePaySample_Sep232016_loanmonth.rds")

setwd("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay")
pefdata <- readRDS("FreddiePrePaySample_Sep232016_loanmonth.rds")

loanids <- unique(pefdata$loanid)
loanids <- sample(loanids,5e5,replace=FALSE)
pefdata <- pefdata[pefdata$loanid %in% loanids,]
pefdata <- data.table(pefdata)
setkeyv(pefdata,'loanid')

allloans <- readRDS("CombinedFreddieFannieAct_Nov18.rds")
allloans <- allloans[(allloans$gse=="freddie"),]
allloans <- data.table(allloans)
setkeyv(allloans,'loanid')

pefdata <- merge(pefdata,allloans,by=c("loanid"))

pefdata$reportingperiod_t <- as.Date(pefdata$reportingperiod_t)
rm(allloans)


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


pefdata <- as.data.frame(pefdata)
pefdata['current_q']<- as.yearqtr(((pefdata$reportingperiod_t)))
pefdata['org_q']<-as.yearqtr(pefdata$orgdate)
pefdata$cltv <- ifelse(pefdata$cltv>pefdata$ltv,pefdata$cltv,pefdata$ltv)
pefdata$cltv <- ifelse(is.na(pefdata$cltv),pefdata$ltv,pefdata$cltv)
pefdata <- data.table(pefdata)
setkeyv(pefdata,c('loanid','current_q','org_q'))
pefdata <- merge(pefdata,hpi, by=c('current_q','zip'),all.x = TRUE)
pefdata$current_q<-NULL
pefdata <- merge(pefdata,hpiorg, by=c('org_q','zip'),all.x = TRUE)
pefdata$org_q<-NULL
pefdata <- as.data.frame(pefdata)
pefdata['prepaid'] <- ifelse(pefdata$zbcode_t==1,1,0)
pefdata['prepaid'] <- ifelse(is.na(pefdata$zbcode_t),0,pefdata$prepaid)
pefdata['current_housevalue'] <- (pefdata$upb*100/pefdata$ltv)*(pefdata$current_hpi)/pefdata$org_hpi
pefdata['current_equity'] <- (pefdata$current_housevalue - (pefdata$currentupb_t*pefdata$cltv/pefdata$ltv))/(pefdata$current_housevalue)

prepaid <- pefdata[pefdata$prepaid==1,]
prepaid$age_t <- prepaid$age_t-1
prepaid <- prepaid[,c("loanid","age_t")]
prepaid <- merge(prepaid,pefdata,by=c("loanid","age_t"),all.x = TRUE)
prepaid <- prepaid[,c("loanid","age_t","current_equity")]
prepaid$age_t <- prepaid$age_t+1
names(prepaid) <- c("loanid","age_t","current_equity_l")
pefdata <- merge(pefdata,prepaid,by=c("loanid","age_t"),all.x = TRUE)
pefdata$current_equity <- ifelse(pefdata$prepaid==1,pefdata$current_equity_l,pefdata$current_equity)
pefdata$current_equity_l <- NULL
saveRDS(pefdata,file="prepayment_loanmonth.rds")



##################
rm(list=ls())
setwd("C:/Dim/Freddie Per")
require(data.table)
prepaid <- read.csv("FreddiePrePaySample_Sep192016.csv",sep="|")
prepaid <- prepaid[,c('loanid','reportingperiod_t','age_t','monthstomaturity_t','currentupb_t')]
names(prepaid) <- c('loanid','prepayment_month','prepayment_age','months_left_at_prepayment','amount_prepaid')
prepaid$prepayment_month <- as.Date(paste(substr(prepaid$prepayment_month,1,4),"-",substr(prepaid$prepayment_month,5,6),"-01",sep=""))

setwd("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay")
allloans <- readRDS("CombinedFreddieFannieAct_Nov18.rds")
allloans <- allloans[allloans$gse=="freddie",]

prepaid <- data.table(prepaid)
setkeyv(prepaid,c('loanid'))

allloans <- data.table(allloans)
setkeyv(allloans,c('loanid'))

allloans<-merge(allloans,prepaid,by=c("loanid"),all.x = TRUE)
allloans <- as.data.frame(allloans)
allloans['prepaid'] <- ifelse(is.na(allloans$prepayment_age),0,1)

rent <- read.csv("zillowrent.csv")
rent['zip']<-(rent$RegionName %/% 100)*100
rent$RegionName<-NULL
rent['rent']<- rent$values
rent$values<-NULL
rent$City<-NULL
rent$Metro <- NULL
rent$State <-NULL
rent$CountyName <-NULL
rent <- rent[!is.na(rent$rent),]

allloans <- merge(allloans,rent,by=c("prepayment_month","zip"))

# deldata['mpay']<-apply(deldata[,c('upb','interestrate','loanterm')],1,function(x) mortgage(x['upb'],x['interestrate'],x['loanterm']))
# 
saveRDS(allloans,file="freddie_prepayment.rds")

# for(fn in filelist) {
#   cat(fn,i,"\n")
#   i=i+1
#   pefdata <- read.table(fn, sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE) 
#   names(pefdata)<-c("loanid","reportingperiod_t","currentupb_t","delstatus_t","age_t","monthstomaturity_t","repurchase_t","modification_t","zbcode_t","zbdate_t","interestrate_t","differedupb_t","lastduedate_t","insrecovery_t","salesproceeds_t","noninsrecovery_t","expenses_t")
#   zbloanid <- pefdata[pefdata$zbcode_t %in% c(1),'loanid']
#   pefdata <- pefdata[pefdata$loanid %in% zbloanid,]
#   
#   j=1
#   for(loan in zbloanid) {
#     cat("File",i,"Loan",j,"\n")
#     tryCatch({
#       temp <- pefdata[pefdata$loanid==loan,]  
#       temp <- temp[order(temp$reportingperiod_t),]
#       
#       
#       temp$reportingperiod_t <- as.Date(paste(substr(temp$reportingperiod_t,1,4),"-",substr(temp$reportingperiod_t,5,6),"-01",sep=""))
#       
#       record <- temp[nrow(temp),]
#       record['modified']<- "Y" %in% temp[,'modification_t']
#       record['max_pastdelinq'] <- max(temp[,'delstatus_t'],na.rm = TRUE)
#       record$currentupb_t = temp[nrow(temp)-1,'currentupb_t']
#       if(record$currentupb_t<=0) min(temp[temp$currentupb_t>0,'currentupb_t'])
#       write.table(record,file="FreddiePrePaySample_Sep192016.csv",append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)
#       j=j+1
#     },error=function(e){
#       cat("Skipped\n")
#       skipped = skipped + 1
#     })
#   }
#   
#   
# }


####################################
rm(list=ls())
setwd("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Loss Local")
source("C:/R/functions.R")
gc()


zbloans <- read.csv("FreddieZeroBalanceSample_Nov17.csv",sep="|")
zbloans$salesproceeds_t <- as.character(zbloans$salesproceeds_t)

zbloans$salesproceeds_t <- ifelse(zbloans$salesproceeds_t %in% c("C"),"-99999",zbloans$salesproceeds_t)
zbloans$salesproceeds_t <- as.numeric(zbloans$salesproceeds_t)
zbloans$salesproceeds_t <- abs(zbloans$salesproceeds_t)


zbloans$reportingperiod_t <- as.Date(zbloans$reportingperiod_t)
zbloans$lastduedate_t <- as.Date(zbloans$lastduedate_t)
zbloans['timetoforc'] <- zbloans$reportingperiod_t - zbloans$lastduedate_t
zbloans['zbyear'] <- format(zbloans$reportingperiod_t, format="%Y")

# loans<-readRDS("FreddieallAcqDataAug302015.rds")
# lids <-unique(zbloans$loanid)
# loans <-loans[loans$loanid %in% lids,]
# zbloans<-merge(zbloans,loans,by="loanid")


zbloans['delinterest']<- (as.numeric(zbloans$reportingperiod_t-zbloans$lastduedate_t)/30)*zbloans$currentupb_t*(30/360)*zbloans$interestrate_t/100
zbloans$delinterest[is.na(zbloans$delinterest)]<-0
zbloans<-zbloans[!(zbloans$salesproceeds_t %in% c("", " ", "  ",NA)),]
zbloans['loss']<-ifelse(zbloans$salesproceeds_t==-99999,0,(zbloans$currentupb_t-zbloans$salesproceeds_t+abs(zbloans$expenses_t)-zbloans$insrecovery_t-abs(zbloans$noninsrecovery_t)+zbloans$delinterest))
zbloans['lossnoinsurance']<-ifelse(zbloans$salesproceeds_t==-99999,0,(zbloans$currentupb_t-zbloans$salesproceeds_t-zbloans$expenses_t-zbloans$noninsrecovery_t+zbloans$delinterest))

zbloans<-zbloans[!is.na(zbloans$loanid),]
zbloans$loss <- ifelse((zbloans$loss<0),0,zbloans$loss)
#zbloans$loss <- ifelse((zbloans$salesproceeds_t<0),0,zbloans$loss)

keeps <- c('loanid','reportingperiod_t','currentupb_t','delstatus_t','age_t','monthstomaturity_t','modification_t','zbcode_t','zbdate_t','interestrate_t','lastduedate_t','modified','pastdelinq','timetoforc','zbyear','loss','lossnoinsurance')
zbloans<-zbloans[,names(zbloans) %in% keeps]
zbloans<- zbloans[,order(names(zbloans))]
zbloans$zbdate_t <- as.Date(paste(substr(zbloans$zbdate_t,1,4),"-",substr(zbloans$zbdate_t,5,6),"-01",sep=""))
zbfactor <- sapply(zbloans, is.factor)
zbloans[zbfactor] <- lapply(zbloans[zbfactor], as.character)

saveRDS(zbloans,"FreddieZBloansCommon_Nov21.rds")
# meansum(zbloans,c('state','zbyear'),item)
# forctimeyr <- meansdmedsum2(zbloans,c('state','zbyear'),'timetoforc')
