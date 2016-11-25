rm(list=ls())
library(data.table)
library(zoo)
library(sqldf)
library(plyr)


setwd("C:/Dim/strategic_default/")
source("functions.R")

fn_cox = "self_cure_sample_Nov242016.csv"

# Source: http://www.zillow.com/research/data/#rental-data -> Median Rent List Price ($), Single-Family Residence
rent <- read.csv("Zip_MedianRentalPrice_Sfr.csv") 
rent <- data.frame(rent[1], stack(rent[7:ncol(rent)]))
rent['ind']<- as.Date(paste(substr(rent$ind,2,5),substr(rent$ind,7,8),"01",sep = "-"))
rent['ind'] <- as.double(as.yearqtr(rent$ind))
names(rent)<-c("zip","rent","ne_qt")
rent['zip']<-(rent$zip %/% 100)*100
rent <- ddply(rent,.(zip,ne_qt),summarise,rent=median(rent,na.rm = TRUE))
zipcodes <- unique(rent$zip)

allloans <- readRDS("C:/Dim/strategic_default/Freddie_Origination/freddie_orgination_data_upto_Q32015.rds")
allloans <- allloans[allloans$zip %in% unique(rent$zip),]
loanids <- unique(allloans$loanid)
allloans <- data.table(allloans)
setkeyv(allloans,'loanid')


# Source: http://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx#mpo -> Three-Digit ZIP Codes (Developmental Index; Not Seasonally Adjusted)   
hpi <- read.csv("HPI_AT_3zip.csv")
hpi$zip <- hpi$zip*100
hpi['current_q'] <- as.double(as.yearqtr(as.Date(paste(hpi$year,hpi$qt*3,"01",sep="-"))))
hpi$qt <- NULL
hpi$year <- NULL
names(hpi) <- c('zip','current_hpi','current_q')
hpiorg <- hpi
hpi <- data.table(hpi)
setkeyv(hpi,c('current_q','zip'))
names(hpiorg) <- c('zip','org_hpi','org_q')
hpiorg <- data.table(hpiorg)
setkeyv(hpiorg,c('org_q','zip'))

unemp <- readRDS("zipunemp.rds")
names(unemp)<-c("reportingperiod_t","zip","del_unemp")
unemp['ne_qt'] <- as.double(as.yearqtr(unemp$reportingperiod_t))
unemp$reportingperiod_t <- NULL

# Source: https://www.ffiec.gov/hmda/hmdaflat.htm and seach: Mortgage Lending Assessment Data File, 200X in https://catalog.archives.gov/
lardata <-readRDS("censusdata_zip_Aug232016.rds")
lardata$zip <- lardata$zip*100
medgrossrent <- ddply(lardata,.(zip,year),summarise,medgrossrent=mean(medgrossrent,na.rm = TRUE))


setwd("C:/Dim/strategic_default/Freddie_Performance")
filelist = list.files(pattern = ".*.txt")
i=1
for(fn in filelist) {
  tryCatch({
    cat("\n File",fn,"-",i,"\n")
    pefdata <- read.table(fn, sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE)
    names(pefdata)<-c("loanid","reportingperiod_t","currentupb_t","delstatus_t","age_t","monthstomaturity_t","repurchase_t","modification_t","zbcode_t","zbdate_t","interestrate_t","differedupb_t","lastduedate_t","insrecovery_t","salesproceeds_t","noninsrecovery_t","expenses_t","legal_cost","main_cost","tax_n_insurance","miss_ins","act_loss")    
    pefdata <- pefdata[pefdata$loanid %in% loanids,]
    pefdata$reportingperiod_t <- as.Date(paste(substr(pefdata$reportingperiod_t,1,4),"-",substr(pefdata$reportingperiod_t,5,6),"-01",sep=""))
    pefdata <- pefdata[pefdata$reportingperiod_t>='2010-01-01',]
    
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
    
    lids <- unique(pefdata[pefdata$delstatus_t==2,]$loanid)
    lids <- lids[!is.na(lids)]
    pefdata <- pefdata[pefdata$loanid %in% lids, ]
    
    def2loans <- pefdata[pefdata$delstatus_t==2,]
    def2loans['report_year'] <-format(def2loans$reportingperiod_t,"%Y")
    def2loans <- def2loans[!duplicated(def2loans[c("loanid","report_year")]),]
    def2loans <- def2loans[is.na(def2loans$zbcode_t),]
    def2loans <- def2loans[!is.na(def2loans$loanid),]
    
    selfcure <- sqldf(paste("select pd1.* from pefdata as pd1 
                            JOIN
                            (select loanid,reportingperiod_t as start,reportingperiod_t+366 as end from def2loans) as pd2 ON pd1.loanid=pd2.loanid AND pd1.reportingperiod_t>=pd2.start AND pd1.reportingperiod_t <= pd2.end where pd1.delstatus_t in (0)",sep=""))
    
    selfcure <- selfcure[,c("loanid","reportingperiod_t","age_t","delstatus_t")]
    names(selfcure)<- c("loanid","sc_date","sc_age","selfcure")
    selfcure['rep_year']<-format(selfcure$sc_date,"%Y")
    selfcure <- selfcure[!duplicated(selfcure[c("loanid","rep_year")]),]
    
    def2loans <- merge(def2loans,selfcure,by="loanid",all.x = TRUE)
    def2loans['selfcure'] <- ifelse(is.na(def2loans$selfcure),1,def2loans$selfcure)
    def2loans$selfcure = ifelse(def2loans$selfcure==1,0,1)
    
    def2loans$sc_age <- ifelse(is.na(def2loans$sc_age),def2loans$age_t+1,def2loans$sc_age)
    def2loans['agediff'] <- def2loans$sc_age - def2loans$age_t    
    def2loans <- def2loans[def2loans$agediff<=12,]
    def2loans <- def2loans[def2loans$agediff>0,]
    def2loans <- def2loans[!is.na(def2loans$loanid),]
    def2loans <- def2loans[!duplicated(def2loans[c("loanid","reportingperiod_t")]),]
    def2loans$sc_age<-NULL
    def2loans$report_year<-NULL
    def2loans$rep_year<-NULL
    def2loans['duration'] <- def2loans$agediff
    def2loans$monthstomaturity_t <- NULL
    def2loans$modification_t <- NULL
    def2loans$zbcode_t <- NULL
    def2loans$channel <- NULL
    def2loans$seller <- NULL
    def2loans$duration <- ifelse(def2loans$selfcure==0,NA,def2loans$duration)
    
    
    if(i==1)  {
      write.table(t(names(def2loans)),file=fn_cox,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)
    }
    write.table(def2loans,file=fn_cox,append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE,na="")
    cat(nrow(def2loans),sep="-")
    i= i+1
    gc()
  },error=function(e){
    cat(fn,"Skipped\n")
  })
}

rm(allloans)
pefdata <- read.csv(fn_cox,sep = "|",header = TRUE)

setwd("C:/Dim/strategic_default")

judicial=c('CT','DE','FL','IL','IN','IA','KS','KY','LA','MA','MD','ME','NJ','NM','NY','ND','OH','OK','PA','SC','SD','VT','WI')
nonrecourse=c('AK','AZ','CA','IA','MN','MT','NC','ND','OR','WA','WI')

pefdata['judicial']<-ifelse(pefdata$state %in% judicial,1,0)
pefdata['recourse']<-ifelse(pefdata$state %in% nonrecourse,0,1)


unemp <- readRDS("zipunemp.rds")
names(unemp)<-c("reportingperiod_t","zip","del_unemp")
pefdata$reportingperiod_t <- as.Date(pefdata$reportingperiod_t)
pefdata <- merge(pefdata,unemp,by=c("reportingperiod_t","zip"),all.x = TRUE)
unemp<-NULL

lardata <-readRDS("censusdata_zip_Aug232016.rds")
lardata$zip <- lardata$zip*100

pefdata['year']<-format(as.Date(pefdata$orgdate),'%Y')
pefdata <- merge(pefdata,lardata,by=c("year","zip"),all.x = TRUE)

pefdata$current_equity <- ifelse(is.infinite(pefdata$current_equity),NA,pefdata$current_equity)
source("C:/R/functions.R")
pefdata['mpay']<-apply(pefdata[,c('upb','interestrate','loanterm')],1,function(x) mortgage(x['upb'],x['interestrate'],x['loanterm']))



pefdata['ne_qt']<-as.double(as.yearqtr(pefdata$reportingperiod_t))
pefdata <- merge(pefdata,rent,by=c("ne_qt","zip"))

med_housevalue <- ddply(pefdata,.(reportingperiod_t,zip),summarise,med_housevalue=median(current_housevalue,na.rm = TRUE))
pefdata <- merge(pefdata,med_housevalue,by=c("reportingperiod_t","zip"))
pefdata['adjusted_rent']<- pefdata$rent*pefdata$current_housevalue/pefdata$med_housevalue
pefdata['adjusted_mpay_rent']<-pefdata$mpay/pefdata$adjusted_rent

saveRDS(pefdata,file="self_cure_sample_Nov242016.rds")

