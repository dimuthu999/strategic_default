rm(list=ls())
library(data.table)
library(zoo)
library(sqldf)
library(plyr)
source("C:/R/functions.R")

setwd("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay")
fn_cox = "PMT_vs_Rent_freddie_cox_sample_Oct142016_0pct.csv"
fn_loanmonth = "PMT_vs_Rent_freddie_dynamiclogit_sample_Oct142016_0pct.csv"


#common columns in both Freddie and Fannie
colnames <-c('loanid','reportingperiod_t','currentupb_t','delstatus_t','age_t','monthstomaturity_t','modification_t','zbcode_t','interestrate_t')

allloans <- readRDS("CombinedFreddieFannieAct_Nov18.rds")
nonrecourse=c('AK','AZ','CA','IA','MN','MT','NC','ND','OR','WA','WI')
#allloans <- allloans[(allloans$state %in% nonrecourse) & (allloans$gse=="freddie"),]
allloans <- allloans[(allloans$gse=="freddie"),]
loanids <- as.vector(allloans$loanid)

allloans <- data.table(allloans)
setkeyv(allloans,'loanid')

rent <- read.csv("zillowrent.csv")
rent['zip']<-(rent$RegionName %/% 100)*100
zipcodes <- unique(rent$zip)

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

generate_sample <- function(cut)  {

  gc()
  
  ne_cut= cut
  cat(ne_cut,"\n")
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
      pefdata <- pefdata[pefdata$reportingperiod_t>='2010-01-01',]
      
      pefdata['current_q']<- as.yearqtr(((pefdata$reportingperiod_t)))
      
      pefdata <- data.table(pefdata)
      setkeyv(pefdata,c('loanid','current_q'))
      
      pefdata<-merge(pefdata,allloans,by=c("loanid"),all.x = TRUE)
      
      
      pefdata <- merge(pefdata,hpi, by=c('current_q','zip'),all.x = TRUE)
      pefdata$current_q<-NULL
      
      pefdata <- as.data.frame(pefdata)
      pefdata <- pefdata[pefdata$zip %in% zipcodes,]
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
      
      lids <- unique(pefdata[pefdata$current_equity<=ne_cut,]$loanid)
      lids <- lids[!is.na(lids)]
      pefdata <- pefdata[pefdata$loanid %in% lids, ]
      
      lids <- sqldf(paste("select pd1.loanid,pd1.delstatus_t from pefdata as pd1 JOIN (select loanid,min(reportingperiod_t) as start from pefdata where current_equity<=",ne_cut," group by loanid) as pd2 ON pd1.loanid=pd2.loanid AND pd1.reportingperiod_t=pd2.start",sep=""))
      
      pefdata<-pefdata[pefdata$loanid %in% lids[lids$delstatus_t==0,'loanid'],]
      
      pefdata_lm <- sqldf(paste("select pd1.* from pefdata as pd1 JOIN (select loanid,min(reportingperiod_t) as start,min(reportingperiod_t)+366 as end from pefdata where current_equity<=",ne_cut,"  group by loanid) as pd2 ON pd1.loanid=pd2.loanid AND pd1.reportingperiod_t>=pd2.start",sep=""))
      
      pefdata <- sqldf(paste("select pd1.* from pefdata as pd1 
                       JOIN
                       (select loanid,min(reportingperiod_t) as start,min(reportingperiod_t)+366 as end from pefdata where current_equity<=",ne_cut," group by loanid) as pd2 ON pd1.loanid=pd2.loanid AND pd1.reportingperiod_t>=pd2.start AND pd1.reportingperiod_t <= pd2.end where pd1.delstatus_t in (0,1)",sep=""))
      
      nondefaulted <- pefdata[!(pefdata$loanid %in% unique(pefdata[pefdata$delstatus_t==1,'loanid'])),]
        nondefaulted<-sqldf("select *,min(reportingperiod_t) as periodstart,max(delstatus_t)<>'0' as def,count(*) as duration from nondefaulted group by loanid")
      
      defaulted <- pefdata[pefdata$loanid %in% unique(pefdata[pefdata$delstatus_t==1,'loanid']),]
        defaulted <- sqldf("select pd1.* from defaulted as pd1 
                                  JOIN 
                                  (select min(reportingperiod_t) as end,loanid from defaulted where delstatus_t==1 group by loanid) as pd2
                                  ON pd1.reportingperiod_t<=pd2.end AND pd1.loanid = pd2.loanid")
        defaulted<-sqldf("select *,min(reportingperiod_t) as periodstart,max(delstatus_t)<>'0' as def,count(*) as duration from defaulted group by loanid")
        
      pefdata <- rbind(defaulted,nondefaulted)  
      
      temp <- sqldf("select loanid,min(reportingperiod_t) as end,age_t as age_end from pefdata_lm where delstatus_t>0 group by loanid")
      
      pefdata_lm <- merge(pefdata_lm,temp,by=c("loanid"),all.x = TRUE)
      pefdata_lm <- pefdata_lm[order(pefdata_lm$loanid,pefdata_lm$reportingperiod_t),]
      pefdata_lm$age_end <- ifelse(is.na(pefdata_lm$age_end),1000000,pefdata_lm$age_end)
      pefdata_lm <- pefdata_lm[pefdata_lm$age_t>0,]
      pefdata_lm <- pefdata_lm[pefdata_lm$age_t <= pefdata_lm$age_end,]
      pefdata_lm <- pefdata_lm[is.na(pefdata_lm$zbcode_t),]
      
      if(i==1)  {
        write.table(t(names(pefdata)),file=fn_cox,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)
        write.table(t(names(pefdata_lm)),file=fn_loanmonth,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)
      }
      write.table(pefdata,file=fn_cox,append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE,na="")
      write.table(pefdata_lm,file=fn_loanmonth,append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE,na="")
      cat(nrow(pefdata),sep="-")
      i= i+1
      gc()
    },error=function(e){
      cat(fn,"Skipped\n")
    })
  }
  
  gc(reset = TRUE)
  
  pefdata <- read.csv(fn_cox,sep = "|",header = TRUE)
  
  setwd("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay")
  
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
  medgrossrent <- ddply(lardata,.(zip,year),summarise,medgrossrent=mean(medgrossrent,na.rm = TRUE))
  
  
  pefdata['year']<-format(as.Date(pefdata$orgdate),'%Y')
  pefdata <- merge(pefdata,lardata,by=c("year","zip"),all.x = TRUE)
  
  pefdata$current_equity <- ifelse(is.infinite(pefdata$current_equity),NA,pefdata$current_equity)
  
  pefdata['mpay']<-apply(pefdata[,c('upb','interestrate','loanterm')],1,function(x) mortgage(x['upb'],x['interestrate'],x['loanterm']))
  
  rent <- read.csv("zillowrent.csv")
  rent['reportingperiod_t']<-as.Date(rent$Month)
  rent['ne_year']<-format(rent$reportingperiod_t,"%Y")
  rent$Month<-NULL
  rent['zip']<-(rent$RegionName %/% 100)*100
  rent$RegionName<-NULL
  rent['rent']<- rent$values
  rent$values<-NULL
  rent$City<-NULL
  rent$Metro <- NULL
  rent$State <-NULL
  rent$CountyName <-NULL
  rent <- rent[!is.na(rent$rent),]
  rent <-ddply(rent,.(zip,ne_year),summarize,rent=median(rent,na.rm=TRUE))
  
  pefdata['ne_year']<-format(pefdata$reportingperiod_t,"%Y")
  pefdata <- merge(pefdata,rent,by=c("ne_year","zip"))
  #pefdata['ne_year']<-format(pefdata$reportingperiod_t,"%Y")
  
  med_housevalue <- ddply(pefdata,.(reportingperiod_t,zip),summarise,med_housevalue=median(current_housevalue,na.rm = TRUE))
  pefdata <- merge(pefdata,med_housevalue,by=c("reportingperiod_t","zip"))
  pefdata['adjusted_rent']<- pefdata$rent*pefdata$current_housevalue/pefdata$med_housevalue
  pefdata['adjusted_mpay_rent']<-pefdata$mpay/pefdata$adjusted_rent
  pefdata['ne_cutoff']<-ne_cut
  
  fn_cox_out = paste("cox_ne_",ne_cut,"_Oct142016.rds",sep="")
  saveRDS(pefdata,file=fn_cox_out)
  
  setwd("C:/Dim/Freddie Per")
  pefdata_lm <- read.csv(fn_loanmonth,sep = "|",header = TRUE)
  pefdata_lm$reportingperiod_t <- as.character(pefdata_lm$reportingperiod_t)
  setwd("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay")
  
  pefdata_lm['ne_year']<-as.integer(substr(pefdata_lm$reportingperiod_t,1,4))
  
  pefdata_lm <- merge(pefdata_lm,rent,by=c("ne_year","zip"))
  
  pefdata_lm <- pefdata_lm[order(pefdata_lm$loanid,pefdata_lm$age_t),]
  startage <- pefdata_lm[!duplicated(pefdata_lm$loanid),]
  startage <- startage[,c("loanid","age_t")]
  names(startage) <- c("loanid","age_1")
  pefdata_lm <- merge(pefdata_lm,startage,c("loanid"))
  pefdata_lm['age_since_ne']<- pefdata_lm$age_t-pefdata_lm$age_1
  
  pefdata_lm['mpay']<-apply(pefdata_lm[,c('upb','interestrate','loanterm')],1,function(x) mortgage(x['upb'],x['interestrate'],x['loanterm']))
  med_housevalue_lm <- ddply(pefdata_lm,.(reportingperiod_t,zip),summarise,med_housevalue=median(current_housevalue,na.rm = TRUE))
  pefdata_lm <- merge(pefdata_lm,med_housevalue_lm,by=c("reportingperiod_t","zip"))
  pefdata_lm['adjusted_rent']<- pefdata_lm$rent*pefdata_lm$current_housevalue/pefdata_lm$med_housevalue
  pefdata_lm['adjusted_mpay_rent']<-pefdata_lm$mpay/pefdata_lm$adjusted_rent
  pefdata_lm['ne_cutoff']<-ne_cut
  pefdata_lm$delstatus_t <- as.character(pefdata_lm$delstatus_t)
  pefdata_lm <- pefdata_lm[pefdata_lm$delstatus_t %in% c("0","1"),]
  pefdata_lm$delstatus_t <- as.factor(pefdata_lm$delstatus_t)
  fn_loanmonth_out = paste("loanmonth_ne_",ne_cut,"_Oct142016.rds",sep="")
  saveRDS(pefdata_lm,file=fn_loanmonth_out)
}

cuts <- c(0,-0.1,-0.2,-0.3,-0.4,-0.5,-0.6,-0.7)

for(c in cuts) {
  generate_sample(c)
}



# 
# cox<-list()
# cox[[1]]<-coxph(Surv(duration,def)~adjusted_mpay_rent+age_t+age_t*age_t+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = pefdata[pefdata$recourse==0 ,])
# cox[[2]]<-coxph(Surv(duration,def)~adjusted_mpay_rent+age_t+age_t*age_t+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = pefdata[pefdata$recourse==1 ,])
# stargazer(cox,column.labels=c('Non-Recourse-0','Recourse-0'), omit = c("state","ne_year","loanpurpose","occupancy"),omit.labels = c("State","Year","",""),type = "text",no.space=TRUE)



# logit <- glm(delstatus_t~adjusted_mpay_rent*current_equity+interestrate+poly(age_since_ne,5),family='binomial',data=pefdata_lm,maxit=100)
