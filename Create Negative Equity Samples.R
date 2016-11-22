rm(list=ls())
library(data.table)
library(zoo)
library(sqldf)
library(plyr)


setwd("C:/Dim/strategic_default/")
source("functions.R")

fn_cox = "temp_cox_strategicdefault.csv"
fn_loanmonth = "temp_loanmonth_strategicdefault.csv"

# Source: http://www.zillow.com/research/data/#rental-data -> Median Rent List Price ($), Single-Family Residence
rent <- read.csv("Zip_MedianRentalPrice_Sfr.csv") 
rent <- data.frame(rent[1], stack(rent[7:ncol(rent)]))
rent['ind']<- as.Date(paste(substr(rent$ind,2,5),substr(rent$ind,7,8),"01",sep = "-"))
rent['ind'] <- as.double(as.yearqtr(rent$ind))
names(rent)<-c("zip","rent","ne_qt")
rent['zip']<-(rent$zip %/% 100)*100
rent <- ddply(rent,.(zip,ne_qt),summarise,rent=median(rent,na.rm = TRUE))


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
pefdata$reportingperiod_t <- as.Date(pefdata$reportingperiod_t)
pefdata <- merge(pefdata,unemp,by=c("reportingperiod_t","zip"),all.x = TRUE)
unemp<-NULL

# Source: https://www.ffiec.gov/hmda/hmdaflat.htm and seach: Mortgage Lending Assessment Data File, 200X in https://catalog.archives.gov/
lardata <-readRDS("censusdata_zip_Aug232016.rds")
lardata$zip <- lardata$zip*100
medgrossrent <- ddply(lardata,.(zip,year),summarise,medgrossrent=mean(medgrossrent,na.rm = TRUE))



generate_sample <- function(cut)  {
  
  gc()
  ne_cut= cut
  cat(ne_cut,"\n")
  setwd("C:/Dim/strategic_default/Freddie_Performance")
  filelist = list.files(pattern = ".*.txt")
  i=1
  for(fn in filelist) {
    tryCatch({
      cat("\n File",fn,"-",i,"\n")
      pefdata <- read.table(fn, sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE)
      names(pefdata)<-c("loanid","reportingperiod_t","currentupb_t","delstatus_t","age_t","monthstomaturity_t","repurchase_t","modification_t","zbcode_t","zbdate_t","interestrate_t","differedupb_t","lastduedate_t","insrecovery_t","salesproceeds_t","noninsrecovery_t","expenses_t","legal_cost","main_cost","tax_n_insurance","miss_ins","act_loss")
      pefdata <- pefdata[pefdata$loanid %in% loanids,]
      modified_loanids <- unique(pefdata[pefdata$modification_t=="Y",]$loanid)
      pefdata <- pefdata[!pefdata$loanid %in% modified_loanids,]
      pefdata$reportingperiod_t <- as.Date(paste(substr(pefdata$reportingperiod_t,1,4),"-",substr(pefdata$reportingperiod_t,5,6),"-01",sep=""))
      pefdata <- pefdata[pefdata$reportingperiod_t>='2010-01-01',]
      
      pefdata['current_q']<- as.double(as.yearqtr(((pefdata$reportingperiod_t))))
      
      pefdata <- data.table(pefdata)
      setkeyv(pefdata,c('loanid','current_q'))
      
      pefdata<-merge(pefdata,allloans,by=c("loanid"),all.x = TRUE)
      
      
      pefdata <- merge(pefdata,hpi, by=c('current_q','zip'),all.x = TRUE)
      
      
      pefdata <- as.data.frame(pefdata)
      
      pefdata['org_q']<-as.double(as.yearqtr(pefdata$orgdate))
      pefdata <- data.table(pefdata)
      setkeyv(pefdata,c('loanid','org_q'))
      
      pefdata <- merge(pefdata,hpiorg, by=c('org_q','zip'),all.x = TRUE)

      pefdata$cltv <- ifelse(pefdata$cltv>pefdata$ltv,pefdata$cltv,pefdata$ltv)
      pefdata$cltv <- ifelse(is.na(pefdata$cltv),pefdata$ltv,pefdata$cltv)
      
      pefdata <- as.data.frame(pefdata)
      pefdata$current_q<-NULL
      pefdata$org_q<-NULL
      
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
      
      # generating loanmonth dataset when cut == 0
      if(cut==0)  {
          pefdata_lm <- sqldf(paste("select pd1.* from pefdata as pd1 
                                 JOIN
                                 (select *,min(reportingperiod_t) as start from pefdata where current_equity<=",ne_cut," group by loanid) as pd2 ON pd1.loanid=pd2.loanid AND pd1.reportingperiod_t>=pd2.start where pd1.delstatus_t in (0,1)",sep=""))
          
          pefdata_lm_default_lids <- unique(pefdata_lm[pefdata_lm$delstatus_t=="1",]$loanid)
          pefdata_lm_non_default <- pefdata_lm[!pefdata_lm$loanid %in% pefdata_lm_default_lids,]
          pefdata_lm_default <- pefdata_lm[pefdata_lm$loanid %in% pefdata_lm_default_lids,]
          temp <- pefdata_lm_default[pefdata_lm_default$delstatus_t=="1",]
          temp <- temp[!duplicated(temp$loanid),]
          temp <- temp[,c("loanid","age_t")]
          names(temp) <- c("loanid","end_age")
          pefdata_lm_default <- merge(pefdata_lm_default,temp, by="loanid")
          pefdata_lm_default <- pefdata_lm_default[pefdata_lm_default$age_t <= pefdata_lm_default$end_age,]
          pefdata_lm_default$end_age <-NULL
          pefdata_lm <- rbind(pefdata_lm_non_default,pefdata_lm_default)
      }
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
      
      
      if(i==1)  {
        write.table(t(names(pefdata)),file=fn_cox,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)
        if (cut==0) write.table(t(names(pefdata_lm)),file=fn_loanmonth,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)
      }
      write.table(pefdata,file=fn_cox,append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE,na="")
      if (cut==0) write.table(pefdata_lm,file=fn_loanmonth,append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE,na="")
      
      cat(nrow(pefdata),sep="-")
      i= i+1
      gc()
    },error=function(e){
      cat(fn,"Skipped\n")
    })
  }
  
  gc(reset = TRUE)
  
  pefdata <- read.csv(fn_cox,sep = "|",header = TRUE)
  
  #setwd("C:/Users/dnratnadiwakara/Documents/2016 Foreclosure Delay")
  
  judicial=c('CT','DE','FL','IL','IN','IA','KS','KY','LA','MA','MD','ME','NJ','NM','NY','ND','OH','OK','PA','SC','SD','VT','WI')
  nonrecourse=c('AK','AZ','CA','IA','MN','MT','NC','ND','OR','WA','WI')
  
  pefdata['judicial']<-ifelse(pefdata$state %in% judicial,1,0)
  pefdata['recourse']<-ifelse(pefdata$state %in% nonrecourse,0,1)
  
  pefdata['year']<-format(as.Date(pefdata$orgdate),'%Y')
  pefdata <- merge(pefdata,lardata,by=c("year","zip"),all.x = TRUE)
  
  pefdata$current_equity <- ifelse(is.infinite(pefdata$current_equity),NA,pefdata$current_equity)
  
  pefdata['mpay']<-apply(pefdata[,c('upb','interestrate','loanterm')],1,function(x) mortgage(x['upb'],x['interestrate'],x['loanterm']))
  
  pefdata['ne_qt']<-as.double(as.yearqtr(pefdata$reportingperiod_t))
  pefdata <- merge(pefdata,rent,by=c("ne_qt","zip"))
  #pefdata['ne_year']<-format(pefdata$reportingperiod_t,"%Y")
  
  med_housevalue <- ddply(pefdata,.(reportingperiod_t,zip),summarise,med_housevalue=median(current_housevalue,na.rm = TRUE))
  pefdata <- merge(pefdata,med_housevalue,by=c("reportingperiod_t","zip"))
  pefdata['adjusted_rent']<- pefdata$rent*pefdata$current_housevalue/pefdata$med_housevalue
  pefdata['adjusted_mpay_rent']<-pefdata$mpay/pefdata$adjusted_rent
  pefdata['ne_cutoff']<-ne_cut
  
  fn_cox_out = paste("cox_ne_",ne_cut,"_Oct142016.rds",sep="")
  saveRDS(pefdata,file=fn_cox_out)
  
}

cuts <- c(-0.1)

for(c in cuts) {
  generate_sample(c)
}


# edit loanmonth file which was created when cut==0
loanmonth <- read.csv(fn_loanmonth,sep = "|",header = TRUE)
loanmonth['ne_qt'] <- as.double(as.yearqtr(as.Date(as.character(loanmonth$reportingperiod_t))))
loanmonth <- as.data.table(loanmonth)
setkeyv(loanmonth,c('ne_qt','zip'))

rent <- as.data.table(rent)
setkeyv(rent,c('ne_qt','zip'))

loanmonth <- merge(loanmonth,rent,by=c("ne_qt","zip"))

med_housevalue <- ddply(loanmonth,.(ne_qt,zip),summarise,med_housevalue=median(current_housevalue,na.rm = TRUE))
med_housevalue <- as.data.table(med_housevalue)
setkeyv(med_housevalue,c('ne_qt','zip'))
loanmonth <- merge(loanmonth,med_housevalue,by=c("ne_qt","zip"))

loanmonth <- as.data.frame(loanmonth)
loanmonth['mpay']<-apply(loanmonth[,c('upb','interestrate','loanterm')],1,function(x) mortgage(x['upb'],x['interestrate'],x['loanterm']))
loanmonth['adjusted_rent']<- loanmonth$rent*loanmonth$current_housevalue/loanmonth$med_housevalue
loanmonth['adjusted_mpay_rent']<-loanmonth$mpay/loanmonth$adjusted_rent

loanmonth['current_year'] <- substr(as.character(loanmonth$reportingperiod_t),1,4)
loanmonth['org_year'] <- substr(as.character(loanmonth$orgdate),1,4)

saveRDS(loanmonth,file = "loanmonth_strategicdefault.rds")
