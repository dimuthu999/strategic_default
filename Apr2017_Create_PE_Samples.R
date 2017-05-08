rm(list=ls())
library(data.table)
library(zoo)
library(sqldf)
library(plyr)
library(lubridate)



setwd("C:/Dim/strategic_default/")
source("functions.R")

fn_cox = "temp_cox_strategicdefault_new2.csv"

# Source: http://www.zillow.com/research/data/#rental-data -> Median Rent List Price ($), Single-Family Residence
rent <- read.csv("Zip_MedianRentalPrice_Sfr.csv") 
rent <- data.frame(rent[1], stack(rent[7:ncol(rent)]))
rent$ind <- as.character(rent$ind)
rent['month']<- as.Date(paste(substr(rent$ind,2,5),substr(rent$ind,7,8),"01",sep = "-"))
rent$ind <- NULL
names(rent)<-c("zip","rent","month")
rent['zip']<-(rent$zip %/% 100)*100
rent <- ddply(rent,.(zip,month),summarise,rent=median(rent,na.rm = TRUE))




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
      pefdata <- readRDS(file = paste(substr(fn,1,nchar(fn)-4),".rds",sep = ""))
      pefdata$delstatus_t <- ifelse(pefdata$delstatus_t=="1","0",pefdata$delstatus_t)
      pefdata$delstatus_t <- ifelse(pefdata$delstatus_t=="2","1",pefdata$delstatus_t)
      
      lids <- unique(pefdata[pefdata$current_equity<=(ne_cut-0.1) & pefdata$current_equity>= (ne_cut-0.2),]$loanid)
      
      
      lids <- lids[!is.na(lids)]
      pefdata <- pefdata[pefdata$loanid %in% lids, ]
      pefdata$cd <- pefdata$current_date
      
      
      # lids <- sqldf(paste("select loanid,min(cd) as start from pefdata where current_equity<=",ne_cut," ",sep=""))
      lids <- sqldf(paste("select pd1.loanid,pd1.delstatus_t from pefdata as pd1 JOIN (select loanid,min(cd) as start from pefdata where current_equity<=",(ne_cut-0.1)," AND current_equity>=",(ne_cut-0.2)," group by loanid) as pd2 ON pd1.loanid=pd2.loanid AND pd1.cd=pd2.start",sep=""))
      lids <- lids[lids$delstatus_t %in% c(0),]
      
      pefdata<-pefdata[pefdata$loanid %in% lids$loanid,]

     
      pefdata <- sqldf(paste("select pd1.* from pefdata as pd1 
                             JOIN
                             (select loanid,min(cd)-10 as start from pefdata where current_equity<=",(ne_cut-0.1)," AND current_equity>=",(ne_cut-0.2)," group by loanid) as pd2 ON pd1.loanid=pd2.loanid AND pd1.cd>=pd2.start",sep=""))
          
      # pefdata <- pefdata[pefdata$loanid=="F105Q1164709",]
      
      lids <- sqldf(paste("select pd1.loanid,pd1.delstatus_t from pefdata as pd1 JOIN (select loanid,min(cd) as start from pefdata where current_equity>=",(ne_cut)," AND current_equity<=",(ne_cut+0.1)," group by loanid) as pd2 ON pd1.loanid=pd2.loanid AND pd1.cd=pd2.start",sep=""))
      lids <- lids[lids$delstatus_t %in% c(0),]
      
      pefdata<-pefdata[pefdata$loanid %in% lids$loanid,]
      
      
      pefdata <- sqldf(paste("select pd1.* from pefdata as pd1 
                             JOIN
                             (select loanid,min(cd)-10 as start,min(cd)+366 as end from pefdata where current_equity>=",ne_cut," AND current_equity<=",(ne_cut+0.1)," group by loanid) as pd2 ON pd1.loanid=pd2.loanid AND pd1.cd>=pd2.start AND pd1.cd <= pd2.end where pd1.delstatus_t in (0,1)",sep=""))
      
      
      
      nondefaulted <- pefdata[!(pefdata$loanid %in% unique(pefdata[pefdata$delstatus_t==1,'loanid'])),]
      prepaid <- unique(nondefaulted[nondefaulted$zbcode_t==1,]$loanid)
      nondefaulted<-sqldf("select *,min(cd) as periodstart,max(delstatus_t)<>'0' as def,count(*) as duration from nondefaulted group by loanid")
      nondefaulted['prepaid'] <- ifelse(nondefaulted$loanid %in% prepaid,1,0)

      
      defaulted <- pefdata[pefdata$loanid %in% unique(pefdata[pefdata$delstatus_t==1,'loanid']),]
      defaulted <- sqldf("select pd1.* from defaulted as pd1 
                         JOIN 
                         (select min(cd) as end,loanid from defaulted where delstatus_t==1 group by loanid) as pd2
                         ON pd1.cd<=pd2.end AND pd1.loanid = pd2.loanid")
      defaulted<-sqldf("select *,min(cd) as periodstart,max(delstatus_t)<>'0' as def,count(*) as duration from defaulted group by loanid")
      if(nrow(defaulted)>0) defaulted['prepaid'] <- 0
      
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
      cat(e,"Skipped\n")
    })
  }
  
  gc(reset = TRUE)
  
  pefdata <- read.table(fn_cox,sep = "|",row.names = NULL,header = TRUE)
  pefdata <- pefdata[pefdata$current_equity>=ne_cut & pefdata$current_equity<ne_cut+0.1,]
 
  pefdata['mpay']<-apply(pefdata[,c('upb','interestrate')],1,function(x) mortgage(x['upb'],x['interestrate'],360))
  pefdata <- pefdata[pefdata$mpay>=quantile(pefdata$mpay,0.01,na.rm = TRUE) & pefdata$mpay<=quantile(pefdata$mpay,0.99,na.rm = TRUE),]
  
  pefdata$current_date <- as.Date(as.character(pefdata$current_date))
  pefdata$orgdate <- as.Date(as.character(pefdata$orgdate))
  pefdata$loanid <- as.character(pefdata$loanid)
  pefdata$cd <- NULL
  pefdata$state <- as.character(pefdata$state)

  
  pefdata <- merge(pefdata,rent,by.x=c("current_date","zip"),by.y = c("month","zip"),all.x = TRUE)
  pefdata <- pefdata[as.numeric(pefdata$current_date - pefdata$orgdate)>300,]

  pefdata['ne_cutoff']<-ne_cut
  
  fn_cox_out = paste("cox_ne_",ne_cut,"_Apr282017_60day.rds",sep="")
  saveRDS(pefdata,file=fn_cox_out)
  
}

cuts <- c(0.1,0.3,0.5,0.7)

for(c in cuts) {
  generate_sample(c)
}

