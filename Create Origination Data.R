rm(list=ls())
setwd("C:/Dim/strategic_default/Freddie_Origination")
org_file_name = "freddie_orgination_data_upto_Q32015.csv"

filelist = list.files(pattern = ".*.txt")
i=1
for (fn in filelist)  {
  cat("\n File",fn,"-","\n")
  org_data <- read.table(fn, sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE)
  org_data['org_qt']<-substr(fn,19,23)
  names(org_data) <- c("fico","firstpmtdate","firsttimebuyer","maturitydate","msa","pmi","noofunits","occupancy","cltv","dti",
                       "upb","ltv","interestrate","channel","prepaypanelty","mtgtype","state","propertytype","zip","loanid",
                       "purpose","loanterm","noofborrowers","seller","servicer","superconforming",'org_qt')
  
  
  if(i==1)  {
    write.table(t(names(org_data)),file=org_file_name,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE)
  }
  write.table(org_data,file=org_file_name,append=TRUE,sep="|",col.names=FALSE,quote=FALSE,row.names = FALSE,na="")
  i=i+1
}


org_data <- read.table(file = org_file_name,sep = "|",quote = "", row.names = NULL, stringsAsFactors = FALSE,header = TRUE)
org_data$firstpmtdate <- as.Date(paste(substr(org_data$firstpmtdate,1,4),"-",substr(org_data$firstpmtdate,5,6),"-01",sep=""))
org_data$maturitydate <- as.Date(paste(substr(org_data$maturitydate,1,4),"-",substr(org_data$maturitydate,5,6),"-01",sep=""))
org_data['orgdate']<- as.Date(paste(substr(org_data$org_qt,2,5),"-",as.numeric(substr(org_data$org_qt,1,1))*3,"-01",sep=""))
org_data$org_qt <- NULL
saveRDS(org_data,file="freddie_orgination_data_upto_Q32015.rds")
