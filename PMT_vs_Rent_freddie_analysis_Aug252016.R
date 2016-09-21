
# Setting Up --------------------------------------------------------------


rm(list=ls())
library(data.table)
library(zoo)
library(sqldf)
library(stargazer)
library(survival)
library(plotly)
setwd("E:/strategic_default")
source("functions.R")

# temp<-read.table("PMT_vs_Rent_freddie_cox_sample_Aug23016_0pct.csv", sep = "|",header=TRUE,quote = "", row.names = NULL, stringsAsFactors = FALSE)
# temp['ne_cutoff']<-"zero"
# deldata<-temp
# 
# temp<-read.table("PMT_vs_Rent_freddie_cox_sample_Aug23016_20pct.csv", sep = "|",header=TRUE,quote = "", row.names = NULL, stringsAsFactors = FALSE)
# temp['ne_cutoff']<-"twenty"
# deldata<-rbind(deldata,temp)
# 
# temp<-read.table("PMT_vs_Rent_freddie_cox_sample_Aug23016_50pct.csv", sep = "|",header=TRUE,quote = "", row.names = NULL, stringsAsFactors = FALSE)
# temp['ne_cutoff']<-"fifty"
# deldata<-rbind(deldata,temp)
# 
# deldata['ne_year']<- format(as.Date(deldata$reportingperiod_t),'%Y')
# deldata['ne_date']<- as.Date(deldata$reportingperiod_t)
# 
# judicial=c('CT','DE','FL','IL','IN','IA','KS','KY','LA','MA','MD','ME','NJ','NM','NY','ND','OH','OK','PA','SC','SD','VT','WI')
# nonrecourse=c('AK','AZ','CA','IA','MN','MT','NC','ND','OR','WA','WI')
# 
# deldata['judicial']<-ifelse(deldata$state %in% judicial,1,0)
# deldata['recourse']<-ifelse(deldata$state %in% nonrecourse,0,1)
# 
# unemp <- readRDS("zipunemp.rds")
# names(unemp)<-c("reportingperiod_t","zip","del_unemp")
# deldata$reportingperiod_t <- as.Date(deldata$reportingperiod_t)
# deldata <- merge(deldata,unemp,by=c("reportingperiod_t","zip"),all.x = TRUE)
# unemp<-NULL
# 
# lardata <-readRDS("censusdata_zip_Aug232016.rds")
# lardata$zip <- lardata$zip*100
# medgrossrent <- ddply(lardata,.(zip,year),summarise,medgrossrent=mean(medgrossrent,na.rm = TRUE))
# 
# 
# deldata['year']<-format(as.Date(deldata$orgdate),'%Y')
# deldata <- merge(deldata,lardata,by=c("year","zip"),all.x = TRUE)
# 
# deldata$current_equity <- ifelse(is.infinite(deldata$current_equity),NA,deldata$current_equity)
# 
# deldata['mpay']<-apply(deldata[,c('upb','interestrate','loanterm')],1,function(x) mortgage(x['upb'],x['interestrate'],x['loanterm']))
# deldata['mpay_rent']<-deldata$mpay/deldata$medgrossrent
# deldata['mpay_std']<-(deldata$mpay - mean(deldata$mpay,na.rm=TRUE))/sd(deldata$mpay,na.rm = TRUE)
# 
# deldata['mpay_rent_q']<-ifelse(deldata$mpay_rent<quantile(deldata$mpay_rent,0.2,na.rm=TRUE),1,
#                                ifelse(deldata$mpay_rent<quantile(deldata$mpay_rent,0.4,na.rm=TRUE),2,
#                                       ifelse(deldata$mpay_rent<quantile(deldata$mpay_rent,0.6,na.rm=TRUE),3,
#                                              ifelse(deldata$mpay_rent<quantile(deldata$mpay_rent,0.8,na.rm=TRUE),4,
#                                                     ifelse(deldata$mpay_rent<max(deldata$mpay_rent,na.rm=TRUE),5,NA)))))
# 
# saveRDS(deldata,file="deldata.rds")

deldata <- readRDS("deldata.rds")

# PMT_RENT_ne_pct.pdf -----------------------------------------------------
loan_df_sum <- readRDS("loan_df_sum.rds")

pdf(file='PMT_RENT_ne_pct.pdf', width = 15, height = 8)
  plot(loan_df_sum$month, loan_df_sum$ne0_pct*100, type='n', xlab='Time', ylab='Percent',yaxt='n')
  lines(loan_df_sum$month, loan_df_sum$ne0_pct*100,col='red')
  lines(loan_df_sum$month, loan_df_sum$ne20_pct*100,col='black',lty=2)
  axis(side = 2, las = 1)
  legend("topleft",c("Pct equity < 0%","Pct equity < -20%"), lty=c(1,2),col=c('red','black'), bty = "n",horiz=T,lwd=1, cex = 0.8, xpd = TRUE)
dev.off()

# PMT_RENT_def_ne.pdf -------------------------------------------------------------------
#defpct=crosstab(deldata, row.vars = c("ne_cutoff","ne_year"), col.vars= c("def"),type = "row.pct")
defpct=ddply(deldata,.(ne_cutoff,ne_year),summarise,default=mean(def))
defpct <- defpct[defpct$ne_year<2014,]
temp1 <- defpct[defpct$ne_cutoff=="zero",c("ne_year","default")]
names(temp1)<-c("ne_year","zero")

temp2 <- defpct[defpct$ne_cutoff=="twenty",c("ne_year","default")]
names(temp2)<-c("ne_year","twenty")

temp3 <- defpct[defpct$ne_cutoff=="fifty",c("ne_year","default")]
names(temp3)<-c("ne_year","fifty")
temp3 <- temp3[temp3$ne_year>=2009,]

defpct <- merge(temp1,temp2,by=c("ne_year"))
defpct <- merge(defpct,temp3,by=c("ne_year"),all.x = TRUE)

pdf(file='PMT_RENT_def_ne.pdf', width = 15, height = 8)
  plot(defpct$ne_year, defpct$zero, type='n', xlab='Negative Equity Year', ylab='Default %',ylim=c(0,0.5),yaxt='n')
  lines(defpct$ne_year, defpct$zero,col='red')
  lines(defpct$ne_year, defpct$twenty,col='black',lty=2)
  lines(defpct$ne_year, defpct$fifty,col='blue',lty=4)
  axis(side = 2, las = 1)
  legend("topleft",c("Negative Equity","> 20% Negative Equity","> 50% Negative Equity"), lty=c(1,2,4),col=c('red','black','blue'), bty = "n",horiz=T,lwd=1, cex = 0.8, xpd = TRUE)
dev.off()

# PMT_RENT_hazardrates.pdf ---------------------------------------------------------------------


pdf(file='PMT_RENT_hazardrates.pdf', width = 15, height = 8)

par(mfrow=c(1,2))

temp <- deldata[deldata$ne_cutoff == "zero" & deldata$mpay_rent_q %in% c(1,5) & deldata$recourse==0,]
fit<-summary(survfit(Surv(temp$duration,temp$def)~temp$mpay_rent_q))
time <-fit$time[1:(length(fit$time)/2)]
lowmpay_rent <-fit$surv[1:(length(fit$time)/2)]
highpay_rent <-fit$surv[((length(fit$time)/2)+1):length(fit$time)]  
plot(time, lowmpay_rent, type='n', xlab='Time', ylab='Survival',ylim=c(0.8,1),yaxt='n',main = "Non-Recourse States")
lines(time, lowmpay_rent,col='red')
lines(time, highpay_rent,col='black',lty=2)
axis(side = 2, las = 1)
legend("topright",c("Low mpay_rent","High mpay_rent"), lty=c(1,2),col=c('red','black'), bty = "n",horiz=T,lwd=1, cex = 0.8, xpd = TRUE)

temp <- deldata[deldata$ne_cutoff == "zero" & deldata$mpay_rent_q %in% c(1,5) & deldata$recourse==1,]
fit<-survfit(Surv(temp$duration,temp$def)~temp$mpay_rent_q)
time <-fit$time[1:(length(fit$time)/2)]
lowmpay_rent <-fit$surv[1:(length(fit$time)/2)]
highpay_rent <-fit$surv[((length(fit$time)/2)+1):length(fit$time)]  
plot(time, lowmpay_rent, type='n', xlab='Time', ylab='',ylim=c(0.8,1),yaxt='n',main = "Recourse States")
lines(time, lowmpay_rent,col='red')
lines(time, highpay_rent,col='black',lty=2)
#axis(side = 2, las = 1)
legend("topright",c("Low mpay_rent","High mpay_rent"), lty=c(1,2),col=c('red','black'), bty = "n",horiz=T,lwd=1, cex = 0.8, xpd = TRUE)

dev.off()

# PMT_RENT_mpayq.csv ------------------------------------------------------

#crosstab(deldata, col.vars = c("recourse","mpay_rent_q"),row.vars = c("ne_cutoff","def"),type = "column.pct")
mpay_defpct <- ddply(deldata, .(recourse,ne_cutoff,mpay_rent_q),summarise,default=mean(def))
mpay_defpct <- mpay_defpct[!is.na(mpay_defpct$mpay_rent_q),]
write.csv(mpay_defpct,"PMT_RENT_mpayq.csv")

# Cox Regressions ---------------------------------------------------------

# without controls
cox<-list()
cox[[1]]<-coxph(Surv(duration,def)~mpay_rent+factor(state)+factor(ne_year)+factor(ne_cutoff)+factor(ne_cutoff)*mpay_rent,data = deldata[deldata$recourse==0,])
cox[[2]]<-coxph(Surv(duration,def)~mpay_rent+factor(state)+factor(ne_year)+factor(ne_cutoff)+factor(ne_cutoff)*mpay_rent,data = deldata[deldata$recourse==1,])
stargazer(cox,out='PMTRent_01_cox.htm',column.labels=c('Non-Recourse','Recourse'), omit = c("state","ne_year"),omit.labels = c("State","Year"))

# without controls seperate
cox<-list()
cox[[1]]<-coxph(Surv(duration,def)~mpay_rent+factor(state)+factor(ne_year),data = deldata[deldata$recourse==0 & deldata$ne_cutoff=="zero",])
cox[[2]]<-coxph(Surv(duration,def)~mpay_rent+factor(state)+factor(ne_year),data = deldata[deldata$recourse==1 & deldata$ne_cutoff=="zero",])
cox[[3]]<-coxph(Surv(duration,def)~mpay_rent+factor(state)+factor(ne_year),data = deldata[deldata$recourse==0 & deldata$ne_cutoff=="twenty",])
cox[[4]]<-coxph(Surv(duration,def)~mpay_rent+factor(state)+factor(ne_year),data = deldata[deldata$recourse==1 & deldata$ne_cutoff=="twenty",])
cox[[5]]<-coxph(Surv(duration,def)~mpay_rent+factor(state)+factor(ne_year),data = deldata[deldata$recourse==0 & deldata$ne_cutoff=="fifty",])
cox[[6]]<-coxph(Surv(duration,def)~mpay_rent+factor(state)+factor(ne_year),data = deldata[deldata$recourse==1 & deldata$ne_cutoff=="fifty",])
stargazer(cox,out='PMTRent_01_cox_sep.htm',column.labels=c('Non-Recourse','Recourse'), omit = c("state","ne_year"),omit.labels = c("State","Year"))

# with controls
cox<-list()
cox[[1]]<-coxph(Surv(duration,def)~mpay_rent+factor(ne_cutoff)+mpay_rent*factor(ne_cutoff)+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==0,])
cox[[2]]<-coxph(Surv(duration,def)~mpay_rent+factor(ne_cutoff)+mpay_rent*factor(ne_cutoff)+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==1,])
stargazer(cox,out='PMTRent_01_cox_controls.htm',column.labels=c('Non-Recourse','Recourse'), omit = c("state","ne_year","loanpurpose","occupancy"),omit.labels = c("State","Year","",""))

# with controls seperate
cox<-list()
cox[[1]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==0 & deldata$ne_cutoff=="zero",])
cox[[2]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==1 & deldata$ne_cutoff=="zero",])
cox[[3]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==0 & deldata$ne_cutoff=="twenty",])
cox[[4]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==1 & deldata$ne_cutoff=="twenty",])
cox[[5]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==0 & deldata$ne_cutoff=="fifty",])
cox[[6]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==1 & deldata$ne_cutoff=="fifty",])
stargazer(cox,out='PMTRent_01_cox_controls_sep.htm',column.labels=c('Non-Recourse','Recourse','Non-Recourse','Recourse','Non-Recourse','Recourse'), omit = c("state","ne_year","loanpurpose","occupancy"),omit.labels = c("State","Year","",""))

# with standardized controls

deldata['std_mpay_rent']<-(deldata$mpay_rent - mean(deldata$mpay_rent,na.rm=TRUE))/sd(deldata$mpay_rent,na.rm = TRUE)
deldata['std_del_unemp']<-(deldata$del_unemp - mean(deldata$del_unemp,na.rm=TRUE))/sd(deldata$del_unemp,na.rm = TRUE)
deldata['std_fico']<-(deldata$fico - mean(deldata$fico,na.rm=TRUE))/sd(deldata$fico,na.rm = TRUE)
deldata['std_ltv']<-(deldata$ltv - mean(deldata$ltv,na.rm=TRUE))/sd(deldata$ltv,na.rm = TRUE)
deldata['std_dti']<-(deldata$dti - mean(deldata$dti,na.rm=TRUE))/sd(deldata$dti,na.rm = TRUE)
deldata['std_ubp']<-(deldata$upb - mean(deldata$upb,na.rm=TRUE))/sd(deldata$upb,na.rm = TRUE)

cox<-list()
cox[[1]]<-coxph(Surv(duration,def)~std_mpay_rent+std_del_unemp+factor(state)+factor(ne_year)+std_fico+std_ltv+std_dti+std_ubp+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==0 & deldata$ne_cutoff=="zero",])
cox[[2]]<-coxph(Surv(duration,def)~std_mpay_rent+std_del_unemp+factor(state)+factor(ne_year)+std_fico+std_ltv+std_dti+std_ubp+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==1 & deldata$ne_cutoff=="zero",])
cox[[3]]<-coxph(Surv(duration,def)~std_mpay_rent+std_del_unemp+factor(state)+factor(ne_year)+std_fico+std_ltv+std_dti+std_ubp+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==0 & deldata$ne_cutoff=="twenty",])
cox[[4]]<-coxph(Surv(duration,def)~std_mpay_rent+std_del_unemp+factor(state)+factor(ne_year)+std_fico+std_ltv+std_dti+std_ubp+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==1 & deldata$ne_cutoff=="twenty",])
cox[[5]]<-coxph(Surv(duration,def)~std_mpay_rent+std_del_unemp+factor(state)+factor(ne_year)+std_fico+std_ltv+std_dti+std_ubp+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==0 & deldata$ne_cutoff=="fifty",])
cox[[6]]<-coxph(Surv(duration,def)~std_mpay_rent+std_del_unemp+factor(state)+factor(ne_year)+std_fico+std_ltv+std_dti+std_ubp+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==1 & deldata$ne_cutoff=="fifty",])
stargazer(cox,out='PMTRent_01_cox_std_controls.htm',column.labels=c('Non-Recourse','Recourse'), omit = c("state","ne_year","loanpurpose","occupancy"),omit.labels = c("State","Year","",""))

deldata['std_mpay_rent']<-NULL
deldata['std_del_unemp']<-NULL
deldata['std_fico']<-NULL
deldata['std_ltv']<-NULL
deldata['std_dti']<-NULL
deldata['std_ubp']<-NULL

# Rent occ pct cox --------------------------------------------------------

# without controls seperate
cox<-list()
cox[[1]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==0 & deldata$ne_cutoff=="zero"  & deldata$rent_pct<.2,])
cox[[2]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata[deldata$recourse==1 & deldata$ne_cutoff=="zero"  & deldata$rent_pct<.2,])
stargazer(cox,out='PMTRent_rent_occ_cox.htm',column.labels=c('Non-Recourse','Recourse'), omit = c("state","ne_year","loanpurpose","occupancy"),omit.labels = c("State","Year","",""))

# House value within 1sd --------------------------------------------------

# allloans <- readRDS("CombinedFreddieFannieAct_Nov18.rds")
# allloans['org_year']<-format(allloans$orgdate,"%Y")
# allloans <- allloans[(allloans$gse=="freddie"),]
# mean_sd <- ddply(allloans,.(org_year,zip),summarise,ub=(mean(upb,na.rm = TRUE)+sd(upb,na.rm = TRUE)),lb=(mean(upb,na.rm = TRUE)-sd(upb,na.rm = TRUE)))
# saveRDS(mean_sd,"mean_sd.rds")
# q60_q40 <- ddply(allloans,.(org_year,zip),summarise,ub=quantile(upb,0.6,na.rm = TRUE),lb=quantile(upb,0.4,na.rm = TRUE))
# saveRDS(q60_q40,"q60_q40.rds")


mean_sd <- readRDS("mean_sd.rds")
deldata['org_year']<-format(as.Date(deldata$orgdate),"%Y")
deldata <- merge(deldata,mean_sd,by=c("org_year","zip"))
temp <- deldata[deldata$upb<deldata$ub & deldata$upb>deldata$lb,]

cox<-list()
cox[[1]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = temp[temp$recourse==0 & temp$ne_cutoff=="zero",])
cox[[2]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = temp[temp$recourse==1 & temp$ne_cutoff=="zero",])
cox[[3]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = temp[temp$recourse==0 & temp$ne_cutoff=="twenty",])
cox[[4]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = temp[temp$recourse==1 & temp$ne_cutoff=="twenty",])
stargazer(cox,out='PMTRent_01_cox_controls_sep_onesd.htm',column.labels=c('Non-Recourse','Recourse','Non-Recourse','Recourse','Non-Recourse','Recourse'), omit = c("state","ne_year","loanpurpose","occupancy"),omit.labels = c("State","Year","",""))


q60_q40 <- readRDS("q60_q40.rds")
deldata['org_year']<-format(as.Date(deldata$orgdate),"%Y")
deldata <- merge(deldata,q60_q40,by=c("org_year","zip"))
temp <- deldata[deldata$upb<deldata$ub & deldata$upb>deldata$lb,]


cox<-list()
cox[[1]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = temp[temp$recourse==0 & temp$ne_cutoff=="zero",])
cox[[2]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = temp[temp$recourse==1 & temp$ne_cutoff=="zero",])
cox[[3]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = temp[temp$recourse==0 & temp$ne_cutoff=="twenty",])
cox[[4]]<-coxph(Surv(duration,def)~mpay_rent+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = temp[temp$recourse==1 & temp$ne_cutoff=="twenty",])
stargazer(cox,out='PMTRent_01_cox_controls_sep_q60_q40.htm',column.labels=c('Non-Recourse','Recourse','Non-Recourse','Recourse','Non-Recourse','Recourse'), omit = c("state","ne_year","loanpurpose","occupancy"),omit.labels = c("State","Year","",""))

# Median mpay median mpay_rent --------------------------------------------

median_mpay <- ddply(deldata,.(ne_year,recourse,zip),summarise,
                     def=mean(def,na.rm = TRUE),
                     median_mpay_rent = (median(mpay,na.rm = TRUE)/median(medgrossrent,na.rm = TRUE)),
                     fico = median(fico,na.rm = TRUE),
                     ltv = median(ltv,na.rm = TRUE),
                     dti = median(dti,na.rm = TRUE),
                     upb = median(upb,na.rm = TRUE),
                     medhhincome = median(medhhincome,na.rm = TRUE),
                     medhouseage = median(medhouseage,na.rm = TRUE),
                     vacantpct = median(vacantpct,na.rm = TRUE),
                     bachelorpct = median(bachelorpct,na.rm = TRUE),
                     withsalarypct = median(withsalarypct,na.rm = TRUE))

median_mpay['median_mpay_rent_q']<-ifelse(median_mpay$median_mpay_rent<quantile(median_mpay$median_mpay_rent,0.2,na.rm=TRUE),1,
                                          ifelse(median_mpay$median_mpay_rent<quantile(median_mpay$median_mpay_rent,0.4,na.rm=TRUE),2,
                                                 ifelse(median_mpay$median_mpay_rent<quantile(median_mpay$median_mpay_rent,0.6,na.rm=TRUE),3,
                                                        ifelse(median_mpay$median_mpay_rent<quantile(median_mpay$median_mpay_rent,0.8,na.rm=TRUE),4,
                                                               ifelse(median_mpay$median_mpay_rent<max(median_mpay$median_mpay_rent,na.rm=TRUE),5,NA)))))


median_desc <- ddply(median_mpay,.(median_mpay_rent_q),summarise,default_pct = mean(def,na.rm = TRUE))


ols <- list()
ols[[1]]<-lm(def~median_mpay_rent+factor(recourse)+factor(ne_year),data=median_mpay)
ols[[2]]<-lm(def~median_mpay_rent+factor(recourse)+factor(ne_year)+fico+ltv+dti+
               log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct,data=median_mpay)
stargazer(ols,out='PMTRent_rent_zip_median.htm', omit = c("state","ne_year"),omit.labels = c("State","Year"))

# Median mpay fama mcbeth -------------------------------------------------
years <- unique(median_mpay$ne_year)

median_mpay <- ddply(deldata,.(reportingperiod_t,recourse,zip),summarise,
                     def=mean(def,na.rm = TRUE),
                     median_mpay_rent = (median(mpay,na.rm = TRUE)/median(medgrossrent,na.rm = TRUE))
                     )

median_mpay['ne_year'] <- format(as.Date(median_mpay$reportingperiod_t),'%Y')

betas_nr <- NULL
betas_r <- NULL

for(i in 5:length(years)) {
  temp <- deldata[deldata$ne_year %in% (years[i-4]:years[i]) & deldata$recourse==0,]
  temp['median_mpay_rent'] <- temp$mpay_rent
  zip_mean <- ddply(temp,
                    .(zip),summarise,
                    mean_mpay_rent=mean(median_mpay_rent,na.rm = TRUE))
  
  q <- quantile(temp$median_mpay_rent, probs = seq(0,1,by=0.04),na.rm = TRUE)
  temp['pf'] <- cut(temp$median_mpay_rent, q, include.lowest=TRUE,
                      labels=paste("pf", 1:25))
  
  temp <- ddply(temp,.(pf),summarise,
                def=mean(def,na.rm = TRUE),
                median_mpay_rent = mean(median_mpay_rent,na.rm = TRUE))
  
  temp<- summary(lm(def~median_mpay_rent,data=temp))
  betas_nr <- rbind(betas_nr,c(years[i],temp$coefficients[[2]],as.numeric(temp$coefficients[,3][2]),as.numeric(temp$coefficients[,2][2])))
  
  
  temp <- deldata[deldata$ne_year %in% (years[i-4]:years[i]) & deldata$recourse==1,]
  temp['median_mpay_rent'] <- temp$mpay_rent
  zip_mean <- ddply(temp,
                    .(zip),summarise,
                    mean_mpay_rent=mean(median_mpay_rent,na.rm = TRUE))
  
  q <- quantile(temp$median_mpay_rent, probs = seq(0,1,by=0.04),na.rm = TRUE)
  temp['pf'] <- cut(temp$median_mpay_rent, q, include.lowest=TRUE,
                    labels=paste("pf", 1:25))
  
  temp <- ddply(temp,.(pf),summarise,
                def=mean(def,na.rm = TRUE),
                median_mpay_rent = mean(median_mpay_rent,na.rm = TRUE))
  
  temp<- summary(lm(def~median_mpay_rent,data=temp))
  betas_r <- rbind(betas_r,c(years[i],temp$coefficients[[2]],as.numeric(temp$coefficients[,3][2]),as.numeric(temp$coefficients[,2][2])))
  
}
betas_nr <- as.data.frame(betas_nr)
betas_r <- as.data.frame(betas_r)
names(betas_nr)<-c("year","coefficient_nr","tstat_nr","sd_nr")
names(betas_r)<-c("year","coefficient_r","tstat_n","sd_r")
betas <- merge(betas_nr,betas_r,by=c("year"))

betas[,1:7] = apply(betas[,1:7], 2, function(x) as.numeric(as.character(x)))
betas['lb_nr'] <- betas$coefficient_nr-1.96*betas$sd_nr
betas['ub_nr'] <- betas$coefficient_nr+1.96*betas$sd_nr
betas['lb_r'] <- betas$coefficient_r-1.96*betas$sd_r
betas['ub_r'] <- betas$coefficient_r+1.96*betas$sd_r


pdf(file='PMT_RENT_median_famamcbeth.pdf', width = 15, height = 8)
  plot(betas$year, betas$coefficient_nr, type='n', xlab='Negative Equity Year', ylab='Coefficient',ylim = c(min(betas$lb_nr),max(betas$ub_nr)),yaxt='n')
  lines(betas$year, betas$coefficient_nr,col='red',lwd=2)
  lines(betas$year, betas$lb_nr,col='red',lty=3)
  lines(betas$year, betas$ub_nr,col='red',lty=3)
  
  lines(betas$year, betas$coefficient_r,col='black',lwd=2)
  lines(betas$year, betas$lb_r,col='black',lty=3)
  lines(betas$year, betas$ub_r,col='black',lty=3)
  
  abline(0,0)
  axis(side = 2, las = 1)
  legend("topleft",c("Non-Recourse States","Recourse States"), lty=c(1,1),col=c('red','black'), bty = "n",horiz=T,lwd=1, cex = 0.8, xpd = TRUE)
dev.off()






betas_nr <- NULL
betas_r <- NULL

for(i in 5:length(years)) {
  temp <- deldata[deldata$ne_year %in% (years[i-4]:years[i]) & deldata$recourse==0,]
  temp['median_mpay_rent'] <- temp$mpay_rent

  q <- quantile(temp$median_mpay_rent, probs = seq(0,1,by=0.04),na.rm = TRUE)
  temp['pf'] <- cut(temp$median_mpay_rent, q, include.lowest=TRUE,
                    labels=paste("pf", 1:25))
  
  temp <- ddply(temp,.(pf),summarise,
                def=mean(def,na.rm = TRUE),
                median_mpay_rent = median(median_mpay_rent,na.rm = TRUE))
  
  temp<- summary(lm(def~median_mpay_rent,data=temp))
  betas_nr <- rbind(betas_nr,c(years[i],temp$coefficients[[2]],as.numeric(temp$coefficients[,3][2]),as.numeric(temp$coefficients[,2][2])))
  
  
  temp <- deldata[deldata$ne_year %in% (years[i-4]:years[i]) & deldata$recourse==1,]
  temp['median_mpay_rent'] <- temp$mpay_rent
  zip_mean <- ddply(temp,
                    .(zip),summarise,
                    mean_mpay_rent=mean(median_mpay_rent,na.rm = TRUE))
  
  q <- quantile(temp$median_mpay_rent, probs = seq(0,1,by=0.04),na.rm = TRUE)
  temp['pf'] <- cut(temp$median_mpay_rent, q, include.lowest=TRUE,
                    labels=paste("pf", 1:25))
  
  temp <- ddply(temp,.(pf),summarise,
                def=mean(def,na.rm = TRUE),
                median_mpay_rent = mean(median_mpay_rent,na.rm = TRUE))
  
  temp<- summary(lm(def~median_mpay_rent,data=temp))
  betas_r <- rbind(betas_r,c(years[i],temp$coefficients[[2]],as.numeric(temp$coefficients[,3][2]),as.numeric(temp$coefficients[,2][2])))
  
}
betas_nr <- as.data.frame(betas_nr)
betas_r <- as.data.frame(betas_r)
names(betas_nr)<-c("year","coefficient_nr","tstat_nr","sd_nr")
names(betas_r)<-c("year","coefficient_r","tstat_n","sd_r")
betas <- merge(betas_nr,betas_r,by=c("year"))

betas[,1:7] = apply(betas[,1:7], 2, function(x) as.numeric(as.character(x)))
betas['lb_nr'] <- betas$coefficient_nr-1.96*betas$sd_nr
betas['ub_nr'] <- betas$coefficient_nr+1.96*betas$sd_nr
betas['lb_r'] <- betas$coefficient_r-1.96*betas$sd_r
betas['ub_r'] <- betas$coefficient_r+1.96*betas$sd_r


pdf(file='PMT_RENT_median_famamcbeth_2.pdf', width = 15, height = 8)
plot(betas$year, betas$coefficient_nr, type='n', xlab='Negative Equity Year', ylab='Coefficient',ylim = c(min(betas$lb_nr),max(betas$ub_nr)),yaxt='n')
lines(betas$year, betas$coefficient_nr,col='red',lwd=2)
lines(betas$year, betas$lb_nr,col='red',lty=3)
lines(betas$year, betas$ub_nr,col='red',lty=3)

lines(betas$year, betas$coefficient_r,col='black',lwd=2)
lines(betas$year, betas$lb_r,col='black',lty=3)
lines(betas$year, betas$ub_r,col='black',lty=3)

abline(0,0)
axis(side = 2, las = 1)
legend("topleft",c("Non-Recourse States","Recourse States"), lty=c(1,1),col=c('red','black'), bty = "n",horiz=T,lwd=1, cex = 0.8, xpd = TRUE)
dev.off()

# Zillow Rent -------------------------------------------------------------

rent <- read.csv("zillowrent.csv")
rent['ne_date']<-as.Date(rent$Month)
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
rent <-ddply(rent,.(zip,ne_date),summarize,rent=median(rent,na.rm=TRUE))

summary(rent$rent)
length(unique(rent$zip))
length(unique(rent$ne_date))

deldata_zillow <- merge(deldata,rent,by=c("ne_date","zip"))

deldata_zillow['mpay_zillow_rent']<-deldata_zillow$mpay/deldata_zillow$rent
deldata_zillow['mpay_zillow_rent_q']<-ifelse(deldata_zillow$mpay_zillow_rent<quantile(deldata_zillow$mpay_zillow_rent,0.2,na.rm=TRUE),1,
                               ifelse(deldata_zillow$mpay_zillow_rent<quantile(deldata_zillow$mpay_zillow_rent,0.4,na.rm=TRUE),2,
                                      ifelse(deldata_zillow$mpay_zillow_rent<quantile(deldata_zillow$mpay_zillow_rent,0.6,na.rm=TRUE),3,
                                             ifelse(deldata_zillow$mpay_zillow_rent<quantile(deldata_zillow$mpay_zillow_rent,0.8,na.rm=TRUE),4,
                                                    ifelse(deldata_zillow$mpay_zillow_rent<max(deldata_zillow$mpay_zillow_rent,na.rm=TRUE),5,NA)))))

mpay_defpct <- ddply(deldata_zillow, .(recourse,ne_cutoff,mpay_zillow_rent_q),summarise,default=mean(def))
mpay_defpct <- mpay_defpct[!is.na(mpay_defpct$mpay_zillow_rent_q),]
write.csv(mpay_defpct,"PMT_RENT_mpayq_zillow.csv")

cox<-list()
cox[[1]]<-coxph(Surv(duration,def)~mpay_zillow_rent+age_t+age_t*age_t+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata_zillow[deldata_zillow$recourse==0 & deldata_zillow$ne_cutoff=="zero",])
cox[[2]]<-coxph(Surv(duration,def)~mpay_zillow_rent+age_t+age_t*age_t+del_unemp+factor(state)+factor(ne_year)+fico+ltv+dti+log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(occupancy)+factor(loanpurpose),data = deldata_zillow[deldata_zillow$recourse==1 & deldata_zillow$ne_cutoff=="zero",])
stargazer(cox,out='PMTRent_01_cox_zillow.htm',column.labels=c('Non-Recourse','Recourse'), omit = c("state","ne_year","loanpurpose","occupancy"),omit.labels = c("State","Year","",""))

median_mpay_zillow <- ddply(deldata_zillow,.(ne_year,ne_cutoff,recourse,state,zip),summarise,
                     def=mean(def,na.rm = TRUE),
                     median_mpay_rent = (median(mpay,na.rm = TRUE)/median(rent,na.rm = TRUE)),
                     fico = median(fico,na.rm = TRUE),
                     ltv = median(ltv,na.rm = TRUE),
                     dti = median(dti,na.rm = TRUE),
                     upb = median(upb,na.rm = TRUE),
                     medhhincome = median(medhhincome,na.rm = TRUE),
                     medhouseage = median(medhouseage,na.rm = TRUE),
                     vacantpct = median(vacantpct,na.rm = TRUE),
                     bachelorpct = median(bachelorpct,na.rm = TRUE),
                     withsalarypct = median(withsalarypct,na.rm = TRUE))


ols <- list()
ols[[1]]<-lm(def~median_mpay_rent+factor(ne_cutoff),data=median_mpay_zillow)
ols[[2]]<-lm(def~median_mpay_rent+factor(ne_cutoff)+factor(recourse)+log(upb)+factor(ne_year)+factor(state),data=median_mpay_zillow)
ols[[3]]<-lm(def~median_mpay_rent+factor(ne_cutoff)+factor(recourse)+factor(ne_year)+fico+ltv+dti+
               log(upb)+medhhincome+medhouseage+vacantpct+bachelorpct+withsalarypct+factor(state),data=median_mpay_zillow)
stargazer(ols,out='PMTRent_rent_zip_median_zillow.htm', omit = c("state","ne_year"),omit.labels = c("State","Year"))

