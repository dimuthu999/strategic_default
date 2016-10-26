rm(list=ls())
source("functions.R")



# Functions ---------------------------------------------------------------


amortize <- function(p_input = 25000, i_input = .10, n_months = 36, 
                     output = "table", index = NULL) { 
  
  n_months <- rep(n_months, length(p_input))
  
  if(is.null(index)) {
    index <- matrix(rep(1:length(n_months), each = n_months[1]), 
                    nrow = n_months[1])
  } else {
    index <- matrix(rep(index, each = n_months[1]), nrow = n_months[1])
  }
  
  p_input <- matrix(p_input, ncol = length(p_input))
  i_input <- matrix(i_input, ncol = length(i_input))
  i_monthly <- i_input / (12)
  payment <- p_input * i_monthly / (1 - (1 + i_monthly)^(-n_months[1]))
  
  Pt <- p_input # current principal or amount of loan
  currP <- NULL
  
  for(i in 1:n_months[1]) {
    H <- Pt * i_monthly # current monthly interest
    C <- payment - H # monthly payment minus monthly interest (principal paid for each month)
    Q <- Pt - C # new balance of principal of loan
    Pt <- Q # loops through until balance goes to zero
    currP <- rbind(currP, Pt)    
  }
  
  amortization <- rbind(p_input, currP[1:(n_months[1]-1),, drop = FALSE])
  monthly_principal <- amortization - currP
  monthly_interest <- rbind(
    (matrix(
      rep(payment, n_months[1]), 
      nrow = n_months[1], 
      byrow = TRUE) - monthly_principal)[1:(n_months[1]-1),, drop = FALSE],
    rep(0, length(n_months)))
  monthly_interest[1:nrow(monthly_interest) %% 12 == 0] <-
    monthly_principal[1:nrow(monthly_interest) %% 12 == 0] * i_monthly
  monthly_payment <- monthly_principal + monthly_interest
  installment <- matrix(rep(1 : n_months[1], length(n_months)), 
                        nrow = n_months[1])
  
  input <- list(
    "amortization" = amortization,
    "payment" = monthly_payment,
    "principal" = monthly_principal,
    "interest" = monthly_interest,
    "installment" = installment,
    "index" = index)
  
  out <- switch(output, 
                "list" = input,
                "table" = as.data.frame(
                  lapply(input, as.vector), 
                  stringsAsFactors = FALSE),
                "balance" = as.data.frame(
                  lapply(input[c("index", "amortization")], as.vector), 
                  stringsAsFactors = FALSE),
                "payment" = as.data.frame(
                  lapply(input[c("index", "payment")], as.vector), 
                  stringsAsFactors = FALSE),
                "principal" = as.data.frame(
                  lapply(input[c("index", "principal")], as.vector), 
                  stringsAsFactors = FALSE), 
                "interest" = as.data.frame(
                  lapply(input[c("index", "interest")], as.vector), 
                  stringsAsFactors = FALSE)
  )
  
  out
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Illustration ------------------------------------------------------------
library(FinCal)
library(ggplot2)
library("reshape2")
# loan originated in 2005-Jan-01
# House value at origination: 300,000. Loan amount 240,000
# Current date 2010-Jan-01. i.e. after 60  months since origination

upb = 240000
interestrate = 6
term = 360
current_age= 60
time_horizen = 84

negative_equity = -0.5
mpay <- mortgage(upb,interestrate,term)
rent = 1500
amort_table <- amortize(upb,interestrate/100,term)
outstanding_start = amort_table[current_age,]$amortization
outstanding_end = amort_table[current_age+time_horizen,]$amortization
forc_time = 12
recourse_recovery = 0.2


other_cost_of_default = 5000
cost_of_default = (pv(r = interestrate/1200,pmt = -rent,n = (time_horizen-forc_time)))/((1+interestrate/1200)^forc_time)+other_cost_of_default


ne <- seq(from=0,to=-1,by=-0.05)
house_appreciation = 4
ne_plot0 <- NULL
for(negative_equity in ne)  {
  current_housevalue = outstanding_start/(1-negative_equity)
  housevalue_end = current_housevalue*(1+house_appreciation/1200)^time_horizen  
  if(outstanding_end>housevalue_end) {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)
  } 
  else {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)+(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
  }
  cost_of_default_r = (pv(r = interestrate/1200,pmt = -2000,n = (time_horizen-forc_time)))/((1+interestrate/1200)^forc_time)+other_cost_of_default
  
  ne_plot0 <- rbind(ne_plot0,c(negative_equity,cost_of_default,cost_of_default_r,cost_of_nodefault))
} 
ne_plot0 <- as.data.frame(ne_plot0)
names(ne_plot0) <- c("negative_equity","cost_of_default(Rent = 2000)","cost_of_default(Rent=1200)","cost_of_no_default")
ne_plot0 <- melt(ne_plot0, id="negative_equity")
names(ne_plot0)[3]<-"cost"


ne <- seq(from=0,to=-1,by=-0.05)
house_appreciation = 2
ne_plot <- NULL
for(negative_equity in ne)  {
  current_housevalue = outstanding_start/(1-negative_equity)
  housevalue_end = current_housevalue*(1+house_appreciation/1200)^time_horizen  
  if(outstanding_end>housevalue_end) {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)+other_cost_of_default/((1+interestrate/1200)^time_horizen)
  } 
  else {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)+(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
  }
  ne_plot <- rbind(ne_plot,c(negative_equity,cost_of_default,cost_of_nodefault))
} 
ne_plot <- as.data.frame(ne_plot)
names(ne_plot) <- c("negative_equity","cost_of_default(NR)","cost_of_no_default")
ne_plot <- melt(ne_plot, id="negative_equity")
names(ne_plot)[3]<-"cost"


ne <- seq(from=0,to=-1,by=-0.05)
house_appreciation = 2
ne_plot2 <- NULL
for(negative_equity in ne)  {
  current_housevalue = outstanding_start/(1-negative_equity)
  housevalue_end = current_housevalue*(1+house_appreciation/1200)^time_horizen  
  if(outstanding_end>housevalue_end) {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)+other_cost_of_default/((1+interestrate/1200)^time_horizen)+recourse_recovery*(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
    cost_of_default_r = cost_of_default+recourse_recovery*(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
  } 
  else {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)+(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
    cost_of_default_r = cost_of_default
  }
  ne_plot2 <- rbind(ne_plot2,c(negative_equity,cost_of_default_r,cost_of_nodefault))
} 
ne_plot2 <- as.data.frame(ne_plot2)
names(ne_plot2) <- c("negative_equity","cost_of_default(R)","cost_of_no_default")
ne_plot2 <- melt(ne_plot2, id="negative_equity")
names(ne_plot2)[3]<-"cost"


ne = c(-0.25)
app_rate = seq(from=-5,to=5,by=0.5)
app_rate_plot <- NULL
for(house_appreciation in app_rate)  {
  current_housevalue = outstanding_start/(1-ne)
  housevalue_end = current_housevalue*(1+house_appreciation/1200)^time_horizen  
  if(outstanding_end>housevalue_end) {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)
    cost_of_default_r = cost_of_default+recourse_recovery*(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
  } 
  else {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)+(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
    cost_of_default_r = cost_of_default
  }  
  app_rate_plot <- rbind(app_rate_plot,c(house_appreciation,cost_of_default,cost_of_default_r,cost_of_nodefault))
} 
app_rate_plot <- as.data.frame(app_rate_plot)
names(app_rate_plot) <- c("house_appreciation","cost_of_default(NR)","cost_of_default(R)","cost_of_no_default")
app_rate_plot <- melt(app_rate_plot, id="house_appreciation")
names(app_rate_plot)[3]<-"cost"

rent = 1800
ne = -0.5
house_appreciation = 0
ft = seq(from=8,to=24,by=0.5)
forc_time_plot <- NULL
for(forc_time in ft)  {
  cost_of_default = (pv(r = interestrate/1200,pmt = -rent,n = (time_horizen-forc_time)))/((1+interestrate/1200)^forc_time)+other_cost_of_default
  current_housevalue = outstanding_start/(1-ne)
  housevalue_end = current_housevalue*(1+house_appreciation/1200)^time_horizen  
  if(outstanding_end>housevalue_end) {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)
    cost_of_default_r = cost_of_default+recourse_recovery*(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
  } 
  else {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)+(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
    cost_of_default_r = cost_of_default
  }  
  forc_time_plot <- rbind(forc_time_plot,c(forc_time,cost_of_default,cost_of_default_r,cost_of_nodefault))
} 
forc_time_plot <- as.data.frame(forc_time_plot)
names(forc_time_plot) <- c("forc_time","cost_of_default(NR)","cost_of_default(R)","cost_of_no_default")
forc_time_plot <- melt(forc_time_plot, id="forc_time")
names(forc_time_plot)[3]<-"cost"


ne = -0.25
house_appreciation = 2
rt = seq(from=0,to=200,by=5)
forc_time = 12
rt_plot <- NULL
for(time_horizen in rt)  {
  if(time_horizen<=84) cost_of_default = (pv(r = interestrate/1200,pmt = -rent,n = (time_horizen-forc_time)))/((1+interestrate/1200)^forc_time)
  else cost_of_default = (pv(r = interestrate/1200,pmt = -rent,n = (84-forc_time)))/((1+interestrate/1200)^forc_time)+
      (pv(r = interestrate/1200,pmt = -mpay,n = ((time_horizen-84)-forc_time)))/((1+interestrate/1200)^84)+(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
  cost_of_default = cost_of_default + other_cost_of_default
  current_housevalue = outstanding_start/(1-ne)
  housevalue_end = current_housevalue*(1+house_appreciation/1200)^time_horizen  
  if(outstanding_end>housevalue_end) {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)
    cost_of_default_r = cost_of_default+recourse_recovery*(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
  } 
  else {
    cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)+(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
    cost_of_default_r = cost_of_default
  }  
  rt_plot <- rbind(rt_plot,c(time_horizen,cost_of_default,cost_of_default_r,cost_of_nodefault))
} 
rt_plot <- as.data.frame(rt_plot)
names(rt_plot) <- c("remaining_time","cost_of_default(NR)","cost_of_default(R)","cost_of_no_default")
rt_plot <- melt(rt_plot, id="remaining_time")
names(rt_plot)[3]<-"cost"



p0 <- ggplot(data=ne_plot0,aes(x=negative_equity, y=cost, colour=variable)) +  geom_line(aes(linetype=variable), size=2)+
  theme(legend.position="bottom",legend.direction="horizontal",legend.title=element_blank())+
  xlab("Negative Equity") + ylab("Cost ($)")+
  annotate("text", x = -0.25, y = 125000, label = "House Price Appreciation: 4%", size = 3)

p1 <- ggplot(data=app_rate_plot,aes(x=house_appreciation, y=cost, colour=variable)) +  geom_line(aes(linetype=variable), size=2)+
  theme(legend.position="bottom",legend.direction="horizontal",legend.title=element_blank())+
  xlab("House Value Appreciation Rate (%)") + ylab("Cost ($)")+
  annotate("text", x = 2.5, y = 110000, label = "Negative Equity: -25%", size = 3)

p2_1 <- ggplot(data=ne_plot,aes(x=negative_equity, y=cost, colour=variable)) +  geom_line(aes(linetype=variable), size=2)+
  theme(legend.position="bottom",legend.direction="horizontal",legend.title=element_blank())+
  xlab("Negative Equity") + ylab("Cost ($)")#+
  #annotate("text", x = -0.25, y = 110000, label = "House Price Appreciation: 2%", size = 3)

p2_2 <- ggplot(data=ne_plot2,aes(x=negative_equity, y=cost, colour=variable)) +  geom_line(aes(linetype=variable), size=2)+
  theme(legend.position="bottom",legend.direction="horizontal",legend.title=element_blank())+
  xlab("Negative Equity") + ylab("Cost ($)")#+
  #annotate("text", x = -0.25, y = 110000, label = "House Price Appreciation: 2%", size = 3)



p3 <- ggplot(data=forc_time_plot,aes(x=forc_time, y=cost, colour=variable)) +  geom_line(aes(linetype=variable), size=2)+
  theme(legend.position="bottom",legend.direction="horizontal",legend.title=element_blank())+
  xlab("Foreclosure Time") + ylab("Cost ($)")+
annotate("text", x = 20, y = 148000, label = "House Price Appreciation: 0%; Negative Equity: -0.5", size = 3)


p4 <- ggplot(data=rt_plot,aes(x=remaining_time, y=cost, colour=variable)) +  geom_line(aes(linetype=variable), size=2)+
  theme(legend.position="bottom",legend.direction="horizontal",legend.title=element_blank())+
  xlab("Remaining Time") + ylab("Cost ($)")+
  annotate("text", x = 50, y = 172000, label = "House Price Appreciation: 2%; Negative Equity: 0", size = 3)

p0 

multiplot(p2_1, p2_2, cols=2)

multiplot(p3, p4, cols=2)