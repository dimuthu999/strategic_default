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
rent = 2000
amort_table <- amortize(upb,interestrate/100,term)
outstanding_start = amort_table[current_age,]$amortization
outstanding_end = amort_table[current_age+time_horizen,]$amortization


other_cost_of_default = 2000
cost_of_default = pv(r = interestrate/1200,pmt = -rent,n = time_horizen)+other_cost_of_default

ne <- seq(from=0,to=-1,by=-0.05)
house_appreciation = 0
ne_plot <- NULL
for(negative_equity in ne)  {
  current_housevalue = outstanding_start/(1-negative_equity)
  housevalue_end = current_housevalue*(1+house_appreciation/1200)^time_horizen  
  cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)+(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
  ne_plot <- rbind(ne_plot,c(negative_equity,cost_of_default,cost_of_nodefault))
} 
ne_plot <- as.data.frame(ne_plot)
names(ne_plot) <- c("negative_equity","cost_of_default","cost_of_no_default")
ne_plot <- melt(ne_plot, id="negative_equity")
names(ne_plot)[3]<-"cost"

ne = c(-0.25,-0.75)
app_rate = seq(from=-10,to=10,by=0.5)
app_rate_plot <- NULL
for(house_appreciation in app_rate)  {
  current_housevalue = outstanding_start/(1-ne)
  housevalue_end = current_housevalue*(1+house_appreciation/1200)^time_horizen  
  cost_of_nodefault = pv(r=interestrate/1200,pmt = -mpay,n = time_horizen)+(outstanding_end-housevalue_end)/((1+interestrate/1200)^time_horizen)
  app_rate_plot <- rbind(app_rate_plot,c(house_appreciation,cost_of_default,cost_of_nodefault))
} 
app_rate_plot <- as.data.frame(app_rate_plot)
names(app_rate_plot) <- c("house_appreciation","cost_of_default","cost_of_no_default_25","cost_of_no_default_75")
app_rate_plot <- melt(app_rate_plot, id="house_appreciation")
names(app_rate_plot)[3]<-"cost"


p1 <- ggplot(data=app_rate_plot,aes(x=house_appreciation, y=cost, colour=variable)) +  geom_line()+
  theme(legend.position="bottom",legend.direction="horizontal",legend.title=element_blank())+
  xlab("House Value Appreciation Rate (%)") + ylab("Cost ($)")

p2 <- ggplot(data=ne_plot,aes(x=negative_equity, y=cost, colour=variable)) +  geom_line()+
  theme(legend.position="bottom",legend.direction="horizontal",legend.title=element_blank())+
  xlab("Negative Equity") + ylab("Cost ($)")

multiplot(p2, p1, cols=2)