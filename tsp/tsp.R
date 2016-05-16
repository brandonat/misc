#!! BAT 12jun2014 

## Calculate mean return and st.dev. for TSP funds 
## over a two year period using month end prices 
## 
## Share prices from: 
## https://www.tsp.gov/investmentfunds/shareprice/sharePriceHistory.shtml 

library(xts) 
library(reshape2) 
library(ggplot2) 

setwd("~/Google Drive/Projects/TSP")

## INPUT 

tsp <- read.csv("shareprices.csv",stringsAsFactors=FALSE) 
tsp$X <- NULL 
tsp$date <- as.POSIXct(strptime(tsp$date,"%Y-%m-%d"))
tsp <- as.xts(tsp[,-1],order.by=tsp[,1]) 

## FUNCTIONS TO BE USED IN APPLY 

returns <- function(col) { 
  eom <- to.monthly(col)[,4] 
  (eom-lag(eom))/lag(eom) 
} 
ann_ret <- function(x) { 
  mu <- mean(x,na.rm=TRUE) 
  (1 + mu)^12 - 1   
} 
ann_sd  <- function(x) { 
  sd <- sd(x,na.rm=TRUE) 
  sd * sqrt(12)   
} 

## COMPUTE RETURNS, MEAN RETURN, SD 

ret <- apply(tsp,2,returns) 
apply(ret,2,ann_ret) 
apply(ret,2,ann_sd) 

## PLOT MONTHLY RETURNS 

df <- as.data.frame(ret) 
df$Date <- as.POSIXct(strptime( 
  paste("01 ",rownames(ret)), 
  format = "%d %b %Y")) 

df <- melt(df,id.vars="Date",value.name="Return",variable.name="Fund") 
ggplot(subset(df,!(Fund == "F.Fund"))) + 
  geom_line(aes(x=Date,y=Return,color=Fund)) + 
  ggtitle("Monthly Returns of TSP Funds")