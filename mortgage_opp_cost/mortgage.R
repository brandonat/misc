# Determine Required Home Appreciation
# to equate to an equivalent investment

library(FinCal)
library(ggplot2)
library(reshape2)

# Mortgage Settings
# ----------------

  m <- list(
    price = 100000,
    down  = 10000,  
    tax   = 200,
    insur = 50,  
    int   = .04)
  m$loan  <- m$price - m$down
  m$close <- m$price * 0.025
  m$rate  <- 1 + m$int / 12
  m$pay   <- with(m, round(pmt( int/12 , 360 , -loan , 0),2))

# Prior Rent with 2% increase each year
# -------------------------------------
 
  r       <- list(pay = 2000, insur = 10)
  rent    <- c(r$pay, 1:360)
  newyear <- 0:360 %% 12 == 0
  for (i in 2:361) {
    rent[i] <- rent[i-1] + newyear[i] * rent[i-1] * .02
  }
  rent <- rent + r$ins

# Individual Req's/Restraints
# ---------------------------
  
  ret <- 0.08     # Required return
  tax <- 0.25     # Marignal tax rate

# Amortization Table
# ------------------

  month <- 0:360
  balance <- with(m,loan*(rate^month) - pay*(rate^month-1)/(rate-1))
  balance <- ifelse(balance < 0, 0, balance)
  ip  <- c(0,balance[-361]*(m$rate - 1))
  pp  <- c(0,m$pay - ip[-1])
  pay <- c(0,rep(m$pay,times=360))
  equity <- m$down + cumsum(pp)
  equity[361] <- m$price
  amort <- cbind(pay,pp,ip,balance,equity)

# Assumed Invesment & Additions 
# -----------------------------

  # Investment begins with initial outflows
  portf <- c(m$down + m$close,rep(0,360))

  # Effective payment after taxes
  eff_pay <- ip[-1]*(1-tax) + pp[-1] + m$insur + m$tax
  eff_pay <- c(0,eff_pay)
  
  # Any excess over previous rent assumed to be invested
  portf_add <- pmax(eff_pay - rent, 0)  
  
  # Growth in portfolio at req. rate with additions of 'portf_add'
  for (i in 2:361) {
    portf[i] <- (portf[i-1] + eff_pay[i-1]) * (1 + ret)^(1/12)
  }

# Required Sales Price
# --------------------

# Objective: net sales transaction = portfolio value,
# assuming closing costs to sell are 8.5% of the sales price
#
# sell - .085*sell - balance + equity = portf
#

  sell <- (portf + balance - equity) / (1 - .085)
  appr <- sell/m$price - 1

# Plot
# --------------------

  start <- as.POSIXct("2014-03-01")
  date <- seq(start,by="month",length=361)
  cf <- as.data.frame(date)
  cf <- cbind(cf,month,pay,ip,pp,balance,equity,portf,sell,appr)
  head(cf,13)
  
  cf_melt <- melt(cf,id.vars="date",measure.vars=c("balance","equity","portf","sell"),variable.name="account")
  
  ggplot(subset(cf_melt,date <= as.POSIXct("2024-03-01"))) +
      geom_line(aes(x=date,y=value,color=account))
    
