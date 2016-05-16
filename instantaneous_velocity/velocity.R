
set.seed(123123)

library(lubridate)
options(digits.secs=3)

## 10 minutes of uniform random time
anchor <- ymd_hms("2014-01-01 00:00:00.000",tz="US/Eastern")

## Scenarios
spike   <- anchor + sort(seconds(c(runif(1000,min=0,max=600),rnorm(1000,120,15))))
uniform <- anchor + sort(seconds(runif(5000,min=0,max=600)))
bigunif <- anchor + sort(seconds(runif(1e6,0,86400)))

velocity <- function(ts,interval=1,output=FALSE,plot=FALSE) {  
  require(dplyr)
  vel <- rep(0,length(ts))    
  continue <- TRUE  
  i <- 1  
  while(continue) {
    i <- i + 1  
    # in range if lag is within 'interval' seconds
    in_range <- as.numeric(ts - lag(ts,i)) <= interval
    vel      <- vel + in_range
    continue <- sum(in_range,na.rm=TRUE) > 0  
  }
  if (output) print(paste("Completed in",i,"iteration(s)."))
  if (plot) {
      require(ggplot2)
      df <- data.frame(time = ts, velocity = vel)
      suppressWarnings( print( 
        ggplot(df) + geom_point(aes(x=time,y=velocity),color="steelblue") +
               labs(x="Time",y="Obs. per second",title="Instantaneous Velocity") + theme_bw()
      ))
  }  
  invisible(vel)
}

## Test
velocity(uniform,output=TRUE,plot=TRUE)
velocity(spike,output=TRUE,plot=TRUE)
velocity(bigunif,output=TRUE,plot=TRUE)

system.time(velocity(uniform,interval=1))
system.time(velocity(bigunif,interval=1))
