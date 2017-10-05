setwd("C:/Users/kimberly.cressman/Desktop/Main Docs/data visualization/storm effects")

library(plyr)
library(dplyr)
library(ggplot2)
library(lubridate)

dat <- read.csv("GNDBHWQ_TSCindy.csv")
names(dat) <- tolower(names(dat))
dat <- dat %>% 
    select(datetimestamp, temp, spcond, sal, do_pct, do_mgl, depth, cdepth, ph, turb) %>%
    mutate(datetimestamp = mdy_hm(datetimestamp), depthft = depth * 3.28) %>%
    filter(., depth > 0.4)

plot(depth ~ datetimestamp, data=dat, type='l', col='blue')

baseline <- filter(dat, datetimestamp <= '2017-06-20 0:00')
plot(depth ~ datetimestamp, data=baseline, type='l', col='red')
high <- quantile(baseline$depth, 0.95)
low <- quantile(baseline$depth, 0.05)
highft <- quantile(baseline$depthft, 0.95)
lowft <- quantile(baseline$depthft, 0.05)
TSCindy <-mdy("6/20/2017")

dat2 <- filter(dat, datetimestamp >= '2017-06-10 0:00')

png('BH depth TS Cindy.png', width=12, height=6, units='in', res=400)
plot(depthft ~ datetimestamp, data=dat2, type='n', 
     main="Bayou Heron depth before and during TS Cindy",
     ylab="Sensor Depth (ft)",
     xlab="Date")
abline(h=highft, lty=2, col='firebrick3', lwd=2)
abline(h=lowft, lty=2, col='firebrick3', lwd=2)
abline(h=max(dat2$depthft), lty=2, lwd=2, col='darkorange1')
lines(x=dat2$datetimestamp, y=dat2$depthft, lwd=1.5, col='deepskyblue4')
text(x=min(dat2$datetimestamp), y=max(dat2$depthft), labels="orange line is max depth from TS Cindy \n \nred lines show 5th and 95th percentiles \nof depth from previous month", adj=c(0,1.2), cex=0.8)
dev.off()
