dummywdir <- seq(0:359)

converted <- ifelse(dummywdir <= 270, 270 - dummywdir, 630 - dummywdir)

both <- cbind(dummywdir, converted)
both <- data.frame(both)
names(both) <- c("wdir", "converted")
write.csv(both, "convertedwdir.csv", row.names = FALSE)
getwd()

library(shape)
library(dplyr)

NE <- both %>%
    filter(wdir <= 90)
SE <- filter(both, wdir > 90, wdir <= 180)

SW <- filter(both, wdir > 180, wdir <= 270)
NW <- filter(both, wdir > 270)

par(mfrow = c(4, 1))

plot(converted~wdir, data=NE, type="n", ylim=c(-10, 10), main="NE")
Arrowhead(x0=NE$wdir,
          y0=0,
          angle=NE$converted,
          arr.type="triangle",
          arr.width=0.15,
          arr.length=0.3,
          arr.lwd = 1,
          arr.col="cadetblue3")

plot(converted~wdir, data=SE, type="n", ylim=c(-10, 10), main="SE")
Arrowhead(x0=SE$wdir,
          y0=0,
          angle=SE$converted,
          arr.type="triangle",
          arr.width=0.15,
          arr.length=0.3,
          arr.lwd = 1,
          arr.col="cadetblue3")

plot(converted~wdir, data=SW, type="n", ylim=c(-10, 10), main="SW")
Arrowhead(x0=SW$wdir,
          y0=0,
          angle=SW$converted,
          arr.type="triangle",
          arr.width=0.15,
          arr.length=0.3,
          arr.lwd = 1,
          arr.col="cadetblue3")

plot(converted~wdir, data=NW, type="n", ylim=c(-10, 10), main="NW")
Arrowhead(x0=NW$wdir,
          y0=0,
          angle=NW$converted,
          arr.type="triangle",
          arr.width=0.05,
          arr.length=0.1,
          arr.lwd = 1,
          arr.col="cadetblue3")
par(mfcol = c(1, 1))
