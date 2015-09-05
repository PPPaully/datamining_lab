windData = read.csv("windCleaned.csv",TRUE)

pwLim = c()

init = 1
cutZone = 10
nCut = 5
SAVETOFILE = TRUE

# sd(windData[c(init:i),"POWER"])

for(i in (2:nrow(windData))) {
  if(max(windData[c(init:i),"POWER"]) - min(windData[c(init:i),"POWER"]) < cutZone & i - init > nCut) {
  }
  else if(max(windData[c(init:i),"POWER"]) - min(windData[c(init:i),"POWER"]) > cutZone) {
    if(i - init > nCut)pwLim = append(pwLim,c(init:i-1))
    init = i-1
  }
}

plotData = windData[pwLim, ]
plotData = plotData[ order(plotData$WIND_SPEED), ]
plotCol = rgb(1-(abs(720-plotData$MIN_TIME)/720),(120 - (abs(720-plotData$MIN_TIME)/720)*100)/255,(abs(720-plotData$MIN_TIME)/720))


x = plotData$WIND_SPEED
y = (plotData$POWER)

plot(x, y,
     col = plotCol, pch = 20,
     xlim = c(0,15), ylim = c(0,1500),
     xlab = "Speed", ylab = "Power"
)
l = lm(y ~ x)
abline(l,lwd = 1)
grid(20,20)
title(paste("Delete Data"))

plotData = windData[-pwLim, ]

x = plotData$WIND_SPEED
y = (plotData$POWER)

plot(x, y,
     col = plotCol, pch = 20,
     xlim = c(0,15), ylim = c(0,1500),
     xlab = "Speed", ylab = "Power"
)
l = lm(y ~ x)
abline(l,lwd = 1)
grid(20,20)
title(paste("Deleted Data"))

if(SAVETOFILE) {
  windData = windData[-pwLim, ]
  write.csv(windData, file = "windCleaned.csv", row.names = FALSE)
}
