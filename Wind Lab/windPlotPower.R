windData = read.csv("windCleanedH.csv",TRUE)

# plotData = windData[!(600 <= windData$S_Time & windData$S_Time < 1800),]
plotData = windData[range, ]
# plotData = plotData[ order(plotData$WIND_SPEED), ]
plotCol = rgb(1-(abs(720-windData$MIN_TIME)/720),(120 - (abs(720-windData$MIN_TIME)/720)*100)/255,(abs(720-windData$MIN_TIME)/720))

x = plotData$WIND_SPEED # as.numeric(plotData$TIME_STAMP)*1440+plotData$MIN_TIME
y = sqrt(plotData$POWER)

# svg(width=500,height = 10)

plot(x, y,
     col = plotCol, pch = 20,
     xlab = "Speed", ylab = "Power"
)

# dev.off()

l = lm(y ~ x)
abline(l,lwd = 1)
grid(20,20)
title(paste("Data 2011-2013"))