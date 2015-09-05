windData = read.csv("windCleanedH.csv",TRUE)
plotData = windData
plotCol = rgb(1-(abs(720-plotData$MIN_TIME)/720),(120 - (abs(720-plotData$MIN_TIME)/720)*100)/255,(abs(720-plotData$MIN_TIME)/720))
plot(plotData$HOUR_TIME, plotData$WIND_SPEED,
     col = plotCol, pch = 20,
     xlim = c(0,1440), ylim = c(0,15),
     xlab = "Time", ylab = "Speed"
)
grid(20,20)
title(paste("Data"))
