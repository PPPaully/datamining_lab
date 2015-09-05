windData = read.csv("windCleaned.csv",TRUE)
windData$TIME_STAMP = as.Date(windData$TIME_STAMP)

SDate = as.Date(windData[1,"TIME_STAMP"])
dayTrend = matrix(nrow = 0,ncol = 2)
nightTrend = matrix(nrow = 0,ncol = 2)
diffCoef = matrix(nrow = 0, ncol = 2)
dayLinears = list()
NightLinears = list()

plotDayLine = FALSE
plotNightLine = FALSE

codiff = 0

plotDay = windData[windData$TIME_STAMP == SDate & (600 <= windData$HOUR_Time & windData$HOUR_Time < 1800), ]
plotDay$HOUR_Time = plotDay$HOUR_Time - 600
plotDay$HOUR_Time = (60*as.integer(plotDay$HOUR_Time/100) + (plotDay$HOUR_Time%%100))/10
if(plotDayLine) {
  plot(plotDay$HOUR_Time,plotDay$WIND_SPEED, col="orange", ylim = c(0,15), xlab = "Time(6.00 - 18.00)", ylab = "Wind Speed")
  title(paste("Linear Regression @",SDate))
}
x = plotDay$HOUR_Time
y = plotDay$WIND_SPEED
l = lm(y ~ x)
dayLinears[[length(dayLinears)+1]] = l
dayTrend = rbind(dayTrend,coef(l))
codiff = coef(l)[1]
if(plotDayLine) {
  xx = seq(0,1200,length = nrow(plotDay))
  lines(xx, predict(l, data.frame(x=xx)), col="red")
}

plotNight = windData[(windData$TIME_STAMP == SDate & windData$HOUR_Time >= 1800) | (windData$TIME_STAMP == SDate+1 & windData$HOUR_Time < 600), ]
plotNight$HOUR_Time = (plotNight$HOUR_Time + 600) %% 2400
plotNight$HOUR_Time = (60*as.integer(plotNight$HOUR_Time/100) + (plotNight$HOUR_Time%%100))/10
if(plotNightLine) {
  plot(plotNight$HOUR_Time,plotNight$WIND_SPEED, col="blue", ylim = c(0,15), xlab = "Time(18.00 - 6.00(TMR))", ylab = "Wind Speed")
  title(paste("Linear Regression @",SDate))
}
x = plotNight$HOUR_Time
y = plotNight$WIND_SPEED
l = lm(y ~ x)
# NightLinears = append(NightLinears,l)
nightTrend = rbind(nightTrend, coef(l))
if(nrow(plotDay) > 0) {
  codiff = codiff - coef(l)[1]
  diffCoef = append(diffCoef,codiff)
}
if(plotNightLine) {
  xx = seq(0,1200,length = nrow(plotNight))
  lines(xx, predict(l, data.frame(x=xx)), col="red")
}

SDate = SDate + 1

while (SDate < as.Date(windData[nrow(windData),"TIME_STAMP"])) {
  plotDay = windData[windData$TIME_STAMP == SDate & (600 <= windData$HOUR_Time & windData$HOUR_Time < 1800), ]
  if(nrow(plotDay) > 0) {
    plotDay$HOUR_Time = plotDay$HOUR_Time - 600
    plotDay$HOUR_Time = (60*as.integer(plotDay$HOUR_Time/100) + (plotDay$HOUR_Time%%100))/10
    if(plotDayLine)points(plotDay$HOUR_Time,plotDay$WIND_SPEED, col="orange")
    x = plotDay$HOUR_Time
    y = plotDay$WIND_SPEED
    l = lm(y ~ x)
    # dayLinears[[length(dayLinears)+1]] = l
    dayTrend = rbind(dayTrend,coef(l))
    codiff = coef(l)[1]
    if(plotDayLine) {
      xx = seq(0,1200,length = nrow(plotDay))
      lines(xx, predict(l, data.frame(x=xx)), col="red")
    }
  }
  
  plotNight = windData[(windData$TIME_STAMP == SDate & windData$HOUR_Time >= 1800) | (windData$TIME_STAMP == SDate+1 & windData$HOUR_Time < 600), ]
  if(nrow(plotNight) > 0) {
    plotNight$HOUR_Time = (plotNight$HOUR_Time + 600) %% 2400
    plotNight$HOUR_Time = (60*as.integer(plotNight$HOUR_Time/100) + (plotNight$HOUR_Time%%100))/10
    if(plotNightLine)points(plotNight$HOUR_Time,plotNight$WIND_SPEED, col="blue")
    x = plotNight$HOUR_Time
    y = plotNight$WIND_SPEED
    l = lm(y ~ x)
    # NightLinears = append(NightLinears,l)
    nightTrend = rbind(nightTrend, coef(l))
    if(nrow(plotDay) > 0) {
      codiff = codiff - coef(l)[1]
      diffCoef = append(diffCoef,codiff)
    }
    if(plotNightLine) {
      xx = seq(0,1200,length = nrow(plotNight))
      lines(xx, predict(l, data.frame(x=xx)), col="red")
    }
  }
  SDate = SDate + 1
}

limx = c(-5,15)
limy = c(-0.2,0.2)
plot(dayTrend, col="orange", xlim=limx, ylim=limy, xlab = "Init. WindSpeed", ylab = "Slope")
title("Day Time")
plot(nightTrend, col="blue", xlim=limx, ylim=limy, xlab = "Init. WindSpeed", ylab = "Slope")
title("Night Time")
plot(dayTrend, col="orange", xlim=limx, ylim=limy, xlab = "Init. WindSpeed", ylab = "Slope")
points(nightTrend, col="blue", xlim=limx, ylim=limy, xlab = "Init. WindSpeed", ylab = "Slope")
title("Day-Night Time")
plot(diffCoef, col="red", xlab = "Date", ylab = "dSlope")
title("Different Slope between Day-Night")