windData = read.csv("windCleaned.csv",TRUE)
# svg(width=500)

# for (y in (2011:2013)) {
#   for (x in (1:12)) {
    plotData = windData
    plotCol = rgb(1-(abs(1200-plotData$S_Time)/1200),(120 - (abs(1200-plotData$S_Time)/1200)*100)/255,(abs(1200-plotData$S_Time)/1200))
    # if(!(y == 2013 & x > 3) & !(y==2011 & x <= 2)) {
      plot((as.numeric(plotData$TIME_STAMP)-1)*240 + plotData$S_Time/10, plotData$WIND_SPEED,
           col = plotCol, pch = 20,
           ylim = c(0,16),
           xlab = "Time", ylab = "Speed",
           panel.first = grid(1000,16)
          )
#       plot(plotData$WIND_SPEED, plotData$POWER,
#            col = plotCol, pch = 20,
#            xlim = c(0,10), ylim = c(0,1000),
#            xlab = "Speed", ylab = "Power",
#            panel.first = grid(20,20)
#       )
#       title(paste("Data @ Month",x,"Year",y))
#     }
#  }
# }

# dev.off()