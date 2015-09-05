windData = read.csv("windCleaned.csv",TRUE)
SAVETOFILE = FALSE
windData <- windData[,-c(2)]
# initialDate <- windData[1,"TIME_STAMP"]
# endDate <- windData[nrow(windData),"TIME_STAMP"]
cleanData <- matrix(ncol = 10,nrow = 0)
colnames(cleanData) <- c("TIMESTAMP","WIND_SPEED","WIND_DIR","POWER","DAY","MONTH","YEAR","MIN_TIME","HOUR_TIME","WIND_MOV")
to_ten_degree <- function(pure_degree){
  return (as.integer((pure_degree+5)%/%10+1))
}
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
initial_row = 2
for(i in 2:nrow(windData)){
  if(windData[initial_row,"HOUR_TIME"] %/%100 != windData[i,"HOUR_TIME"] %/% 100){
    cleanData <- rbind(cleanData,windData[initial_row,])
    cleanData[nrow(cleanData),"WIND_SPEED"] = mean(windData[c(initial_row:(i-1)),"WIND_SPEED"])
    cleanData[nrow(cleanData),"POWER"] = mean(windData[c(initial_row:(i-1)),"POWER"])
    cleanData[nrow(cleanData),"WIND_MOV"] = mean(windData[c(initial_row:(i-1)),"WIND_MOV"])
    cleanData[nrow(cleanData),"WIND_DIR"] = Mode(to_ten_degree(windData[c(initial_row:(i-1)),"WIND_DIR"]))
    initial_row <- i
    
  }
}
if(SAVETOFILE)
  write.csv(cleanData,file="windCleanedH.csv")