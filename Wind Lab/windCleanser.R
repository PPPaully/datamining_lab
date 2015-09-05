printf <- function(...)print(sprintf(...))

windData <- read.csv("windEdit.csv",header = TRUE)
rawDMY <- as.numeric(unlist(strsplit(toString(windData$TIME_STAMP),"[^0-9]")))
rawDMY <- rawDMY[!is.na(rawDMY)]
DMY <- matrix(rawDMY, ncol = 3, byrow = TRUE)
colnames(DMY) <- c("DAY", "MONTH", "YEAR")

rawTIME <- as.numeric(unlist(strsplit(toString(windData$TIME_SPAN),"[^0-9]")))
rawTIME <- rawTIME[!is.na(rawTIME)]
TIME <- matrix(rawTIME, ncol = 4, byrow = TRUE)
TIME <- formatC(TIME, width=2, flag="0")
TIME <- cbind(as.numeric(TIME[,1])*60 + as.numeric(TIME[,2]),as.numeric(paste(TIME[,1],TIME[,2],sep = "")))
colnames(TIME) <- c("MIN_TIME","HOUR_TIME")

date <- as.Date(windData$TIME_STAMP[1],"%d/%m/%Y")
d <- as.numeric(format(date,"%d"))
m <- as.numeric(format(date,"%m"))
DATES <- matrix(nrow=nrow(DMY))

for (i in (1:nrow(DMY))) {
  while(DMY[i,"DAY"] != d || DMY[i,"MONTH"] != m) {
    if(DMY[i,"DAY"] == m && DMY[i,"MONTH"] == d) { # Swap day-month 
      DMY[i,"DAY"] <- d
      DMY[i,"MONTH"] <- m
    }
    else { # if not then date+1
      date <- date + 1
      d <- as.numeric(format(date,"%d"))
      m <- as.numeric(format(date,"%m"))
      if(DMY[i,"DAY"] == m && DMY[i,"MONTH"] == d) {
        DMY[i,"DAY"] <- d
        DMY[i,"MONTH"] <- m
      }
    }
  }
  DATES[i] <- toString(date)
}

DATES <- as.Date(DATES)
windData$TIME_STAMP <- DATES
windData <- cbind(windData,DMY,TIME)

#####################
# CLEAR NA WIND_DIR
#####################
windData = windData[windData$POWER > 0,]
NADir = windData[is.na(windData$WIND_DIR),]
NADir[,"WIND_DIR"] = 0
windData[rownames(NADir),] = NADir
windData$WIND_DIR = windData$WIND_DIR %% 360

#####################
# CLEAR Infected Data
#####################
windData = windData[-c(71133,71171), ]

#####################   
# Calcurate WIND_MOV & SPD_MOV
#####################
WIND_MOV = matrix(ncol = 1,nrow = nrow(windData))
WIND_MOV[1] = 0
SPD_MOV = matrix(ncol = 1, nrow = nrow(windData))
SPD_MOV[1] = 0
for(i in (2:nrow(windData))) {
  timeDiff = ((as.numeric(as.Date(windData[i,"TIME_STAMP"]))-as.numeric(as.Date(windData[i-1,"TIME_STAMP"])))*1440 + windData[i,"MIN_TIME"] - windData[i-1,"MIN_TIME"])
  dirDiff = abs(windData[i,"WIND_DIR"] - windData[i-1,"WIND_DIR"])
  spdDiff = windData[i,"WIND_SPEED"] - windData[i-1,"WIND_SPEED"]
  if(dirDiff > 180) dirDiff = 360 - dirDiff
  WIND_MOV[i] = dirDiff/timeDiff
  SPD_MOV[i] = spdDiff/timeDiff
}
colnames(WIND_MOV) = c("WIND_MOV")
colnames(SPD_MOV) = c("SPD_MOV")
windData = cbind(windData,WIND_MOV)
windData = cbind(windData,SPD_MOV)

write.csv(windData, file = "windCleaned.csv", row.names = FALSE)
