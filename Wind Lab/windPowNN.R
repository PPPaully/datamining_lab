library("neuralnet")

range = (1:9267)

windData = read.csv("windCleanedH.csv",TRUE)
windData = windData[range, c("POWER","WIND_SPEED", "WIND_MOV","SPD_MOV","HOUR_TIME","MIN_TIME")]
windData$POWER = sqrt(windData$POWER)

#############
# NORMALIZE
#############

HOUR_TIMEu = mean(windData$HOUR_TIME)
HOUR_TIMEsd = sd(windData$HOUR_TIME)
windData$HOUR_TIME = scale(windData$HOUR_TIME/100)

WIND_MOVu = mean(windData$WIND_MOV)
WIND_MOVsd = sd(windData$WIND_MOV)
windData$WIND_MOV = scale(windData$WIND_MOV)

SPD_MOVu = mean(windData$SPD_MOV)
SPD_MOVsd = sd(windData$SPD_MOV)
windData$SPD_MOV = scale(windData$SPD_MOV)



#################
# Neural Network
#################

inputList = c("WIND_SPEED","HOUR_TIME","WIND_MOV","SPD_MOV")
inputNames = paste("windData$",inputList,sep = "")
f = as.formula(paste("windData$POWER ~",paste(inputNames,collapse = "+")))

net = neuralnet(f, windData[,(2:(ncol(windData)-1))], hidden = 5, threshold = 1,
                # startweights = s_weights,
                lifesign = "full",lifesign.step = 1000,
                stepmax = 100000)


plotData = windData
plotCol = rgb(1-(abs(720-windData$MIN_TIME)/720),(120 - (abs(720-windData$MIN_TIME)/720)*100)/255,(abs(720-windData$MIN_TIME)/720))
x = plotData$WIND_SPEED
y = plotData$POWER
plot(x, y,
     col = plotCol, pch = 20,
     xlab = "Speed", ylab = "Power"
)
l = lm(y ~ x)
abline(l,lwd = 1)
grid(20,20)
title(paste("Data 2011-2013"))



nnData = as.data.frame(unlist(net["net.result"]))
rownames(nnData) = NULL
nnData = cbind(nnData,windData$WIND_SPEED)
colnames(nnData) = c("POWER","WIND_SPEED")
points(nnData$WIND_SPEED,nnData$POWER,col="green")
s_weights = net["weights"]
print(paste("NN MAPE :",sum(abs((nnData$POWER^2 - windData$POWER^2)/windData$POWER^2))/nrow(windData)))


#################################
# PREDICT BETA
#################################

range = (9268:13901)

windData = read.csv("windCleanedH.csv",TRUE)
windData = windData[range, c("POWER","WIND_SPEED", "WIND_MOV","SPD_MOV","HOUR_TIME","MIN_TIME")]
windData$POWER = sqrt(windData$POWER)

windData$HOUR_TIME = (windData$HOUR_TIME-HOUR_TIMEu)/HOUR_TIMEsd
windData$WIND_MOV = (windData$WIND_MOV-WIND_MOVu)/WIND_MOVsd
windData$SPD_MOV = (windData$SPD_MOV-SPD_MOVu)/SPD_MOVsd

predictData = as.data.frame(unlist(compute(net, covariate = windData[,(2:(ncol(windData)-1))])["net.result"]))
rownames(predictData) = NULL
predictData = cbind(predictData,windData$WIND_SPEED)
colnames(predictData) = c("POWER","WIND_SPEED")
points(windData$WIND_SPEED,windData$POWER,col="red")
points(predictData$WIND_SPEED,predictData$POWER,col="green")
print(paste("Predict MAPE :",sum(abs((predictData$POWER^2 - windData$POWER^2)/windData$POWER^2))/nrow(windData)))
