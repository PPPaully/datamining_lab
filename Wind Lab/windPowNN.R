library("neuralnet")

range = (1:9267)

windData = read.csv("windCleanedH.csv",TRUE)
windData = windData[range, c("POWER","WIND_SPEED", "WIND_MOV","SPD_MOV","HOUR_TIME","MIN_TIME")]
windData$POWER = sqrt(windData$POWER)

windData$HOUR_TIME = scale(windData$HOUR_TIME/100)
windData$WIND_MOV = scale(windData$WIND_MOV)
windData$SPD_MOV = scale(windData$SPD_MOV)


inputList = c("WIND_SPEED","HOUR_TIME","WIND_MOV","SPD_MOV")
inputNames = paste("windData$",inputList,sep = "")
f = as.formula(paste("windData$POWER ~",paste(inputNames,collapse = "+")))

net = neuralnet(f, windData[,(2:(ncol(windData)-1))], hidden = 5, threshold = 1,
                startweights = s_weights,
                lifesign = "full",lifesign.step = 500,
                stepmax = 1000000)

nnData = as.data.frame(unlist(net["net.result"]))
rownames(nnData) = NULL
nnData = cbind(nnData,windData$WIND_SPEED)
colnames(nnData) = c("POWER","WIND_SPEED")
points(nnData$WIND_SPEED,nnData$POWER,col="green")
s_weights = net["weights"]
