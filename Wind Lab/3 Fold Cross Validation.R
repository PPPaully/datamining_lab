library("neuralnet")

########
#SETTING
########
k = 3
thresholdRange = (100:80)
saveToCSV = FALSE
plot = FALSE
printResult = FALSE


fileCSV = matrix(nrow = length(thresholdRange), ncol = 7)
colnames(fileCSV) = c("Threshold","NN MAPE1","NN MAPE2","NN MAPE3","Predict MAPE","SD Predict MAPE","Elapsed Time")
elapsedTime = 0
s_weights = NULL
firstRun = TRUE
windData = read.csv("windCleanedH.csv",TRUE)
row = 1
for(th in thresholdRange){
  print(paste("Threshold :",th))
  error_sum = c()
  fileCSV[row,"Threshold"] = th
  for(i in 0:(k-1)){
    testing_range = c(1:(nrow(windData)%/%k)) + i * (nrow(windData)%/%k)
    trainingData = windData[-testing_range,c("POWER","WIND_SPEED", "WIND_MOV","SPD_MOV","HOUR_TIME","MIN_TIME")]
    testingData = windData[testing_range,c("POWER","WIND_SPEED", "WIND_MOV","SPD_MOV","HOUR_TIME","MIN_TIME")]
    trainingData$POWER = sqrt(trainingData$POWER)
    testingData$POWER = sqrt(testingData$POWER)
    #############
    # NORMALIZE Training data
    #############
    
    HOUR_TIMEu = mean(trainingData$HOUR_TIME)
    HOUR_TIMEsd = sd(trainingData$HOUR_TIME)
    trainingData$HOUR_TIME = scale(trainingData$HOUR_TIME/100)
    
    WIND_MOVu = mean(trainingData$WIND_MOV)
    WIND_MOVsd = sd(trainingData$WIND_MOV)
    trainingData$WIND_MOV = scale(trainingData$WIND_MOV)
    
    SPD_MOVu = mean(trainingData$SPD_MOV)
    SPD_MOVsd = sd(trainingData$SPD_MOV)
    trainingData$SPD_MOV = scale(trainingData$SPD_MOV)
    
    
    
    #################
    # Neural Network
    #################
    
    inputList = c("WIND_SPEED","HOUR_TIME","WIND_MOV","SPD_MOV")
    inputNames = paste("trainingData$",inputList,sep = "")
    f = as.formula(paste("trainingData$POWER ~",paste(inputNames,collapse = "+")))
    
    startTime = proc.time()
    if(firstRun) { 
      net = neuralnet(f, trainingData[,(2:(ncol(trainingData)-1))], hidden = 5, threshold = th,stepmax = 100000)
      firstRun = FALSE
    }
    else
      net = neuralnet(f, trainingData[,(2:(ncol(trainingData)-1))], hidden = 5, threshold = th,startweights = s_weights,stepmax = 100000)
    elapsedTime = (proc.time() - startTime) + elapsedTime
    
    if(plot) {
      plotData = trainingData
      plotCol = rgb(1-(abs(720-trainingData$MIN_TIME)/720),(120 - (abs(720-trainingData$MIN_TIME)/720)*100)/255,(abs(720-trainingData$MIN_TIME)/720))
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
    }
    
    nnData = as.data.frame(unlist(net["net.result"]))
    rownames(nnData) = NULL
    nnData = cbind(nnData,trainingData$WIND_SPEED)
    colnames(nnData) = c("POWER","WIND_SPEED")
    if(plot)points(nnData$WIND_SPEED,nnData$POWER,col="green")
    s_weights = net["weights"]
    fileCSV[row,paste("NN MAPE",(i+1),sep="")] = sum(abs((nnData$POWER^2 - trainingData$POWER^2)/trainingData$POWER^2))/nrow(trainingData)
    if(printResult)print(paste("NN MAPE",(i+1),":",fileCSV[row,paste("NN MAPE",(i+1),sep="")]))
  
    #################################
    # PREDICT BETA
    #################################
    
    testingData$HOUR_TIME = (testingData$HOUR_TIME-HOUR_TIMEu)/HOUR_TIMEsd
    testingData$WIND_MOV = (testingData$WIND_MOV-WIND_MOVu)/WIND_MOVsd
    testingData$SPD_MOV = (testingData$SPD_MOV-SPD_MOVu)/SPD_MOVsd
    
    predictData = as.data.frame(unlist(compute(net, covariate = testingData[,(2:(ncol(testingData)-1))])["net.result"]))
    rownames(predictData) = NULL
    predictData = cbind(predictData,testingData$WIND_SPEED)
    colnames(predictData) = c("POWER","WIND_SPEED")
    if(plot) {
      points(testingData$WIND_SPEED,testingData$POWER,col="red")
      points(predictData$WIND_SPEED,predictData$POWER,col="green")
    }
    error_sum = append(error_sum,abs((predictData$POWER^2 - testingData$POWER^2)/testingData$POWER^2))
  }
  mape = mean(error_sum)
  SDmape = sd(error_sum)
  fileCSV[row,"Predict MAPE"] = mape
  fileCSV[row,"SD Predict MAPE"] = SDmape
  fileCSV[row,"Elapsed Time"] = as.double(elapsedTime[3])
  if(printResult) {
    print(paste("Predict MAPE :",mape))
    print(paste("SD Predict MAPE :",SDmape))
    print(paste("Elapsed Time :",as.double(elapsedTime[3])))
  }
  row = row + 1
}

if(saveToCSV)
  write.csv(fileCSV,"Summary.csv",row.names = FALSE)