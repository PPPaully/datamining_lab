library("neuralnet")

k = 3
n_testcase = 0
error_sum = 0
windData = read.csv("windCleanedH.csv",TRUE)
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
  
  net = neuralnet(f, trainingData[,(2:(ncol(trainingData)-1))], hidden = 5, threshold = 10,
                  # startweights = s_weights,
                  lifesign = "full",lifesign.step = 1000,
                  stepmax = 100000)
  
  
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
  
  
  
  nnData = as.data.frame(unlist(net["net.result"]))
  rownames(nnData) = NULL
  nnData = cbind(nnData,trainingData$WIND_SPEED)
  colnames(nnData) = c("POWER","WIND_SPEED")
  points(nnData$WIND_SPEED,nnData$POWER,col="green")
  s_weights = net["weights"]
  print(paste("NN MAPE :",sum(abs((nnData$POWER^2 - trainingData$POWER^2)/trainingData$POWER^2))/nrow(trainingData)))

  #################################
  # PREDICT BETA
  #################################
  
  
  # windData = read.csv("windCleanedH.csv",TRUE)
  # windData = windData[range, c("POWER","WIND_SPEED", "WIND_MOV","SPD_MOV","HOUR_TIME","MIN_TIME")]
  # windData$POWER = sqrt(windData$POWER)
  
  testingData$HOUR_TIME = (testingData$HOUR_TIME-HOUR_TIMEu)/HOUR_TIMEsd
  testingData$WIND_MOV = (testingData$WIND_MOV-WIND_MOVu)/WIND_MOVsd
  testingData$SPD_MOV = (testingData$SPD_MOV-SPD_MOVu)/SPD_MOVsd
  
  predictData = as.data.frame(unlist(compute(net, covariate = testingData[,(2:(ncol(testingData)-1))])["net.result"]))
  rownames(predictData) = NULL
  predictData = cbind(predictData,testingData$WIND_SPEED)
  colnames(predictData) = c("POWER","WIND_SPEED")
  points(testingData$WIND_SPEED,testingData$POWER,col="red")
  points(predictData$WIND_SPEED,predictData$POWER,col="green")
  #print(paste("Predict MAPE :",sum(abs((predictData$POWER^2 - testingData$POWER^2)/testingData$POWER^2))/nrow(testingData)))
  error_sum = error_sum + sum(abs((predictData$POWER^2 - testingData$POWER^2)/testingData$POWER^2))
  n_testcase = n_testcase + nrow(testingData)
}
mape = error_sum / n_testcase
print(paste("Predict MAPE :",mape))
