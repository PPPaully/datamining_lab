library("neuralnet")

#########
#SETTING
#########
k = 3
thresholdRange = (10:3)
saveToCSV = TRUE
plot = FALSE
printTrace = TRUE
printResult = FALSE

for(hiddenNode in (1:5)) {
  
  fileCSV = data.frame()
  for(z in 1:7)
    fileCSV[1,z] = NA
  colnames(fileCSV) = c("Threshold","NN MAPE1","NN MAPE2","NN MAPE3","Predict MAPE","SD Predict MAPE","Elapsed Time")
  row = 1
  
  for(cnr in (0:7)) {
    
    #########
    #SETTING
    #########
    firstRun = TRUE
    
    inputList = c()
    inputList = append(inputList,"WIND_SPEED")
    if(bitwAnd(cnr,1))inputList = append(inputList,"HOUR_TIME")
    if(bitwAnd(cnr,2))inputList = append(inputList,"WIND_MOV")
    if(bitwAnd(cnr,4))inputList = append(inputList,"SPD_MOV")
    cat("\n",inputList,"\n")
    inputNames = paste("trainingData$",inputList,sep = "")
    f = as.formula(paste("trainingData$POWER ~",paste(inputNames,collapse = "+")))
    
    
    elapsedTime = 0
    if(firstRun)s_weights = NULL
    if(printTrace)cat("Threshold : ")
    windData = read.csv("windCleanedH.csv",TRUE)
    for(th in thresholdRange){
      ###########
      # SETTING
      ###########
      thresholdEquation = 2^th
      
      if(printTrace)cat(thresholdEquation," ")
      error_sum = c()
      fileCSV[row,"Threshold"] = thresholdEquation
      for(i in 0:(k-1)){
        ##############
        # Partition
        ##############
        testing_range = c(1:(nrow(windData)%/%k)) + i * (nrow(windData)%/%k)
        trainingData = windData[-testing_range,append(c("POWER","MIN_TIME"),inputList)]
        testingData = windData[testing_range,append(c("POWER","MIN_TIME"),inputList)]
        trainingData$POWER = sqrt(trainingData$POWER)
        testingData$POWER = sqrt(testingData$POWER)
        
        #############
        # NORMALIZE Training data
        #############
        
        if("HOUR_TIME" %in% inputList) {
          HOUR_TIMEu = mean(trainingData$HOUR_TIME)
          HOUR_TIMEsd = sd(trainingData$HOUR_TIME)
          trainingData$HOUR_TIME = scale(trainingData$HOUR_TIME/100)
        }
        
        if("WIND_MOV" %in% inputList) {
          WIND_MOVu = mean(trainingData$WIND_MOV)
          WIND_MOVsd = sd(trainingData$WIND_MOV)
          trainingData$WIND_MOV = scale(trainingData$WIND_MOV)
        }
        
        if("SPD_MOV" %in% inputList) {
          SPD_MOVu = mean(trainingData$SPD_MOV)
          SPD_MOVsd = sd(trainingData$SPD_MOV)
          trainingData$SPD_MOV = scale(trainingData$SPD_MOV)
        }
        
        
        ##################
        # Neural Network
        ##################
        startTime = proc.time()
        if(firstRun) { 
          net = neuralnet(f, trainingData[,inputList], hidden = hiddenNode, threshold = thresholdEquation,stepmax = 100000)
          firstRun = FALSE
        }
        else
          net = neuralnet(f, trainingData[,inputList], hidden = hiddenNode, threshold = thresholdEquation,startweights = s_weights,stepmax = 100000)
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
          title(paste("Data 2011-2013 MAPE",(i+1)))
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
        
        if("HOUR_TIME" %in% inputList)testingData$HOUR_TIME = (testingData$HOUR_TIME-HOUR_TIMEu)/HOUR_TIMEsd
        if("WIND_MOV" %in% inputList)testingData$WIND_MOV = (testingData$WIND_MOV-WIND_MOVu)/WIND_MOVsd
        if("SPD_MOV" %in% inputList)testingData$SPD_MOV = (testingData$SPD_MOV-SPD_MOVu)/SPD_MOVsd
        
        predictData = as.data.frame(unlist(compute(net, covariate = testingData[,inputList])["net.result"]))
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
    fileCSV[row,1] = paste(substr(inputList[1:length(inputList)],1,1),collapse = "")
    row = row + 1
  }
  if(saveToCSV)
    write.csv(fileCSV,
              paste("SummaryHid",hiddenNode,".csv",sep=""),
              row.names = FALSE)
  cat("\n","COMPLETE for Hidden",hiddenNode,"\n")
  hiddenNode = hiddenNode + 1
}