library('forecast')
windData = read.csv("windCleanedH.csv",header = TRUE)
k = 3
windSpeedData = as.data.frame(windData[,"WIND_SPEED"])
testing_range = c(13878:13901)
colnames(windSpeedData) = "WIND_SPEED"
testing_set = as.data.frame(windSpeedData[testing_range,"WIND_SPEED"])
training_set = as.data.frame(windSpeedData[-testing_range,"WIND_SPEED"])
# training_set= scale(training_set)
# 
# model = Arima(training_set,order = c(1,2,1)) 
# predict_data = as.data.frame(predict(model,24))


