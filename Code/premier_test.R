
#*************************************************************
data<- read.csv2("TrainSample.csv", header=TRUE) 
dataTest <- read.csv2("TestSample.csv",header=TRUE)

#pairs(data[,40:47])

for(i in 1:ncol(data)){
  if(is.numeric(data[,i])){
    data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)}
}
#Data1<-data[-c(4,5,6)]
#cor(data[-c(4,5,6)])
#plot(cor(data[-c(4,5,6)]))
#regression<-lm(data1$OilCum360~ . ,data1)
#Ychap<-t(regression$coefficients)*data1[-c(44)]

OilModel <- lm(data$OilCum360 ~ ., data)
GasModel <- lm(data$GasCum360 ~ ., data)
testOil <- predict(OilModel, newdata=dataTest, interval ='pre')
testGas <- predict(GasModel, newdata=dataTest, interval ='pre')

results<-data.frale(ID = test$API, 
                    CUM360_INF = testOil[,2],
                    CUM360_SUP = testOil[,3],
                    GAS360_INF = testGas[,2],
                    GAS360_SUP = testGas[,3])
