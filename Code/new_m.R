##new models
source("./pretraitement3.R")
source("./finalisation.R")
source("./predInterval_bootstrap.R")
train <- read.csv2("./data/TrainSample.csv") 
test <- read.csv2("./data/TestSample.csv")
library(data.table) #Faster reading
library(xgboost)
library(caret)
ind.test <- sample(1:460, size=100, replace = FALSE)
data.test <- dataTrain[ind.test,]
data.train <- dataTrain[-ind.test,]

sample <- data.train
test <- data.test
solution <- test[,c(ncol(test)-1,ncol(test))] 
API.test <- dataTrain.API[ind.test]
API.train <- dataTrain.API[-ind.test]
sampleOil<-sampleGas<-sample
gas <- sampleGas$GasCum360
oil <- sampleOil$OilCum360
sampleOil$GasCum360<-NULL
sampleGas$OilCum360<-NULL



#index <- createDataPartition(train$OilCum360, p=0.75, list=FALSE)
trainSet_oil <- sampleOil
testSet_oil <- test[-c(45)]
trainSet_gas <- sampleGas
testSet_gas <- test[-c(46)]

#Feature selection using rfe in caret
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 3,
                      verbose = FALSE)

outcomeName_oil<-'OilCum360'
outcomeName_gas<-'GasCum360'
predictors_oil<-names(trainSet_oil)[!names(trainSet_oil) %in% outcomeName_oil]
predictors_gas<-names(trainSet_gas)[!names(trainSet_gas) %in% outcomeName_gas]

Loan_Pred_Profile_gas <- rfe(trainSet_gas[,predictors_gas], trainSet_gas[,outcomeName_gas],
                         rfeControl = control)
Loan_Pred_Profile_gas

Loan_Pred_Profile_oil <- rfe(trainSet_oil[,predictors_oil], trainSet_oil[,outcomeName_oil],
                             rfeControl = control)
Loan_Pred_Profile_oil
names(getModelInfo())

# model_gbm_gas<-train(trainSet_gas[,predictors_gas],trainSet[,outcomeName_gas],method='gbm')
model_rf_gas<-train(trainSet_gas[,predictors_gas],trainSet_gas[,outcomeName_gas],method='rf')
# model_xgbTree_gas<-train(trainSet_gas[,predictors_gas],trainSet[,outcomeName_gas],method='xgbTree')
# model_treebag_gas<-train(trainSet_gas[,predictors_gas],trainSet[,outcomeName_gas],method='treebag')
# model_widekernelpls_gas<-train(trainSet_gas[,predictors_gas],trainSet[,outcomeName_gas],method='widekernelpls')
# model_spls_gas<-train(trainSet_gas[,predictors_gas],trainSet[,outcomeName_gas],method='spls')
# model_RRFglobal_gas<-train(trainSet_gas[,predictors_gas],trainSet[,outcomeName_gas],method='RRFglobal')
# #model_rneuralnet_gas<-train(trainSet_gas[,predictors_gas],trainSet[,outcomeName_gas],method='neuralnet')
# model_kernelpls_gas<-train(trainSet_gas[,predictors_gas],trainSet[,outcomeName_gas],method='kernelpls')
# model_krlsPoly_gas<-train(trainSet_gas[,predictors_gas],trainSet[,outcomeName_gas],method='krlsPoly')
# #model_blackboost_gas<-train(trainSet_gas[,predictors_gas],trainSet[,outcomeName_gas],method='blackboost')
# 



# modelLookup(model='gbm')
# fitControl <- trainControl(
#   method = "repeatedcv",
#   number = 50,
#   repeats = 50)
# model_gbm_gas<-train_gas(trainSet_gas[,predictors_gas],trainSet_gas[,outcomeName_gas],method='gbm',trControl=fitControl,tuneLength=10)
# modelLookup(model='gbm')

modelLookup(model='rf')
# print(model_gbm_gas)
# plot(model_gbm_gas)

print(model_rf_gas)
plot(model_rf_gas)
# 
# #Variable Importance
# varImp(object=model_gbm_gas)
# 
# # #Plotting Varianle importance for GBM
# # plot(varImp(object=model_gbm_gas),main="GBM - Variable Importance")
# 
# varImp(object=model_rf_gas)
# 
# plot(varImp(object=model_rf_gas),main="RF - Variable Importance")
# ####################################################################


