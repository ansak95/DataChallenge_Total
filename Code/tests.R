source("./pretraitement3.R")
source("./finalisation.R")
source("./predInterval_bootstrap.R")
#importer les données
#train <- read.csv2("./data/TrainSample.csv") 
#validation <- read.csv2("./data/TestSample.csv")
#importation brute
data <-ImportData()
dataTrain <- data$train
dataTest1 <- data$validation
dataTestResults <- data$validResults

#pretraitement des données
dataTest.API<-dataTest1$API
dataTrain.API <- dataTrain$API
dataTrain<-preTraitement(dataTrain, 'train')
dataTest <-preTraitement(dataTest1, 'test')

#data.train <- dataTrain
#Cross des données en 360 / 100 enregistrements
ind.test <- sample(1:460, size=100, replace = FALSE)
data.test <- dataTrain[ind.test,]
data.train <- dataTrain[-ind.test,]

#on travaillera par la suite avec data.train 

#traitement des données
data.train_results<-data.train[c(45,46)]
data.gas<-data.train[-c(46)]
data.oil<-data.train[-c(45)]

API.data<-dataTrain.API[-ind.test]
API.vali<-dataTrain.API[ind.test]

#data.gas<-preTraitement(data.gas,"train")$data
#data.oil<-preTraitement(data.oil,"train")$data


#data.train<-cbind(data.gas,data.oil$OilCum360)

#data.test<- preTraitement(data.test,"test")
library(xgboost)
##xgboost
###############################################
data.gas_xgboost <- data.matrix(sampleGas[-c(45)])
gas <- data.matrix(sampleGas[c(45)])

param <- list(max_depth = 7, eta = 0.01, nthread = 3, 
              nrounds = 2000, objective = "reg:linear",
              eval_metric ='rmse')

GasModel1 <- xgboost(data = data.gas_xgboost, label = gas, 
                     max_depth = 7, eta = 0.1, nthread = 3, 
                     nrounds = 2000, objective = "reg:linear",
                     eval_metric ='rmse')
toPredict <- data.matrix(data.test[,-c(45,46)])
predGas <- predict(GasModel1, toPredict)
plot(predGas,gas_test)
#abline(a=0,b=1)
legend(-1, 4,legend=paste(names(param),param,sep="="),cex = 0.6)
err <- sum(abs(predGas-gas_test))
title("xgboost",paste("err",err,sep="="))
############################################################
#intervalles de prédictions
#params =list(quant_alpha,quant_delta,quant_thres,quant_var,nrounds=1000,
             #max_depth=3, alpha=5,
             #lambda=1.0, gamma=0.5)
#xgboost parameters (n_estimators/max_depth/regalpha/reglambda/gamma)
quant_alpha<-0.95
quant_delta <- 1.0
quant_thres <- 6.0
quant_var <- 3.2
dtrain_gas <- xgb.DMatrix(data = data.gas_xgboost, label=gas)
quantile_loss <- function(y_pred,dtrain){#,alpha,delta,threshold,var)
  y_true <- as.numeric(getinfo(dtrain, "label"))
  x <- y_true-y_pred
  grad <- (x<(alpha-1.0)*delta)*(1.0-alpha)-((x>=(alpha-1.0)*delta)&(x<alpha*delta) )*x/delta-alpha*(x>alpha*delta)
  hess <- ((x>=(alpha-1.0)*delta)&(x>alpha*delta) )/delta
  len <- length(y_true)#à vérifier 
  var <- (2*sample(0:1,len)-1.0)*var
  grad <- (abs(x)<threshold)*grad-(abs(x)>=threshold)*var
  hess <- (abs(x)<threshold)*hess+(abs(x)>=threshold)
  return(grad,less)
}
obj <- function(yhat, dtrain){
  y <- as.numeric(getinfo(dtrain, "label"))
  yhat <- as.numeric(yhat)
  grad <- ( yhat - y )
  hess <- rep(1.0, length(y))
  return(list(grad=grad, hess=hess))
}
GasModel1 <- xgboost(data = dtrain_gas,booster = "gbtree",
                     nrounds=1000,alpha=5,
                     lambda=1.0,gamma=0.5,
                     label = gas,max_depth = 5, 
                     eta = 0.1, nthread = 3, 
                      objective = quantile_loss)



gas_test <- data.matrix(data.test$data[c(45)])
dtrain_gas <- xgb.DMatrix(data = data.gas_xgboost, label=gas)
dtest_gas <- xgb.DMatrix(data = toPredict, label=gas_test)


watchlist <- list(train=dtrain_gas, test=dtest_gas)
param <- list(max_depth=7, 
              eta=0.01, nthread = 8, nrounds=2000)
GasModel2 <- xgb.train(data=dtrain_gas,  
                       watchlist=watchlist,max_depth=7, 
                       eta=0.01, nthread = 8, nrounds=2000)

toPredict <- data.matrix(data.test$data[,-c(45,46)])
predGas <- predict(GasModel2, toPredict)
plot(predGas,gas_test)
abline(a=0,b=1)
legend(-1, 4,legend=paste(names(param),param,sep="="),cex = 0.6)

err <- sum(abs(predGas-gas_test))
title("xgb.train",paste("err",err,sep="="))


GasModel3 <- xgb.train(data=dtrain_gas,booster = "gbtree", max_depth=7, 
                       eta=0.1, nthread = 8, nrounds=2000, 
                       watchlist=watchlist)
param<-list(booster = "gbtree", max_depth=7, 
            eta=0.1, nthread = 8, nrounds=2000)
predGas <- predict(GasModel3, toPredict)
plot(predGas,gas_test)
abline(a=0,b=1)
legend(-1, 4,legend=paste(names(param),param,sep="="),cex = 0.6)

err <- sum(abs(predGas-gas_test))
title("xgb.train",paste("err",err,sep="="))

importance_matrix <- xgb.importance(model = GasModel3)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
#to plot the tree
library(DiagrammeR)
xgb.plot.tree(model = GasModel3)
#cross validation
param <- list(max_depth=7, eta=0.1, nthread = 7, nrounds=2000, 
              watchlist=watchlist, silent=1)
nround <- 2000
cv_gas <- xgb.cv(param, dtrain_gas, nround, nfold=5,
                 metrics='rmse', showsd = FALSE)
res <- xgb.cv(params = param, data = dtrain_gas, nrounds = nround, 
              nfold = 10, prediction = TRUE)

toPredict <- data.matrix(data.test$data[,-c(45,46)])
predGas <- predict(GasModel1, toPredict)
gas_test <- data.matrix(data.test$data[c(45)])
monModel_gas<-function(bootIndex){
  bootIndex<-360
  data.gas_xgboost <- data.matrix(data.gas[bootIndex,])
  toPredict <- data.matrix(data.test$data[,-c(45,46)])
  dtrain_gas <- xgb.DMatrix(data = data.gas_xgboost, label=data.gas_xgboost[,45])
  dtest_gas <- xgb.DMatrix(data = toPredict, label=data.matrix(data.test$data[,c(45)]))
  
  
  watchlist <- list(train=dtrain_gas, test=dtest_gas)
  GasModel3 <- xgb.train(data=dtrain_gas,method = "xgbTree", max_depth=7, 
                         eta=0.7, nthread = 2, nrounds=200, 
                         watchlist=watchlist,alpha=0.15)
  #model<- nnet(gas~.,data=data.gas[bootIndex,],size=10,decay=1, linout=TRUE,maxit=500)
  #model sur les données bootstrapées
  #model<-lm(out~x, data.frame(x = inp[bootIndex], out = out[bootIndex]))
  data <- data.matrix(data.gas_xgboost[-c(45)])
  residuals2 <- predict(GasModel3,data)
  #prediction sur l'ensemble des données
  predi <- predict(GasModel3, toPredict)

  #predi<-predict(model, newdata = data.frame(x = inp))$fit
  
  #on retourne les deux
  return(list(residus = residuals, pred=predi))
}
monModel_oil <- function(bootIndex){
  model<- nnet(oil~.,data=data.oil[bootIndex,],size=10,decay=1, linout=TRUE,maxit=500)
  #model sur les données bootstrapées
  #model<-lm(out~x, data.frame(x = inp[bootIndex], out = out[bootIndex]))
  
  #prediction sur l'ensemble des données
  predi <- predict(model_neur1, data.test[,-c(45,46)])
  #predi<-predict(model, newdata = data.frame(x = inp))$fit
  
  #on retourne les deux
  return(list(residus = model$residuals, pred=predi))
}

int_pred_oil <- predInterval(monModel_oil,repLength=100)
int_pred_gas <- predInterval(monModel_gas,repLength=100)

##construire les intervalles de prédiction



#####################################################
##okkk
RMPSE<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab<- as.numeric(labels)
  epreds<- as.numeric(preds)
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMPSE", value = err))
}

obj <- function(yhat, dtrain){
  y <- as.numeric(getinfo(dtrain, "label"))
  yhat <- as.numeric(yhat)
  grad <- ( yhat - y )
  hess <- rep(1.0, length(y))
  return(list(grad=grad, hess=hess))
}

data.gas_xgboost <- data.matrix(sampleGas[-c(45)])
gas <- data.matrix(sampleGas[c(45)])
dtrain_gas <- xgb.DMatrix(data = data.gas_xgboost, label=gas)
toPredict <- data.matrix(data.test[,-c(45,46)])
gas_test <- data.matrix(data.test[c(45)])
#gas <- data.matrix(data.gas[c(45)])
dtest_gas <- xgb.DMatrix(data = toPredict, label=gas_test)
plot(predGas,gas_test)
abline(a=0,b=1)
################################
watchlist <- list(train=dtrain_gas, test=dtest_gas)

param <- list(  objective           = obj,#"reg:linear", 
                booster             = "gbtree",
                eta                 = 0.02,
                max_depth           = 10,
                subsample           = 0.9,
                colsample_bytree    = 0.7,
                #num_parallel_tree   = 2
                alpha = 0.0001,
                 lambda = 1
)
clf <- xgb.train(   params              = param, 
                    data                = dtrain_gas, 
                    nrounds             = 100,
                    verbose             = 0,
                    early_stop_round    = 100,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    feval=RMPSE
)
############################################
predGas <- predict(clf, toPredict)
monModel_gas<-function(bootIndex){
  #bootIndex<-360
  data.gas_xgboost <- data.matrix(sampleGas[bootIndex,-45])
  gas <- data.matrix(sampleGas[bootIndex,45])
  dtrain_gas <- xgb.DMatrix(data = data.gas_xgboost, label=gas)
  
  toPredict <- data.matrix(data.test[,-c(45,46)])
  gas_test <- data.matrix(data.test[c(45)])
  dtest_gas <- xgb.DMatrix(data = toPredict, label=gas_test)
  watchlist <- list(train=dtrain_gas, test=dtest_gas)
  clf <- xgb.train(   params              = param, 
                      data                = dtrain_gas, 
                      nrounds             = 100,
                      verbose             = 0,
                      early_stop_round    = 100,
                      watchlist           = watchlist,
                      maximize            = FALSE,
                      feval=RMPSE
  )
  predi <- predict(clf, toPredict)
  residuals <- gas - predict(clf, dtrain_gas)
  #on retourne les deux
  return(list(residus = residuals, pred=predi))
}
monModel_oil <- function(bootIndex){
  #bootIndex<-360
  data.oil_xgboost <- data.matrix(sampleOil[bootIndex,-45])
  oil <- data.matrix(sampleOil[bootIndex,45])
  dtrain_oil <- xgb.DMatrix(data = data.oil_xgboost, label=oil)
  
  toPredict <- data.matrix(data.test[,-c(45,46)])
  oil_test <- data.matrix(data.test[c(46)])
  dtest_oil <- xgb.DMatrix(data = toPredict, label=oil_test)
  watchlist <- list(train=dtrain_oil, test=dtest_oil)
  clf <- xgb.train(   params              = param, 
                      data                = dtrain_oil, 
                      nrounds             = 100,
                      verbose             = 0,
                      early_stop_round    = 100,
                      watchlist           = watchlist,
                      maximize            = FALSE,
                      feval=RMPSE
  )
  predi <- predict(clf, toPredict)
  residuals <- oil - predict(clf, dtrain_oil)
  #on retourne les deux
  return(list(residus = residuals, pred=predi))
}

int_pred_gas <- predInterval(monModel_gas,repLength=100,n_data=360)
int_pred_oil <- predInterval(monModel_oil,repLength=100,n_data=360)
prediction <- formatPrediction(int_pred_oil,
                               int_pred_gas,API.test)
score <- Metric(prediction, solution)

~#plot(predGas,gas_test)
#abline(a=0,b=1)

# Set necessary parameters and use parallel threads
#param <- list("objective" = "reg:linear", "nthread" = 8, "verbose"=0)

# Fit the model
#xgb.fit = xgboost(param=param, data = dtrain_gas, label = gas, nrounds=1500, eta = .01, max_depth = 7, 
 #                 min_child_weight = 5, scale_pos_weight = 1.0, subsample=0.8) 

