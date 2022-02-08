##CROSS VALIDATION
##Regression lineaire
###extimer les theta_hat(coef) , l'erreur (ehat)
#prendre les quantiles de ehat et construire l'interval de pred à partir de ceci
test <- read.csv2("./data/TestSample.csv")
library(caret)
library(ranger)
library(e1071)
library(data.table) #Faster reading
library(xgboost)
#train<-data.train
#dmy <- dummyVars(" ~ .", data = train,fullRank = T)
#train_transformed <- data.frame(predict(dmy, newdata = train))
#str(train_transformed)


##xgboost
set.seed(2016)
names(data.train)[46]<- names(data.test$data)[46]
train <- rbind(data.train,data.test$data)

data_xgboost <- data.matrix(train[-c(45,46)])
gas <- data.matrix(train[c(45)])
oil <- data.matrix(train[c(46)])
#gas_test <- data.matrix(data.test$data[c(45)])
toPredict <- data.matrix(test)

xgtrain <- xgb.DMatrix(data = data_xgboost, label=gas)
#xgtrain <- xgb.DMatrix(data = data_xgboost, label=oil)
xgtest <- xgb.DMatrix(data = toPredict)

docv <- function(param0, iter) {
  model_cv = xgb.cv(
    params = param0
    , nrounds = iter <- 500
    , nfold = 100
    , data = xgtrain
    , early_stopping_rounds = 10
    , maximize = FALSE
    , nthread = 8
  )
  gc()
  best <- min(model_cv$evaluation_log)
  bestIter <- model_cv$best_iteration
  
  cat("\n",best, bestIter,"\n")
  print(model_cv[bestIter])
  
  bestIter-1
}
doTest <- function(param0, iter) {
  watchlist <- list('train' = xgtrain)
  model = xgb.train(
    nrounds = iter
    , params = param0
    , data = xgtrain
    , watchlist = watchlist
    , print_every_n = 20
    , nthread = 8
  )
  p <- predict(model, xgtest)
  rm(model)
  gc()
  p
}


param0 <- list(
  # some generic, non specific params
  "objective"  = "reg:linear"
  , "eval_metric" = "rmse"
  , "eta" = 0.07
  , "subsample" = 0.9
  , "colsample_bytree" = 0.9
  , "min_child_weight" = 1
  , "max_depth" = 10
)
ensemble <- rep(0, nrow(test))
set.seed(2018)
cv <- docv(param0, 500) 
cv <- round(cv * 1.5)

for (i in 1:100) {
  print(i)
  set.seed(i + 2017)
  p <- doTest(param0, cv) 
  # use 40% to 50% more than the best iter rounds from your cross-fold number.
  # as you have another 50% training data now, which gives longer optimal training time
  ensemble <- ensemble + p
}
cat("Making predictions\n")
submi_gas <- ensemble/i
#submi_oil<- ensemble/i





myControl <- trainControl(
  method = "cv", number = 5,repeats = 5,
  verboseIter = TRUE
)




require(caret)
CrossValidation <- function(formula,train, type="CV",nfold=100,model="linear",alpha=0.95){
  train<-data.train
  flds <- createFolds(train, k =nfold, list = TRUE, returnTrain = FALSE)
  for(i=1:nfolds){
    data.test <- flds[i]
    data.train<- rbind(flds[-i])
  }
  
}



#naif meilleur
GasModel_init <- lm(gas ~. , data=sampleGas[,-ncol(data.gas)])
OilModel_init <- lm(oil ~. , data=sampleOil[,-ncol(data.oil)])
#meilleur modèle
GasModel <- step(GasModel_init,direction = c("both"), scope = list(lower = gas ~ 1, upper = gas~.))
OilModel <- step(OilModel_init,direction = c("both"), scope = list(lower = oil ~ 1, upper = oil~.))

#trans meilleur
fun <- as.function(paste("transform_",trans))
fun_inv <- as.function(paste("inv_trans_",trans))
sample <- fun(data$train)
test <- fun(data$test)
solution <- test[,c(ncol(test)-1,ncol(test))] 
API.test <- data$APITEST
API.train <- data$APITRAIN
sampleOil<-sampleGas<-sample
gas <- sampleGas$GasCum360
oil <- sampleOil$OilCum360
sampleOil$GasCum360<-NULL
sampleGas$OilCum360<-NULL
#model naif
GasModel_init <- lm(gas ~. , data=sampleGas[,-ncol(data.gas)])
OilModel_init <- lm(oil ~. , data=sampleOil[,-ncol(data.oil)])
#meilleur modèle
GasModel <- step(GasModel_init,direction = c("both"), scope = list(lower = gas ~ 1, upper = gas~.))
OilModel <- step(OilModel_init,direction = c("both"), scope = list(lower = oil ~ 1, upper = oil~.))
