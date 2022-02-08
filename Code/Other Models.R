oil<-dataTrain$OilCum360
gas<-dataTrain$GasCum360

x<-dataTrain
x$OilCum360<-NULL
x$GasCum360<-NULL

xNew<-dataTest

source("./predInterval_bootstrap.R")
source("./finalisation.R")

#regression L1 
##############################################
library(Blossom)
regL1.oil<-lad(OilCum360~., dataTrain)
regL1.gas<-lad(GasCum360~., dataTrain)

pred<-NULL
pred$OilCum360<-as.matrix(xNew) %*% regL1.oil@Betas[-1] + regL1.oil@Betas[1]
pred$GasCum360<-as.matrix(xNew) %*% regL1.gas@Betas[-1] + regL1.gas@Betas[1]
plot(pred$OilCum360, dataTestResults$OilCum360)
abline(a = 0,b=1)

regL1.oil.intervPed<-function(bootIndex){
  #model sur les données bootstrapées
  model<-l1fit(x = x[bootIndex,], oil[bootIndex])#lm(out~x, data.frame(x = inp[bootIndex], out = out[bootIndex]))
  
  #prediction sur l'ensemble des données
  predi<-as.matrix(xNew) %*% model$coefficients[-1] + model$coefficients[1]
  
  #on retourne les deux
  return(list(residus = model$residuals, pred=predi))
}
regL1.gas.intervPed<-function(bootIndex){
  #model sur les données bootstrapées
  model<-l1fit(x = x[bootIndex,], gas[bootIndex])#lm(out~x, data.frame(x = inp[bootIndex], out = out[bootIndex]))
  
  #prediction sur l'ensemble des données
  predi<-as.matrix(xNew) %*% model$coefficients[-1] + model$coefficients[1]
  
  #on retourne les deux
  return(list(residus = model$residuals, pred=predi))
}

predOil<-predInterval(FUN = regL1.oil.intervPed, length(oil))
predGas<-predInterval(FUN = regL1.gas.intervPed, length(gas))
prediction<-formatPrediction(predOil, predGas, listAPI=1:length(oil))

Metric(prediction, dataTestResults)
#######################################################

#Ridge
######################################################
library(glmnet)
set.seed(999)
dataOil<-cbind(x,y=oil)
ridge <- glmnet(as.matrix(x),oil, alpha=0)
lasso <- cv.glmnet(as.matrix(x),oil, alpha=1)
plot(lasso)

pred<-predict(lasso, as.matrix(xNew), s=c("lambda.1se"))
plot(pred, dataTestResults$OilCum360)
abline(a=0,b=1)
title("prediction avec Lasso - estimé contre réel")
residus<-(pred-dataTestResults$OilCum360)^2
plot(residus)

pred<-predict(ridge, as.matrix(xNew), s=c("lambda.1se"))
plot(pred, dataTestResults$OilCum360)
abline(a=0,b=1)
title("prediction avec ridge - estimé contre réel")
residus<-(pred-dataTestResults$OilCum360)^2
plot(residus)

lasso.oil.intPred<-function(bootIndex){
  #model sur les données bootstrapées
  model<-cv.glmnet(as.matrix(x),oil, alpha=1)
  
  #prediction sur l'ensemble des données
  predi<-predict(lasso, as.matrix(xNew), s=c("lambda.1se","lambda.min"))
  resi<-oil - predict(lasso, as.matrix(x),  s=c("lambda.1se","lambda.min"))
  
  #on retourne les deux
  return(list(residus = resi, pred=predi))
}
lasso.gas.intPred<-function(bootIndex){
  #model sur les données bootstrapées
  model<-cv.glmnet(as.matrix(x),gas, alpha=1)
  
  #prediction sur l'ensemble des données
  predi<-predict(lasso, as.matrix(xNew), s=c("lambda.1se","lambda.min"))
  resi<-gas - predict(lasso, as.matrix(x),  s=c("lambda.1se","lambda.min"))
  
  #on retourne les deux
  return(list(residus = resi, pred=predi))
}

predOil<-predInterval(FUN = lasso.oil.intPred, length(oil), nboot = 100)
predGas<-predInterval(FUN = lasso.gas.intPred, length(gas), nboot = 100)
prediction<-formatPrediction(predOil, predGas, listAPI=1:length(oil))

Metric(prediction, dataTestResults)
#########################################################

######regresssion L1 et L2
#######################################################
library(LiblineaR)
plotPred<-function(model, title){
  pred<-predict(model, newx = xNew)$predictions
  residus<-(pred-dataTestResults$OilCum360)^2
  #plot(residus)
  plot(pred, dataTestResults$OilCum360)
  title(title)
  abline(0,1)
}
modele<-LiblineaR(data=x, target=oil, type=11)# L2-regularized L2-loss support vector regression (primal)
plotPred(modele, "Regression linéaire L2 régularisée L2")
modele<-LiblineaR(data=x, target=oil, type=12)# L2-regularized L2-loss support vector regression (dual)
#plotPred(modele, "Regression linéaire L2 régularisée L2")
modele<-LiblineaR(data=x, target=oil, type=13)# L2-regularized L1-loss support vector regression (dual)
plotPred(modele, "Regression linéaire L1 régularisée L2")
######################################################

########xgboost
##########################################################
library(xgboost)

plotxgb<-function(xgb, title){
  pred<-predict(xgb, as.matrix(xNew))
  plot(pred, dataTestResults$OilCum360)
  title(title)
  abline(0,1)
}
#normal
xgb<-xgboost(data = as.matrix(x),
             label = oil, 
             nrounds = 100)
plotxgb(xgb, "XGBoost - arbres")

#linear based
xgb<-xgboost(data = as.matrix(x),
             label = oil, 
             nrounds = 100,
             booster = 'gblinear')
plotxgb(xgb, "XGBoost - linéaire")

#eta petit (robust ?)
xgb<-xgboost(data = as.matrix(x),
             label = oil, 
             nrounds = 100,
             eta = 0.1)
plotxgb(xgb)

#eta grand (overfit ?)
xgb<-xgboost(data = as.matrix(x),
             label = oil, 
             nrounds = 100,
             eta = 0.1)
plotxgb(xgb)
