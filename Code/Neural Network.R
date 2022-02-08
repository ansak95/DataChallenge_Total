####TP challenge
source("./pretraitement.R")
source("./finalisation.R")
require(graphics)
########################################################
#importer les données
train <- read.csv2("./data/TrainSample.csv") 
validation <- read.csv2("./data/TestSample.csv")

#Cross des données en 360 / 100 enregistrements
ind.test <- sample(1:460, size=100, replace = FALSE)
data.test <- train[ind.test,]
data.train <- train[-ind.test,]

#on travaillera par la suite avec data.train 

#traitement des données
data.train_results<-data.train[c(46,47)]
data.gas<-data.train[-c(47)]
data.oil<-data.train[-c(46)]
data.gas<-RemplirValeursVides(data.gas)
data.oil<-RemplirValeursVides(data.oil)
API.data<-data.train[1]
API.vali<-data.test[1]
data.gas<-preTraitement(data.gas)$data#enlève les dates et l'identifiant
data.oil<-preTraitement(data.oil)$data
data.train<-preTraitement(data.train)$data

data.test<- preTraitement(data.test)
data.test <- data.test$data
########################################################
#Réseau de neuronnes
set.seed(300)
library(neuralnet)
library(nnet)
library(e1071)

names(data.gas)[45] <- c("gas")
names(data.oil)[45] <- c("oil")
n <- names(data.gas)
f <- as.formula(paste("gas ~", paste(n[!n %in% "gas"], collapse = " + ")))
model_neur <- neuralnet(f ,data=data.gas, hidden=10,linear.output = TRUE,rep=3, threshold=0.001)
#print(model_neur)
plot(model_neur)
pre_neur <- compute(model_neur,data.test[,-c(45,46)]) #, type="raw")
pre_n<- sapply(pre_neur$net.result, FUN ="array")
d <- data.frame(data.test[,c(45)],pre_neur$net.result)
plot(d)

model_neur1 <- nnet(gas~.,data=data.gas,size=10,decay=1, linout=TRUE,maxit=500)
summary(model_neur1)
predic <- predict(model_neur1, data.test[,-c(45,46)])
plot(predic,data.test[,c(45)])
abline(a=0,b=1)


#Bootstrap general, une variable
bootstrap<-function(data){
  taille<-length(data[,1])
  choix <- sample(x = (1:taille), replace=TRUE)
  return(data[choix,])
}
erreurStandart <- function(data, FUN = mean, nbBoot=20, evolution = FALSE, data2 = NULL){
  val<-matrix(NA,nrow = nbBoot)
  if(evolution){
    evolSd<-matrix(NA,nrow = nbBoot)
    evolMean<-matrix(NA,nrow = nbBoot)
    evolMean.Q1<-matrix(NA,nrow = nbBoot)
    evolMean.Q2<-matrix(NA,nrow = nbBoot)
  }
  
  for(i in 1:nbBoot){
    
    if(!is.null(data2)){
      size <-min(length(data[1,]), length(data2[1,]))
      ech <- bootstrap(data)
      ech2<- bootstrap(data2)
      val[i] <- FUN(ech, ech2)
    }else{
      ech <- bootstrap(data)
      val[i] <- FUN(ech)
    }
    if(evolution){
      evolSd[i]<-sd(val[1:i])
      evolMean[i]<-mean(val[1:i])
      evolMean.Q1[i]<-quantile(evolMean[1:i],.025)
      evolMean.Q2[i]<-quantile(evolMean[1:i],.975)
    }
  }
  
  if(evolution){
    return(list(
      fun = deparse(substitute(FUN)),
      sd = sd(val), mean = mean(val),
      evolSd = evolSd,
      evolMean=evolMean,
      evolMean.Q1=evolMean.Q1,
      evolMean.Q2=evolMean.Q2
    ))
  }
  
  return(list(fun = deparse(substitute(FUN)),sd = sd(val), mean = mean(val)))
}
plotEvolES <- function(es){
  if(is.null(es$evolMean)){
    cat("fonction doit être evolutive")
  }
  par(mfrow=c(2,1)) 
  plot(es$evolMean, type = 'l')
  abline(a = es$mean,b=0)
  lines(es$evolMean.Q1, col='grey', lty=2)
  lines(es$evolMean.Q2, col='grey', lty=2)
  title(paste(es$fun, "- evol de la moyenne par bootstrap"))
  plot(es$evolSd, type = 'l')
  abline(a = es$sd,b=0)
  title(paste(es$fun, "- evol de la variance par bootstrap"))
}

fun <- function(data){
  res <- predict(model_neur1, data, rep = 5)
  return(res)
}

erreurStandart(data.test[,-c(45,46)], fun)

monModel_gas<-function(bootIndex){
  model<- nnet(gas~.,data=data.gas[bootIndex,],size=10,decay=1, linout=TRUE,maxit=500)
  #model sur les données bootstrapées
  #model<-lm(out~x, data.frame(x = inp[bootIndex], out = out[bootIndex]))

  #prediction sur l'ensemble des données
  predi <- predict(model_neur1, data.test[,-c(45,46)])
    #predi<-predict(model, newdata = data.frame(x = inp))$fit
  
  #on retourne les deux
  return(list(residus = model$residuals, pred=predi))
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
#faire des bootstrap pour avoir les intervalle (fct de françois)

prediction <- data.frame(cbind(int_pred_gas[,c(2,3)],int_pred_oil[,c(2,3)]))
colnames(prediction) <- c("GAS360_INF","GAS360_SUP","OIL360_INF","OIL360_SUP")
solution <-data.test[c(45,46)]
score <- Metric(prediction, solution)



################################################################
#xgboost
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


library(grnn)
grnn_gas <- learn(data.gas,variable.column=45)
grnn_oil <- learn(data.oil,variable.column=45)
a <- as.matrix(data.test[,-c(45,46)])
pred_grnn_gas <- guess(grnn_gas,a)

##############################################################
plot(tune.nnet(gas ~ .,data=data.gas,size=c(5,10,15), decay=1:5,linout=TRUE))
plot(tune.nnet(gas ~ .,data=data.gas,size=4:5,decay=1:10))
CVnn(gas ~.,data=data.gas,size=7, decay=0)
library(quantmod)
library(nnet)
library(caret)
model_nnet <- train(gas ~ ., data.gas[,-c(42)], method='nnet', trace = FALSE)
prediction_nnet <- predict(model_nnet, data.gas[,-c(42)]) 
