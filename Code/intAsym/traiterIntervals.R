#exemple
if(FALSE){
  #Récupération des données
  source("./pretraitement.R")
  source("./finalisation.R")
  library(matlib)
  #importer les données
  train <- read.csv2("./TrainSample.csv") 
  #validation <- read.csv2("./TestSample.csv")
  
  #Cross des données en 360 / 100 enregistrements
  ind.test <- sample(1:460, size=100, replace = FALSE)
  data.test <- train[ind.test,]
  data.train <- train[-ind.test,]
  
  #traitement des données
  data.train_results<-data.train[c(46,47)]
  data.gas<-data.train[-c(47)]
  data.oil<-data.train[-c(46)]
  data.gas<-RemplirValeursVides(data.gas)
  data.oil<-RemplirValeursVides(data.oil)
  API.data<-data.train[1]
  API.vali<-data.test[1]
  data.gas<-TraiterLesDates(data.gas)#enlève les dates et l'identifiant
  data.oil<-TraiterLesDates(data.oil)
  data.train<-TraiterLesDates(RemplirValeursVides(data.train))
  
  data.test<- RemplirValeursVides(data.test)
  data.test<- TraiterLesDates(data.test)
  
  
  #Choix des variables de linéarisation
  varNamesVec <-c("Depth_TVD_PPLS..ft.")
  varNamesVec <-names(data.oil[-43])
  varNamesString <- paste(varNamesVec, collapse = " + ")
  
  formulaOil <- as.formula(paste('data.oil$OilCum360 ~ ',varNamesString))
  formulaGas <- as.formula(paste('data.gas$GasCum360 ~ ',varNamesString))
  
  newData<<-data.test[varNamesVec]
  model.oil<-lm(formulaOil, data.oil)
  model.gas<-lm(formulaGas, data.gas)
  
  pred.oil<-predict.lm(model.oil, newData, interval="prediction")
  pred.gas<-predict.lm(model.gas, newData, interval="prediction")
  
  solution.oil<-data.test[43]
  solution.gas<-data.test[44]
} 

#décale linéairement les intervales de prediction afin de trouver le meilleur score

BestAsymInter<-function(intSym.oil, intSym.gas, solution.oil, solution.gas){
    #intSym.oil<-pred.oil; intSym.gas<-pred.gas;
  source("./cmaes.R")
  
  #traitement des variables d'entrées
  oil<-NULL
  gas<-NULL
  oil$upr<-intSym.oil[,3]
  oil$lwr<-intSym.oil[,2]
  oil$fit<-(oil$upr+oil$lwr)/2
  gas$upr<-intSym.gas[,3]
  gas$lwr<-intSym.gas[,2]
  gas$fit<-(oil$upr+oil$lwr)/2

  
  #fonction Metric
  Metric <- function(prediction) {
    marge <- (prediction$OIL360_SUP - prediction$OIL360_INF) * (prediction$GAS360_SUP - prediction$GAS360_INF)
    ecart <- ifelse(
      (solution.oil >= prediction$OIL360_INF & solution.oil <= prediction$OIL360_SUP)
      & (solution.gas >= prediction$GAS360_INF & solution.gas <= prediction$GAS360_SUP)
      , marge, 10)
    metric = mean(ecart)
    
    return(metric)
  }
  
  #fonction cout
  scoreIntAsym <- function(r){
    r.oil<-r[1]
    r.gas<-r[2]
    if(r.gas>1 | r.gas<0 | r.oil>1 | r.oil<0 ){
      cat(r)
      stop("R en dehors de [0,1]")
    }
    
    newPred.oil.upr<-oil$fit + (oil$upr - oil$lwr)*r.oil
    newPred.oil.lwr<-oil$fit - (oil$upr - oil$lwr)*(1-r.oil)
    
    newPred.gas.upr<-gas$fit + (gas$upr - gas$lwr)*r.gas
    newPred.gas.lwr<-gas$fit - (gas$upr - gas$lwr)*(1-r.gas)
    
    prediction<-data.frame(
      GAS360_INF = newPred.gas.lwr,
      GAS360_SUP = newPred.gas.upr,
      OIL360_INF = newPred.oil.lwr,
      OIL360_SUP = newPred.oil.upr)
    
    score <- Metric(prediction)
    return(score)
  }

  #initialisation du cmaes
  source("./of_wrapper.R")
  glob_noisy <<- FALSE
  param <<- list(LB=0,UB = 1,budget=500, dim=2, xinit=c(0.5,0.5),sigma=0.1)
  fun<<-scoreIntAsym
  opt<-cmaes
  
  res<-opt(ofwrapper, param)
  plot(res$fhist,ylab = "solution", type = "l", main="Fonction cout")
  plot(res$xhist, col=rainbow(length(res$fhist)), main="évolution de la solution")
  cat("---Décalage des intervals\n finit = ", res$fhist[0])
  cat("fbest = ",res$f_best, "\nxbest = ",res$x_best)

  newPred.oil.upr<-oil$fit + (oil$upr - oil$lwr)*res$xhist[1]
  newPred.oil.lwr<-oil$fit - (oil$upr - oil$lwr)*(1-res$xhist[1])
  
  newPred.gas.upr<-gas$fit + (gas$upr - gas$lwr)*res$xhist[2]
  newPred.gas.lwr<-gas$fit - (gas$upr - gas$lwr)*(1-res$xhist[2])
  
  
  return(list(intAsymOil = cbind(newPred.oil.upr, newPred.oil.lwr),
              intAsymGas = cbind(newPred.gas.upr, newPred.gas.lwr)))
  }

#BestAsymInter(pred.oil, pred.gas, solution.oil, solution.gas)
