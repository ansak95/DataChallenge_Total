##############################################
#Fonction renvoyant un vecteur 2*hauteur(toPredict) Ã©tant la valeur estimÃ©e de nos deux variables OilCum360 et GasCum360
#Les donnÃ©es sont traitÃ©es ici

##vu que cette fonction est le rassemblement de plusieurs bout de code séparé il s e peut qu'elle ne soit pas adapté avecles données ( notations). 
#le script pour chaque modèle est par contre bon

#sample = echantillon de donnÃ©Ã© avec les rÃ©sultats de OilCum360 et OilGas360


#data contient data.train = data$train , data.test =data$test , API.test=data$APItest et API.train=data$APItrain 
#data.test est la partie de train utilisée pour valider les modèles
#data contient les données déjà prétraiter
#toPredict contient la data test sans résultats

#trans = "log"  ou  = "inv"
#predType <- "nouvel espace" "lineaire"
#predType2 <- "acp" "pls"
#predType1 <- "trans_meilleure" "naif" "naif_meilleur" "trans" "nouvel_espace" "nouvel_espace_trans"
#######################################################
source("./pretraitement3.R")
source("./finalisation.R")
source("./predInterval_bootstrap.R")
library(xgboost)
library(nnet)
library(caret)
library("COBRA")
library(randomForest)
#sample<-dataTrain; toPredict<-dataTest
ElPredictador <- function(data, toPredict,  variableAPI,alpha=0.05,
                          predType="lineaire",predType1="trans_meilleure",
                          trans="log",predType2="acp"){ 
  
  nomFichier<<-paste(nomFichier,predType,Sys.Date( ),sep = "_")
  #VÃ©rifie que les entrÃ©es sont pas n'importe quoi 
  #la largeur de sample doit valoir la largeur de toPredict+2 par exemple	TODO
  if(FALSE){
    stop('donnÃ©es rentrÃ©es incorrectes')
  }
  ###############################################################
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
##############################################################
  #prediction linéaire sur toutes les variables
  if(predType == "lineaire"){
    ####on commence par un modèle naif tous les données sans transformations
    if (predType1=="naif"){
      #model naif
      GasModel <- lm(gas ~. , data=sampleGas[,-ncol(sampleGas)])
      OilModel <- lm(oil ~. , data=sampleOil[,-ncol(sampleOil)])
      #pred données d'apprentissage
      predOil <- predict.lm(OilModel, newdata=sampleOil, interval ='pre',level=1-alpha)
      predGas <- predict.lm(GasModel, newdata=sampleGas, interval ='pre',level=1-alpha)
      #predOil_conf <- predict.lm(OilModel, newdata=sampleOil, interval ='conf',level=1-alpha)
      #predGas_conf <- predict.lm(GasModel, newdata=sampleGas, interval ='conf',level=1-alpha)
      
      #pred données test
      predOil_sym <- predict.lm(OilModel, newdata=test, interval ='pre',level=1-alpha)
      predGas_sym <- predict.lm(GasModel, newdata=test, interval ='pre',level=1-alpha)
      #predOil_sym_c <- predict.lm(OilModel, newdata=test, interval ='conf',level=1-alpha)
      #predGas_sym_c <- predict.lm(GasModel, newdata=test, interval ='conf',level=1-alpha)
      
      #plot points et intervalles
      ##points train
      plot(predOil[,1]~sampleOil$OilCum360,pch="*",col='black',xlab="vraies valeurs",ylab="prédictions",lwd=10)
      abline(a=0,b=1,col="purple")
      #title("prédictions vs vrais valeurs")
      segments(x0=sample$OilCum360,y0 = predOil[,2],y1 = predOil[,3],col='grey70')
      points(predOil[,1]~sampleOil$OilCum360,pch="*",col='black')
      ##point test
      points(predOil_sym[,1]~test$OilCum360,pch="+",col='blue')
      segments(x0=test$OilCum360,y0 = predOil_sym[,2],y1 = predOil_sym[,3],col='red')
      title("prédictions en fonction des valeurs réelles 
            avec les intervalles de prédictions Oil")
      
      plot(predGas[,1]~sampleGas$GasCum360,pch="*",col='black',xlab="vraies valeurs",ylab="prédictions",lwd=10)
      abline(a=0,b=1,col="purple")
      #title("prédictions vs vrais valeurs")
      segments(x0=sample$GasCum360,y0 = predGas[,2],y1 = predGas[,3],col='grey70')
      points(predGas[,1]~sampleGas$GasCum360,pch="*",col='black')
      ##point test
      points(predGas_sym[,1]~test$GasCum360,pch="+",col='blue')
      segments(x0=test$GasCum360,y0 = predGas_sym[,2],y1 = predGas_sym[,3],col='red')
      title("prédictions en fonction des valeurs réelles 
            avec les intervalles de prédictions Gas")
      
      
      #calcul score
      prediction <- formatPrediction(predOil_sym,predGas_sym,API.test)
      score <- Metric(prediction, solution)
      #analyse des résidus rls 
      #gas
      ## rls
      plot(rstudent(GasModel) ~ fitted(GasModel),xlab="Réponse estimée",ylab="Résidus",ylim=c(-3,3))
      abline(h=2,col="red")
      abline(h=-2,col="red")
      abline(h=0,lty=2)
      title('Résidus studentisés contre la réponse prédite Gas')
      
      # autre outils de validation : droite de Henry pour la normalité des résidus
      plot(GasModel,which=2)
      abline(0,1,col="red",lwd=2)
      title("droite de Henry dans le cas de modèle naif tous prédicteurs, cas Gas")
      #oil
      ## rls
      plot(rstudent(OilModel) ~ fitted(OilModel),xlab="Réponse estimée",ylab="Résidus",ylim=c(-3,3))
      abline(h=2,col="red")
      abline(h=-2,col="red")
      abline(h=0,lty=2)
      title('Résidus studentisés contre la réponse prédite Oil ')
      
      # autre outils de validation : droite de Henry pour la normalité des résidus
      plot(OilModel,which=2)
      abline(0,1,col="red",lwd=2)
      title("droite de Henry dans le cas de modèle naif tous prédicteurs, cas Oil")
      
      
    }
    else if (predType1=="naif_meilleur"){
      #model naif avec step
      GasModel_init <- lm(gas ~. , data=sampleGas[,-ncol(sampleGas)])
      OilModel_init <- lm(oil ~. , data=sampleOil[,-ncol(sampleOil)])
      #meilleur modèle
      GasModel <- step(GasModel_init,direction = c("both"), scope = list(lower = gas ~ 1, upper = gas~.))
      OilModel <- step(OilModel_init,direction = c("both"), scope = list(lower = oil ~ 1, upper = oil~.))
      #pred données d'apprentissage
      predOil <- predict.lm(OilModel, newdata=sampleOil, interval ='pre',level=1-alpha)
      predGas <- predict.lm(GasModel, newdata=sampleGas, interval ='pre',level=1-alpha)
      #pred données test
      predOil_sym <- predict.lm(OilModel, newdata=test, interval ='pre',level=1-alpha)
      predGas_sym <- predict.lm(GasModel, newdata=test, interval ='pre',level=1-alpha)
      #les predicteurs choisis
      pred_g<- names(GasModel$coefficients)
      pred_o<- names(OilModel$coefficients)
      #plot points et intervalles
      ##points train
      plot(predOil[,1]~sampleOil$OilCum360,pch="*",col='black',xlab="vraies valeurs",ylab="prédictions",lwd=10)
      abline(a=0,b=1,col="purple")
      #title("prédictions vs vrais valeurs")
      segments(x0=sample$OilCum360,y0 = predOil[,2],y1 = predOil[,3],col='grey70')
      points(predOil[,1]~sampleOil$OilCum360,pch="*",col='black')
      ##point test
      points(predOil_sym[,1]~test$OilCum360,pch="+",col='blue')
      segments(x0=test$OilCum360,y0 = predOil_sym[,2],y1 = predOil_sym[,3],col='red')
      title("prédictions en fonction des valeurs réelles 
            avec les intervalles de prédictions, modèle naif amélioré oil")
      plot(predGas[,1]~sampleGas$GasCum360,pch="*",col='black',xlab="vraies valeurs",ylab="prédictions",lwd=10)
      abline(a=0,b=1,col="purple")
      #title("prédictions vs vrais valeurs")
      segments(x0=sample$GasCum360,y0 = predGas[,2],y1 = predGas[,3],col='grey70')
      points(predGas[,1]~sampleGas$GasCum360,pch="*",col='black')
      ##point test
      points(predGas_sym[,1]~test$GasCum360,pch="+",col='blue')
      segments(x0=test$GasCum360,y0 = predGas_sym[,2],y1 = predGas_sym[,3],col='red')
      title("prédictions en fonction des valeurs réelles 
            avec les intervalles de prédictions, modèle naif amélioré GAS")
      #calcul score
      prediction <- formatPrediction(predOil_sym,predGas_sym,API.test)
      score <- Metric(prediction, solution)
      #analyse des résidus rls 
      #gas
      ## rls
      plot(rstudent(GasModel) ~ fitted(GasModel),xlab="Réponse estimée",ylab="Résidus",ylim=c(-3,3))
      abline(h=2,col="red")
      abline(h=-2,col="red")
      abline(h=0,lty=2)
      title('Résidus studentisés contre la réponse prédite Gas
            modèle réduit')
      
      # autre outils de validation : droite de Henry pour la normalité des résidus
      plot(GasModel,which=2)
      abline(0,1,col="red",lwd=2)
      title("droite de Henry dans le cas de modèle naif tous prédicteurs, cas Gas")
      #oil
      ## rls
      plot(rstudent(OilModel) ~ fitted(OilModel),xlab="Réponse estimée",ylab="Résidus",ylim=c(-3,3))
      abline(h=2,col="red")
      abline(h=-2,col="red")
      abline(h=0,lty=2)
      title('Résidus studentisés contre la réponse prédite Oil
            modèle réduit ')
      
      # autre outils de validation : droite de Henry pour la normalité des résidus
      plot(OilModel,which=2)
      abline(0,1,col="red",lwd=2)
      title("droite de Henry dans le cas de modèle naif tous prédicteurs, cas Oil")
      
      
      
    }
    else if (predType=="trans_meilleur"){
      ########################################
      #préparation des variables
      data.gas <- sampleGas
      data.oil <- sampleOil
      data.test <- test
      names(data.gas)[45] <- c("gas")
      names(data.oil)[45] <- c("oil")
      names(data.test)[c(45,46)]<-c("gas","oil")
      
      #gas <- data.gas$gas
      #oil <- data.oil$oil
      
      Gasinf <- min(gas)
      seuil.G = Gasinf - 0.005     # pour éviter ln(0) = +infini
      logGas <- log(gas - seuil.G)
      
      Oilinf <- min(oil)
      seuil.O = Oilinf - 0.005     # pour éviter ln(0) = +infini
      logOil <- log(oil - seuil.O)
      ###########################################
      #construction des modèles
      #gas
      reglogGas1 <- lm(logGas~., data=data.gas[,-c(45)])
      #modèle réduit
      Slogg<-step(reglogGas1, direction = c("both"), scope = list(lower = logGas ~ 1, upper = logGas~.))
      formulag<-Slogg$call
      pred_G<-names(Slogg$coefficients)
      ##plot des résidus
      plot(rstudent(Slogg) ~ fitted(Slogg),xlab="Réponse estimée",ylab="Résidus",ylim=c(-3,3))
      abline(h=2,col="red")
      abline(h=-2,col="red")
      abline(h=0,lty=2)
      title('Résidus studentisés contre la réponse prédite logGas 
            pour rls avec le meilleur modèle trouvé')
      #oil
      reglogOil1 <- lm(logOil ~., data=data.oil[,-c(45)])
      #modèle réduit
      Slogo<-step(reglogOil1, 
                  direction = c("both"), 
                  scope = list(lower = logOil ~ 1, upper = logOil~.))
      formulao<-Slogo$call
      pred_O<-names(Slogo$coefficients)
      #résidus 
      plot(rstudent(Slogo) ~ fitted(Slogo),xlab="Réponse estimée",ylab="Résidus",ylim=c(-3,3))
      abline(h=2,col="red")
      abline(h=-2,col="red")
      abline(h=0,lty=2)
      title('Résidus studentisés contre la réponse prédite logOil 
            pour rls avec le meilleur modèle trouvé')
      #summary(Slogg)
      #summary(sg)
      #summary(Slogo)
      #summary(so)
      ########################################################
      #intervalles de prédictions 
      predGas_sym_B <- predict(Slogg,new=data.test,interval="pred",level=1-alpha)
      # attention à bien utiliser la tranformation inverse
      predGas_sym <-exp(predGas_sym_B) + seuil.G
      
      predOil_sym_B <- predict(Slogo,new=data.test,interval="pred",level=1-alpha)
      predOil_sym<- exp(predOil_sym_B) + seuil.O
      
      predGas_B <- predict(Slogg,new=data.gas,interval="pred",level=1-alpha)
      # attention à bien utiliser la tranformation inverse
      predGas <-exp(predGas_B) + seuil.G

      predOil_B <- predict(Slogo,new=data.oil,interval="pred",level=1-alpha)
      predOil<- exp(predOil_B) + seuil.O
      ################################################
      #plot des résultats
      #oil
      ##points train +intervalles train
      plot(predOil[,1]~sampleOil$OilCum360,pch="*",col='black',xlab="vraies valeurs",ylab="prédictions",lwd=10)
      abline(a=0,b=1,col="purple")
      #title("prédictions vs vrais valeurs")
      segments(x0=sample$OilCum360,y0 = predOil[,2],y1 = predOil[,3],col='grey70')
      points(predOil[,1]~sampleOil$OilCum360,pch="*",col='black')
      
      ##point test+ intervalles test 
      points(predOil_sym[,1]~test$OilCum360,pch="+",col='blue')
      segments(x0=test$OilCum360,y0 = predOil_sym[,2],y1 = predOil_sym[,3],col='red')
      title("prédictions en fonction des valeurs réelles 
      avec les intervalles de prédictions, modèle avec transformation amélioré oil")
      #gas
      #point train +intervalles train
      plot(predGas[,1]~sampleGas$GasCum360,pch="*",col='black',xlab="vraies valeurs",ylab="prédictions",lwd=10)
      abline(a=0,b=1,col="purple")
      segments(x0=sample$GasCum360,y0 = predGas[,2],y1 = predGas[,3],col='grey70')
      points(predGas[,1]~sampleGas$GasCum360,pch="*",col='black')
      ##point test +intervalles test 
      points(predGas_sym[,1]~test$GasCum360,pch="+",col='blue')
      segments(x0=test$GasCum360,y0 = predGas_sym[,2],y1 = predGas_sym[,3],col='red')
      title("prédictions en fonction des valeurs réelles 
      avec les intervalles de prédictions, modèle avec transformation amélioré GAS")
      ####################################################
      #calcul score
      prediction <- formatPrediction(predOil_sym,predGas_sym,API.test)
      score <- Metric(prediction, solution)
      ###################################################
      #améliorer les intervalles
      #les intervalles qui empire le score
      aChangerG <- (predGas[,3]-predGas[,2])/sqrt(10)
      predGas[which(aChangerG>1),3] <- predGas[which(aChangerG>1),1]+sqrt(7)/2
      predGas[which(aChangerG>1),2] <- predGas[which(aChangerG>1),1]-sqrt(7)/2
      
      aChangerO <- (predOil[,3]-predOil[,2])/sqrt(10)
      predOil[which(aChangerO>1),3] <- predOil[which(aChangerO>1),1]+sqrt(7)/2
      predOil[which(aChangerO>1),2] <- predOil[which(aChangerO>1),1]-sqrt(7)/2
      
      aChangerG_sym <- (predGas_sym[,3]-predGas_sym[,2])/sqrt(10)
      predGas_sym[which(aChangerG_sym>1),3] <- predGas_sym[which(aChangerG_sym>1),1]+sqrt(7)/2
      predGas_sym[which(aChangerG_sym>1),2] <- predGas_sym[which(aChangerG_sym>1),1]-sqrt(7)/2
      
      aChangerO_sym <- (predOil_sym[,3]-predOil_sym[,2])/sqrt(10)
      predOil_sym[which(aChangerO_sym>1),3] <- predOil_sym[which(aChangerO_sym>1),1]+sqrt(7)/2
      predOil_sym[which(aChangerO_sym>1),2] <- predOil_sym[which(aChangerO_sym>1),1]-sqrt(7)/2
      #calcul score
      prediction <- formatPrediction(predOil_sym,predGas_sym,API.test)
      score <- Metric(prediction, solution)
      #################################################################
      results_oil_B <- predict.lm(Slogg, newdata=toPredict, interval ='pre',level=1-alpha)
      results_gas_B <- predict.lm(Slogo, newdata=toPredict, interval ='pre',level=1-alpha) 

      results_gas <-exp(results_gas_B) + seuil.G

      results_oil<- exp(results_oil_B) + seuil.O

    }
    results <- formatPrediction(results_oil,results_gas,variableAPI)
  }
  #je crois que cette partie peut être remplacer par step (Imane)
   #prediction linéaire sur les variables correlées
  else if(predType == "lineaireVarCor"){
    seuil<-0.8 #seuil au dessus duquel on considère les variables corrélées.
    nomFichier<<-paste(nomFichier,seuil,sep = '')
    
    #On prend le nom de toutes les variables corrélées
    corMat<-cor(subset(sampleOil, select = -c(OilCum360)))
    varCorNamesVect<-colnames(corMat)[!apply(lower.tri(corMat)&corMat>=seuil,2,any)]#ensemble des varaiables non corélées
    varCorNamesString <-paste(varCorNamesVect, collapse = " + ")
    
    #on crées nos formules de regression
    formulaOil <- as.formula(paste('sampleOil$OilCum360 ~ ',varCorNamesString))
    formulaGas <- as.formula(paste('sampleGas$GasCum360 ~ ',varCorNamesString))
    cat(length(varCorNamesVect), " variables conservées : \n",varCorNamesString,'\n')
    
    #on les appliquent dans les modèles
    OilModel <- lm(formulaOil, sampleOil)
    GasModel <- lm(formulaGas, sampleGas)
    predOil <- predict.lm(OilModel, newdata=toPredict, interval ='pre')
    predGas <- predict.lm(GasModel, newdata=toPredict, interval ='pre')

    #on renvoie les resultats
    results<-data.frame(ID = toPredict$API, 
                        GAS360_INF = predGas[,2],
                        GAS360_SUP = predGas[,3],
                        OIL360_INF = predOil[,2],
                        OIL360_SUP = predOil[,3]
    )
    
    
    
    
    
  } 
  else if (predType=="nouvel_espace"){
    if(predType2 == "acp"){
      #nomFichier<<-paste(nomFichier,seuil,sep = '')
      
      #acp uniquement sur les variables
      rawData<-subset(sampleOil, select = -c(OilCum360))
      pca <- prcomp(rawData, retx=TRUE, center=TRUE, scale=TRUE)
      
      #% d'explication.
      pca.expl <-pca$sdev^2/sum(pca$sdev^2)*100
      #Choix du nb d'axes principaux à utiliser
      nbCompoPrincip <- sum((pca$sdev^2)^(0.05) > 1)
      cat('ACP - explication :', sum(pca.expl[1:nbCompoPrincip]),"%, avec ", nbCompoPrincip, " valeurs propres.")
      
      #ecriture des formules
      varNamesVec <-paste(rep("PC",nbCompoPrincip), 1:nbCompoPrincip,sep="")
      varNamesString <- paste(varNamesVec, collapse = " + ")
      
      formulaOil <- as.formula(paste('sampleOil$OilCum360 ~ ',varNamesString))
      formulaGas <- as.formula(paste('sampleGas$GasCum360 ~ ',varNamesString))
      
      #passage en base propre
      sampleOilPropre <-data.frame(
        as.matrix(rawData) %*% pca$rotation[,1:nbCompoPrincip]
      )
      sampleGasPropre <-data.frame(
        as.matrix(rawData) %*% pca$rotation[,1:nbCompoPrincip]
      )
      
      
      
      #linearisation
      OilModel <- lm(formulaOil, sampleOilPropre)
      GasModel <- lm(formulaGas, sampleGasPropre)
      
      #Résidus
      OilModel.res <- resid(OilModel)
      GasModel.res <- resid(GasModel)
      plot(OilModel.res, col="blue", ylab="Résidus", main="Résidu pour l'ACP")
      lines(GasModel.res, col="green", type="p")
      abline(a=0,b=0)
      
      #Prediction
      #rotation des données à prévoir
      toPredictRot<- data.frame(
        as.matrix(toPredict) %*% pca$rotation[,1:nbCompoPrincip]
      )
      #prediction
      predOil <- predict.lm(OilModel, newdata=toPredictRot, interval ='pre')
      predGas <- predict.lm(GasModel, newdata=toPredictRot, interval ='pre')
      
      
      #on renvoie les resultats
      results<-data.frame(ID = variableAPI, 
                          GAS360_INF = predGas[,2],
                          GAS360_SUP = predGas[,3],
                          OIL360_INF = predOil[,2],
                          OIL360_SUP = predOil[,3]
      )
    }
    else if(predType2=="pca"){
      # On a choisi le package "FactoMineR" pour dérouler l'ACP
      library("FactoMineR")
      
      model.pca <- PCA(data.train,ncp = 15, graph = TRUE) # graph =TRUE pour produire le graph des corrélation des var avec les composantes
      
      summary(model.pca)
      
      # visualisation des résultats avec le package "factoextra"
      library("factoextra")
      
      # les valeures propres
      eig.val <- get_eigenvalue(model.pca)
      eig.val
      
      # graphe pourcentage expliqué par chaque composante
      fviz_eig(model.pca, addlabels = TRUE)
      
      # cercle des corrélations
      fviz_pca_var(model.pca, col.var = "black")
      
      #apperçu de la contribution des variables 
      head(model.pca$var$contrib)
      
      # graph du pourcentage expliqué cumulé 
      cumul_EIG <- model.pca$eig$`cumulative percentage of variance`
      barplot(cumul_EIG,main = "pourcentage cumule ",xlab = "composante",ylab = "pourcentage explique")
      abline(h=85,col="red")
    }
    else if (predType2=="pls"){
      dr2<-data
      for(i in 1:ncol(dr2)){
        na.index<-which(is.na(dr2[,i]))
        l<-length(na.index)
        if(l!=0){
          median_ir<-round(median(dr2[,i], na.rm=TRUE),5)
          # print(dr[na_index,i])
          dr2[which(is.na(dr2[,i])),i] <- median_ir
        }
      }
      summary(dr2)
      data<-dr2
      
      library(pls)
      
      # data test et training data 
      
      ind.test <- sample(1:460, size=100, replace = FALSE)
      data.test <- data[ind.test,]
      data.train <- data[-ind.test,]
      gas<-data.test[,42]
      oil<-data.test[,43]
      
      dataGas <- data.frame(Depth = data.train[, 4], Gas = data.train[, 42])
      # pour prédire Oil à partir d'une rls
      dataOil <- data.frame(Zone = data.train[, 12], Oil = data.train[, 43])
      GAZ<-dataGas$Gas
      OIL<-dataOil$Oil
      
      
      
      # PCR sur GAZ
      pcr_GAZ <- pcr(GAZ~.,data=data.train, scale=TRUE, validation="CV")
      summary(pcr_GAZ)
      pred_GAZ <- predict(pcr_GAZ,data.test, ncomp= 19)
      pred_GAZ
      
      # PCR sur OIL
      pcr_OIL <- pcr(OIL~.,data=data.train, scale=TRUE, validation="CV")
      summary(pcr_OIL)
      pred_OIL <- predict(pcr_OIL,data.test, ncomp= 19)
      pred_OIL
      
      
      #calcul des intervalles de prédiction oil par bootstrap
      nboot<-100
      predConfOil <- PredOil <- matrix(data = NA, nrow=length(data.test[,1]), ncol=nboot)
      for (i in 1:nboot){
        index <- sample(length(data.train[,1]),replace=TRUE)
        predConfOil[,i]<- predict(pcr_OIL,data.test, ncomp= 19)
        resid <- data.test$OilCum360-predConfOil[,i]
        PredOil[,i] <-predConfOil[,i] + rnorm(n = length(data.test[,1]), mean = 0, sd = sd(resid))
      }
      ICprevOil <- matrix(0,nrow=length(pred_OIL),ncol = 3)
      ICprevOil[,1] <- rowMeans(predConfOil)
      sdConf <- apply(predConfOil,1,sd)
      sdPred <- apply(PredOil,1,sd)
      ICprevOil[,2] <- ICprevOil[,1] - 1.96*sdPred
      ICprevOil[,3] <- ICprevOil[,1] + 1.96*sdPred
      
      #graphe des IP
      plot(ICprevOil[,1] ~ data.test$OilCum360,pch="*",col="black")
      segments(x0=data.test$OilCum360,y0=ICprevOil[,2],y1=ICprevOil[,3],col="blue")
      title("intervalles de prédictions PCR sur OIL ")
      
      #calcul des intervalles de prédiction gaz par bootstrap
      nboot<-100
      predConfGas <- PredGas <- matrix(data = NA, nrow=length(data.test[,1]), ncol=nboot)
      for (i in 1:nboot){
        index <- sample(length(data.train[,1]),replace=TRUE)
        predConfGas[,i]<- predict(pcr_GAZ,data.test, ncomp= 19)
        resid <- data.test$GasCum360-predConfGas[,i]
        PredGas[,i] <-predConfGas[,i] + rnorm(n = length(data.test[,1]), mean = 0, sd = sd(resid))
      }
      ICprevGas <- matrix(0,nrow=length(pred_GAZ),ncol = 3)
      ICprevGas[,1] <- rowMeans(predConfGas)
      sdConf <- apply(predConfGas,1,sd)
      sdPred <- apply(PredGas,1,sd)
      ICprevGas[,2] <- ICprevGas[,1] - 1.96*sdPred
      ICprevGas[,3] <- ICprevGas[,1] + 1.96*sdPred
      
      #graphe des IP
      plot(ICprevGas[,1] ~ data.test$GasCum360,pch="*",col="black")
      segments(x0=data.test$GasCum360,y0=ICprevGas[,2],y1=ICprevGas[,3],col="blue")
      title("intervalles de prédictions PCR sur GAZ ")
      
      
    }
          }
  else if (predType=="Ridge"){
    ind.oil<-which(colnames(data.train)=="OilCum360")
    ind.gas<-which(colnames(data.train)=="GasCum360")
    
    #5.11 Sélection de la pénalité par validation croisée
    
    modele_ridgeCV<-function(data, ind.variable){ 
      X.app=as.matrix(data[,-c(ind.gas,ind.oil)])
      Y.app=data[,ind.variable]
      out.ridge = cv.glmnet(X.app,Y.app, alpha=0,type.measure="mse", nlambda=1000)
      return(out.ridge) 
    }
    
    cv_ridgeGas<-modele_ridgeCV(data.train, ind.gas)
    plot(cv_ridgeGas)
    cv_ridgeOil<-modele_ridgeCV(data.train, ind.oil)
    plot(cv_ridgeOil)
    
    #lambda.min is the value of ???? that gives minimum mean cross-validated error. 
    #The other ?? saved is lambda.1se, which gives the most regularized model such that error 
    #is within one standard error of the minimum.
    
    modele_opt<-function(data,ind.variable,modele){
      X.app=as.matrix(data[,-c(ind.gas,ind.oil)])
      Y.app=data[,ind.variable]  
      lambda.opt=modele$lambda.min
      cat("le lambda optimal est égal à =", lambda.opt, "\n")
      app=glmnet(X.app,Y.app,lambda=lambda.opt, alpha=0)
      appr<-predict(app,newx=X.app)
      cat("L'erreur d'apprentissage est égale à =", mean((appr-Y.app)^2) , "\n")
      pred<-predict(app,newx=as.matrix(data.test[,-c(ind.gas,ind.oil)]))
      cat("L'erreur de test est égale à =", mean((pred-data.test[,ind.variable])^2), "\n" )
      
      #outils de validation
      plot(appr,data.train[,ind.variable], xlab='fitted', ylab='observed')
      abline(0,1)
      
      #etude des residus
      title("Echantillon apprentissage")
      r<-data[,ind.variable]-appr
      plot(r ~ appr , xlab="Réponse estimée",ylab="Résidus",ylim=c(-3,3))
      abline(h=2,col="red")
      abline(h=-2,col="red")
      abline(h=0,lty=2)
      title('Résidus studentisés contre la réponse prédite')
      
      
      return(app)
    }
    
    
    ridge_optGas<-modele_opt(data.train,ind.gas,cv_ridgeGas)
    beta_lambdamaxGas<-as.matrix(ridge_optGas$beta)[-1,] #ce qui nous permet d'avoir les estimateurs des 
    #betas des variables explicatives
    ridge_optOil<-modele_opt(data.train,ind.oil,cv_ridgeOil)
    beta_lambdamaxOil<-as.matrix(ridge_optOil$beta)[-1,]
  }
  else if (predType=="Lasso"){
    ind.oil<-which(colnames(data.train)=="OilCum360")
    ind.gas<-which(colnames(data.train)=="GasCum360")
    
    #5.11 Sélection de la pénalité par validation croisée
    
    modele_ridgeCV<-function(data, ind.variable){ 
      X.app=as.matrix(data[,-c(ind.gas,ind.oil)])
      Y.app=data[,ind.variable]
      out.ridge = cv.glmnet(X.app,Y.app, alpha=0,type.measure="mse", nlambda=1000)
      return(out.ridge) 
    }
    
    cv_ridgeGas<-modele_ridgeCV(data.train, ind.gas)
    plot(cv_ridgeGas)
    cv_ridgeOil<-modele_ridgeCV(data.train, ind.oil)
    plot(cv_ridgeOil)
    
    #lambda.min is the value of ???? that gives minimum mean cross-validated error. 
    #The other ?? saved is lambda.1se, which gives the most regularized model such that error 
    #is within one standard error of the minimum.
    
    modele_opt<-function(data,ind.variable,modele){
      X.app=as.matrix(data[,-c(ind.gas,ind.oil)])
      Y.app=data[,ind.variable]  
      lambda.opt=modele$lambda.min
      cat("le lambda optimal est égal à =", lambda.opt, "\n")
      app=glmnet(X.app,Y.app,lambda=lambda.opt, alpha=0)
      appr<-predict(app,newx=X.app)
      cat("L'erreur d'apprentissage est égale à =", mean((appr-Y.app)^2) , "\n")
      pred<-predict(app,newx=as.matrix(data.test[,-c(ind.gas,ind.oil)]))
      cat("L'erreur de test est égale à =", mean((pred-data.test[,ind.variable])^2), "\n" )
      
      #outils de validation
      plot(appr,data.train[,ind.variable], xlab='fitted', ylab='observed')
      abline(0,1)
      
      #etude des residus
      title("Echantillon apprentissage")
      r<-data[,ind.variable]-appr
      plot(r ~ appr , xlab="Réponse estimée",ylab="Résidus",ylim=c(-3,3))
      abline(h=2,col="red")
      abline(h=-2,col="red")
      abline(h=0,lty=2)
      title('Résidus studentisés contre la réponse prédite')
      
      
      return(app)
    }
    
    
    ridge_optGas<-modele_opt(data.train,ind.gas,cv_ridgeGas)
    beta_lambdamaxGas<-as.matrix(ridge_optGas$beta)[-1,] #ce qui nous permet d'avoir les estimateurs des 
    #betas des variables explicatives
    ridge_optOil<-modele_opt(data.train,ind.oil,cv_ridgeOil)
    beta_lambdamaxOil<-as.matrix(ridge_optOil$beta)[-1,]
  }
  else if (predType=="forets_aleatoires"){
    library(randomForest)
    #dataTrain <- as.data.frame(dataTrain)
    
    #indices des résultats
    ind.oil<-which(colnames(dataTrain)=="OilCum360")
    ind.gas<-which(colnames(dataTrain)=="GasCum360")
    #boxplot(dataTrain)
    
    
    # ind.test <- sample(1:460, size=100, replace = FALSE)
    # data.test <- dataTrain[ind.test,]
    # data.train <- dataTrain[-ind.test,]
    #
    # sample <- data.train
    # test <- data.test
    # solution <- test[,c(ncol(test)-1,ncol(test))]
    # API.test <- dataTrain.API[ind.test]
    # API.train <- dataTrain.API[-ind.test]
    # sampleOil<-sampleGas<-sample
    # gas <- sampleGas$GasCum360
    # oil <- sampleOil$OilCum360
    # sampleOil$GasCum360<-NULL
    # sampleGas$OilCum360<-NULL"
    #
    
    #L <- matrix(NA,nrow=8, ncol=5)
    #choix des paramètres pour un meilleur score
    i <- 500
    j <- 0.3
    rfPredBoot <- function(train, reponse, test,
                           nboot = 100){
      
      n <- length(train[,1])
      nNew <- length(test[,1])
      bootPred <- bootConf <- matrix(NA, nrow = nNew, ncol = nboot)
      
      for (i in 1:nboot){
        index <- sample(n, replace = TRUE)
        mboot <- randomForest(reponse[index]~. , data=train[index,],ntree=i)
        bootConf[, i] <- predict(mboot, newdata = test)
        bootPred[, i] <- bootConf[, i] + rnorm(nNew, mean = 0, sd = sqrt(mboot$mse))
        
      }
      pboot <- matrix(NA, nrow = nNew, ncol = 5)
      pboot[, 1] <- rowMeans(bootConf)
      sdConfBoot <- apply(bootConf, 1, sd)
      sdPredBoot <- apply(bootPred, 1, sd)
      pboot[, 2] <-  pboot[, 1] - (2-j)* sdConfBoot
      pboot[, 3] <- pboot[, 1] + (2-j)* sdConfBoot  
      pboot[, 4] <- pboot[, 1] - (2-j)* sdPredBoot  
      pboot[, 5] <- pboot[, 1] + (2-j)* sdPredBoot
      return(pboot)
    }
    #les intervalles par bootstrap
    O <- rfPredBoot(sampleOil[-c(45)],oil,data.test[-c(45,46)])
    G <- rfPredBoot(sampleGas[-c(45)],gas,data.test[-c(45,46)])
    #score obtenu avec data.test
    prediction <- formatPrediction(O[,c(1,4,5)],G[,c(1,4,5)],API.test)
    score <- Metric(prediction, solution)
    #plot des résultats
    plot(O[,c(1)]~ solution$OilCum360,pch="*",col='black',xlab="vraies valeurs",ylab="prédictions",lwd=10)
    
    segments(x0=solution$OilCum360,y0 = O[,c(2)],y1 = O[,c(3)],col='grey70')
    
    title("prédictions en fonction des valeurs réelles
          avec les intervalles de prédictions Gas en utilisant random forest")
    
    plot(G[,c(1)]~ solution$GasCum360,pch="*",col='black',xlab="vraies valeurs",ylab="prédictions",lwd=10)
    
    segments(x0=solution$GasCum360,y0 = G[,c(2)],y1 = G[,c(3)],col='grey70')
    
    title("prédictions en fonction des valeurs réelles
          avec les intervalles de prédictions OIL en utilisant random forest")
    
    
    
    
    #O< <- rfPredBoot(sampleOil[-c(45)],oil,data.test[-c(45,46)])
    
    #G <- rfPredBoot(sampleGas[-c(45)],gas,data.test[-c(45,46)])
    #prediction <- formatPrediction(O[,c(1,4,5)],G[,c(1,4,5)],API.test)
    #score <- Metric(prediction, solution)
    #L <- matrix(NA,nrow=8, ncol=3)
    
    
  }
  else if (predType=="neuralnetwork"){
    #Réseau de neuronnes
    set.seed(300)
    library(neuralnet)
    library(nnet)
    library(e1071)
    
    names(data.gas)[45] <- c("gas")
    names(data.oil)[45] <- c("oil")
    n <- names(data.gas)
    f <- as.formula(paste("gas ~", paste(n[!n %in% "gas"], collapse = " + ")))
    model_neur <- neuralnet(f ,data=data.gas[,-c(45)], hidden=10,linear.output = TRUE,rep=3, threshold=0.001)
    #print(model_neur)
    plot(model_neur)
    pre_neur <- compute(model_neur,data.test[,-c(45,46)]) #, type="raw")
    pre_n<- sapply(pre_neur$net.result, FUN ="array")
    d <- data.frame(data.test[,c(45)],pre_neur$net.result)
    plot(d)
    
    model_neur1 <- nnet(gas~.,data=data.gas[,-c(45)],size=10,decay=1, linout=TRUE,maxit=500)
    summary(model_neur1)
    predic <- predict(model_neur1, data.test[,-c(47,46)])
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
    ###############################################################
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
    
    
  }
  else if (predType=="Xgboost"){
    library(xgboost)
    ##xgboost
    ###############################################
    data.gas_xgboost <- data.matrix(sampleGas[-c(45)])
    gas <- data.matrix(sampleGas[c(45)])
    
    param <- list(max_depth = 7, eta = 0.1, nthread = 3, 
                  nrounds = 2000, objective = obj,
                  eval_metric ='rmse')
    
    GasModel1 <- xgboost(data = data.gas_xgboost, label = gas, 
                         max_depth = 7, eta = 0.1, nthread = 3, 
                         nrounds = 2000, objective = obj,
                         eval_metric ='rmse')
    toPredict <- data.matrix(data.test[,-c(45,46)])
    predGas <- predict(GasModel1, toPredict)
    plot(predGas,gas_test)
    #abline(a=0,b=1)
    legend(-1, 4,legend=paste(names(param),param,sep="="),cex = 0.6)
    err <- sum(abs(predGas-gas_test))
    title("xgboost",paste("err",err,sep="="))
    # ############################################################
    # #intervalles de prédictions
    # #params =list(quant_alpha,quant_delta,quant_thres,quant_var,nrounds=1000,
    # #max_depth=3, alpha=5,
    # #lambda=1.0, gamma=0.5)
    # #xgboost parameters (n_estimators/max_depth/regalpha/reglambda/gamma)
    # quant_alpha<-0.95
    # quant_delta <- 1.0
    # quant_thres <- 6.0
    # quant_var <- 3.2
    # dtrain_gas <- xgb.DMatrix(data = data.gas_xgboost, label=gas)
    # quantile_loss <- function(y_pred,dtrain){#,alpha,delta,threshold,var)
    #   y_true <- as.numeric(getinfo(dtrain, "label"))
    #   x <- y_true-y_pred
    #   grad <- (x<(alpha-1.0)*delta)*(1.0-alpha)-((x>=(alpha-1.0)*delta)&(x<alpha*delta) )*x/delta-alpha*(x>alpha*delta)
    #   hess <- ((x>=(alpha-1.0)*delta)&(x>alpha*delta) )/delta
    #   len <- length(y_true)#à vérifier 
    #   var <- (2*sample(0:1,len)-1.0)*var
    #   grad <- (abs(x)<threshold)*grad-(abs(x)>=threshold)*var
    #   hess <- (abs(x)<threshold)*hess+(abs(x)>=threshold)
    #   return(grad,less)
    # }
    # obj <- function(yhat, dtrain){
    #   y <- as.numeric(getinfo(dtrain, "label"))
    #   yhat <- as.numeric(yhat)
    #   grad <- ( yhat - y )
    #   hess <- rep(1.0, length(y))
    #   return(list(grad=grad, hess=hess))
    # }
    # GasModel1 <- xgboost(data = dtrain_gas,booster = "gbtree",
    #                      nrounds=1000,alpha=5,
    #                      lambda=1.0,gamma=0.5,
    #                      label = gas,max_depth = 5, 
    #                      eta = 0.1, nthread = 3, 
    #                      objective = quantile_loss)
    # 
    # 
    # 
    # gas_test <- data.matrix(data.test$data[c(45)])
    # dtrain_gas <- xgb.DMatrix(data = data.gas_xgboost, label=gas)
    # dtest_gas <- xgb.DMatrix(data = toPredict, label=gas_test)
    # 
    # 
    # watchlist <- list(train=dtrain_gas, test=dtest_gas)
    # param <- list(max_depth=7, 
    #               eta=0.01, nthread = 8, nrounds=2000)
    # GasModel2 <- xgb.train(data=dtrain_gas,  
    #                        watchlist=watchlist,max_depth=7, 
    #                        eta=0.01, nthread = 8, nrounds=2000)
    # 
    # toPredict <- data.matrix(data.test$data[,-c(45,46)])
    # predGas <- predict(GasModel2, toPredict)
    # plot(predGas,gas_test)
    # abline(a=0,b=1)
    # legend(-1, 4,legend=paste(names(param),param,sep="="),cex = 0.6)
    # 
    # err <- sum(abs(predGas-gas_test))
    # title("xgb.train",paste("err",err,sep="="))
    # 
    # 
    # GasModel3 <- xgb.train(data=dtrain_gas,booster = "gbtree", max_depth=7, 
    #                        eta=0.1, nthread = 8, nrounds=2000, 
    #                        watchlist=watchlist)
    # param<-list(booster = "gbtree", max_depth=7, 
    #             eta=0.1, nthread = 8, nrounds=2000)
    # predGas <- predict(GasModel3, toPredict)
    # plot(predGas,gas_test)
    # abline(a=0,b=1)
    # legend(-1, 4,legend=paste(names(param),param,sep="="),cex = 0.6)
    # 
    # err <- sum(abs(predGas-gas_test))
    # title("xgb.train",paste("err",err,sep="="))
    # 
    # importance_matrix <- xgb.importance(model = GasModel3)
    # print(importance_matrix)
    # xgb.plot.importance(importance_matrix = importance_matrix)
    # #to plot the tree
    # library(DiagrammeR)
    # xgb.plot.tree(model = GasModel3)
    # #cross validation
    # param <- list(max_depth=7, eta=0.1, nthread = 7, nrounds=2000, 
    #               watchlist=watchlist, silent=1)
    # nround <- 2000
    # cv_gas <- xgb.cv(param, dtrain_gas, nround, nfold=5,
    #                  metrics='rmse', showsd = FALSE)
    # res <- xgb.cv(params = param, data = dtrain_gas, nrounds = nround, 
    #               nfold = 10, prediction = TRUE)
    # 
    # toPredict <- data.matrix(data.test$data[,-c(45,46)])
    # predGas <- predict(GasModel1, toPredict)
    # gas_test <- data.matrix(data.test$data[c(45)])
    # monModel_gas<-function(bootIndex){
    #   bootIndex<-360
    #   data.gas_xgboost <- data.matrix(data.gas[bootIndex,])
    #   toPredict <- data.matrix(data.test$data[,-c(45,46)])
    #   dtrain_gas <- xgb.DMatrix(data = data.gas_xgboost, label=data.gas_xgboost[,45])
    #   dtest_gas <- xgb.DMatrix(data = toPredict, label=data.matrix(data.test$data[,c(45)]))
    #   
    #   
    #   watchlist <- list(train=dtrain_gas, test=dtest_gas)
    #   GasModel3 <- xgb.train(data=dtrain_gas,method = "xgbTree", max_depth=7, 
    #                          eta=0.7, nthread = 2, nrounds=200, 
    #                          watchlist=watchlist,alpha=0.15)
    #   #model<- nnet(gas~.,data=data.gas[bootIndex,],size=10,decay=1, linout=TRUE,maxit=500)
    #   #model sur les données bootstrapées
    #   #model<-lm(out~x, data.frame(x = inp[bootIndex], out = out[bootIndex]))
    #   data <- data.matrix(data.gas_xgboost[-c(45)])
    #   residuals2 <- predict(GasModel3,data)
    #   #prediction sur l'ensemble des données
    #   predi <- predict(GasModel3, toPredict)
    #   
    #   #predi<-predict(model, newdata = data.frame(x = inp))$fit
    #   
    #   #on retourne les deux
    #   return(list(residus = residuals, pred=predi))
    # }
    # monModel_oil <- function(bootIndex){
    #   model<- nnet(oil~.,data=data.oil[bootIndex,],size=10,decay=1, linout=TRUE,maxit=500)
    #   #model sur les données bootstrapées
    #   #model<-lm(out~x, data.frame(x = inp[bootIndex], out = out[bootIndex]))
    #   
    #   #prediction sur l'ensemble des données
    #   predi <- predict(model_neur1, data.test[,-c(45,46)])
    #   #predi<-predict(model, newdata = data.frame(x = inp))$fit
    #   
    #   #on retourne les deux
    #   return(list(residus = model$residuals, pred=predi))
    # }
    # 
    # int_pred_oil <- predInterval(monModel_oil,repLength=360, n_data=100)
    # int_pred_gas <- predInterval(monModel_gas,repLength=360, n_data=100)
    # 
    # 
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
    prediction <- formatPrediction(predOil_sym,predGas_sym,API.test)
    score <- Metric(prediction, solution)
    
    
    
  }
  else if (predType=="cobra"){
    ind.oil<-which(colnames(data.train)=="OilCum360")
    ind.gas<-which(colnames(data.train)=="GasCum360")
    
    #COBRA
    library(lars)
    library(tree)
    library(randomForest)
    library(ridge)
    library(COBRA)
    
    
    
    cob<-function(response, data, data_test, obs){
      if(require(lars) && require(tree) && require(ridge) &&
         require(randomForest))
      {
        set.seed(1)
        res <- COBRA(train.design = as.matrix(data),
                     train.responses = as.vector(response),
                     test = as.matrix(data_test) , progress= TRUE, logGrid=TRUE) 
        
        plot(res$predict,obs)
        abline(0,1)
        title("Echantillon test")
        cat("L'erreur de test est égale à =", mean((res$predict-obs)^2), "\n" )
        
      }
    }
    
    cobra_gas<-cob(data.train$GasCum360, data.train[,-c(ind.gas,ind.oil)], data.test[,-c(ind.gas,ind.oil)], data.test$GasCum360)
    cobra_oil<-cob(data.train$OilCum360, data.train[,-c(ind.gas,ind.oil)], data.test[,-c(ind.gas,ind.oil)], data.test$OilCum360)
    
    nboot <- 500
    Mboot <- matrix(NA, nrow = nrow(dr2), ncol = nboot)
    Mboot2 <- matrix(NA, nrow = nrow(dr2), ncol = nboot)
    
    for (i in 1:nboot){
      set.seed(i)
      n<-nrow(dr2)
      index <- sample(n, replace = TRUE)
      set.seed(190)
      mboot <- cob("Gas", dr[index,],dr[index,42]) #Gas
      set.seed(190)
      mboot2<- cob("Oil", dr[index,],dr[index,43])
      Mboot[, i] <- mboot$predict
      Mboot2[, i] <-mboot2$predict
      
      
      mboot$residuals<-dr2[,42]-mboot$predict
      mboot2$residuals<-dr2[,43]-mboot2$predict
      Mboot[, i] <- Mboot[, i] + sample(mboot$residuals, size = n, replace = TRUE)
      Mboot2[, i] <- Mboot2[, i] + sample(mboot2$residuals, size = n, replace = TRUE)
      
    }
    #on fait qu'estimer les résidus...
    
    pboot <- matrix(NA, nrow = nrow(dr2), ncol = 3)
    pboot[, 1] <- rowMeans(Mboot)
    pboot[, 2] <- apply(Mboot, 1, quantile, 0.3) 
    pboot[, 3] <- apply(Mboot, 1, quantile, 0.7)
    pboot<-as.data.frame(pboot)
    colnames(pboot[,2:3])<-c("fit","lwr", "upr")
    
    pboot2 <- matrix(NA, nrow = nrow(dr2), ncol = 3)
    pboot2[, 1] <- rowMeans(Mboot2)
    pboot2[, 2] <- apply(Mboot2, 1, quantile, 0.3) 
    pboot2[, 3] <- apply(Mboot2, 1, quantile, 0.7)
    pboot2<-as.data.frame(pboot)
    colnames(pboot2)<-c("fit","lwr", "upr")
    
    
    ktest <- 100
    score <- score.fun(ktest,pboot,pboot2,dr2)
    score
    
    arrows(x0 = xnew, y0 = pboot[, 2], y1 = pboot[, 3], col = "blue", code = 3, angle = 90, length = 0.05)
    points(xnew, pboot[, 1], pch = 19)
    legend('topleft', legend = c("Exact", paste("Bootstrap (", option, ")", sep = "")), 
           col = c("grey", "blue"), lty = c(1,1))
    
    
    formatPrediction<-function(predictionOil, predictionGas, listAPI){
      res<-data.frame(ID = listAPI,
                      GAS360_INF = predictionGas[,2],
                      GAS360_SUP = predictionGas[,3],
                      OIL360_INF = predictionOil[,2],
                      OIL360_SUP = predictionOil[,3]
      )
      return(res)
    }
    
  }
  else if (predType == "liearElastic"){
    ind.oil<-which(colnames(data.train)=="OilCum360")
    ind.gas<-which(colnames(data.train)=="GasCum360")
    
    #5.11 Sélection de la pénalité par validation croisée
    
    modele_elnetCV<-function(data, ind.variable){ 
      X.app=as.matrix(data[,-c(ind.gas,ind.oil)])
      Y.app=data[,ind.variable]
      out.elnet = cv.glmnet(X.app,Y.app, alpha=0.5,type.measure="mse", nlambda=1000)
      return(out.elnet) 
    }
    
    cv_elnetGas<-modele_elnetCV(data.train, ind.gas)
    plot(cv_elnetGas)
    cv_elnetOil<-modele_elnetCV(data.train, ind.oil)
    plot(cv_elnetOil)
    
    #lambda.min is the value of ???? that gives minimum mean cross-validated error. 
    #The other ?? saved is lambda.1se, which gives the most regularized model such that error 
    #is within one standard error of the minimum.
    
    modele_opt<-function(data,ind.variable,modele){
      X.app=as.matrix(data[,-c(ind.gas,ind.oil)])
      Y.app=data[,ind.variable]  
      lambda.opt=modele$lambda.min
      cat("le lambda optimal est égal à =", lambda.opt, "\n")
      app=glmnet(X.app,Y.app,lambda=lambda.opt, alpha=0.5)
      appr<-predict(app,newx=X.app)
      cat("L'erreur d'apprentissage est égale à =", mean((appr-Y.app)^2) , "\n")
      pred<-predict(app,newx=as.matrix(data.test[,-c(ind.gas,ind.oil)]))
      cat("L'erreur de test est égale à =", mean((pred-data.test[,ind.variable])^2), "\n" )
      
      #outils de validation
      plot(appr,data.train[,ind.variable], xlab='fitted', ylab='observed')
      abline(0,1)
      
      #etude des residus
      title("Echantillon apprentissage")
      r<-data[,ind.variable]-appr
      plot(r ~ appr , xlab="Réponse estimée",ylab="Résidus",ylim=c(-3,3))
      abline(h=2,col="red")
      abline(h=-2,col="red")
      abline(h=0,lty=2)
      title('Résidus studentisés contre la réponse prédite.')
      
      
      
      return(app)
    }
    
    
    elnet_optGas<-modele_opt(data.train,ind.gas,cv_elnetGas)
    beta_lambdamaxGas<-as.matrix(elnet_optGas$beta)[-1,] #ce qui nous permet d'avoir les estimateurs des 
    #betas des variables explicatives
    elnet_optOil<-modele_opt(data.train,ind.oil,cv_elnetOil)
    beta_lambdamaxOil<-as.matrix(elnet_optOil$beta)[-1,]
  }
  else if (predType=="autre"){
    
  }
  
  #retour du rÃ©sultat
  return(results=results,model.oil=OilModel,model.gas=GasModel)
}
