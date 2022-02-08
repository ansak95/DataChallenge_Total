##code principale pas finalisé
rm(list=ls(all=TRUE))
source("./importerDonnees.R")
source("./pretraitement3.R")
source("./traitement.R")

source("./finalisation.R")

  
###Fonction principale qui permet de lancer les calculs.
###NE PAS EDITER
nomFichier<<-'submit'

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



#prediction <- ElPredictador(dataTrain, dataTest, dataTest.API)
#normPred <- Metric(prediction, solution = dataTestResults)

#prediction<-ameliorerIntervals(prediction, dataTestResults)
#sauvegarder(prediction)
