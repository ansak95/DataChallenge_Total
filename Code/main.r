rm(list=ls(all=TRUE))
source("./importerDonnees.R")
source("./pretraitement.R")
source("./traitement.R")
source("./finalisation.R")

  
###Fonction principale qui permet de lancer les calculs.
###NE PAS EDITER
nomFichier<<-'submit'

#importation brute
data <-ImportData()
dataTrain <- data$train
dataTest <- data$validation
dataTestResults <- data$validResults

#pretraitement des données
dataTest.API<-dataTest$API
ptTrain<-preTraitement(dataTrain)
  dataTrain<-ptTrain$data
dataTest <-preTraitement(dataTest)$data

dataTest<-dataTest[,-ptTrain$varialbesToDeleted]
dataTrain<-dataTrain[,-ptTrain$varialbesToDeleted]
  
prediction <- ElPredictador(dataTrain, dataTest, dataTest.API)
normPred <- Metric(prediction, solution = dataTestResults)

prediction<-ameliorerIntervals(prediction, dataTestResults)
sauvegarder(prediction)
