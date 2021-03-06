#Fonction permettant charger les données d'apprentissages et les données tests.
ImportData <- function(methode = 'normal') {
 
  if(methode=='normal') {
   train <- read.csv2("./data/TrainSample.csv") 
   validation <- read.csv2("./data/TestSample.csv")
   validResults <- NA
  } else if(methode=='cross'){
    ratio <- 0.75
    data <-read.csv2("./data/TrainSample.csv") 
    permutation <- sample(nrow(data), nrow(data)*ratio)
    
    train <- data[permutation,]
    validation <- data[-permutation,]
    validResults <- validation[c(46,47)] #on garde les r�sultats
    validation <- validation[-c(46, 47)] #on enl�ve les r�sultats
  
  }
 return(list(train = train, validation = validation, validResults = validResults))
}