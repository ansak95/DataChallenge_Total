#Fonction permettant charger les donn√©es d'apprentissages et les donn√©es tests.
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
    validResults <- validation[c(46,47)] #on garde les rÈsultats
    validation <- validation[-c(46, 47)] #on enlËve les rÈsultats
  
  }
 return(list(train = train, validation = validation, validResults = validResults))
}