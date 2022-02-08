
#Remplie les valeurs vides dans un ensemble de données
RemplirValeursVides <- function(data){
  #Remplissage par la moyenne	
  res=data
  for(i in 1:ncol(res)){
    if(is.numeric(res[,i])){
      res[is.na(res[,i]), i] <- mean(res[,i], na.rm = TRUE)
    }
  }
  
  return(res)
}

#Traite les dates, faut-il les supprimer ? garder brutes ? les supprimer mais garder la différence entre deux dates dans une nouvelle variable ?
TraiterLesDates <- function(data){
  #supprimer les vecteurs date TODO
  res <- data[-c(4,5,6)]
  return(res)
}