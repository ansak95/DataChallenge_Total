preTraitement<-function(data){
  
  x<-data
  
  #Traitement des valeurs qualitatives
  #x <- TraiterQualitatives(x)
  
  #Traitement des dates
  x <- TraiterLesDates(x)
  
  #Traitement initial des donnÃ©es
  vv<- RemplirValeursVides(x)
  x<-vv$imp
  
  #x <- CreerAge(x)
  
  #enlevons la variable API parce qu'elle ne sert à rien !
  x<-x[,-1]
  
  return(list(data=x, varialbesToDeleted = vv$toDelete))
}

#Remplie les valeurs vides dans un ensemble de donnÃ©es
RemplirValeursVides <- function(data){
  meth<-'missForest'
  res<-data
  
  if(meth=='moy'){
    #Remplissage par la moyenne	
    for(i in 1:ncol(res)){
      if(is.numeric(res[,i])){
        res[is.na(res[,i]), i] <- mean(res[,i], na.rm = TRUE)
      }
    }
  }else if(meth=='clear'){
    #suppression de lignes contenant des valeurs NA
    rowIndex <- unique(which(is.na(data), arr.ind=TRUE)[,1])
    res[rowindex,]<-NULL
  }else if(meth=='missForest'){
    library(missForest)
    seuil.polution<-1.0
    
    reduction<-missForest(data, verbose=FALSE, variablewise = TRUE)
    var.pertinentes<-reduction$OOBerror<seuil.polution
    for(i in 1:length(var.pertinentes)){
      if(!var.pertinentes[i]){
        cat("/!\\", names(data)[i], " erreur d\'imputation à ", reduction$OOBerror[i],"\n")
      }
    }
    cat("Erreur totale d'imputation de : ", 100*sqrt(sum(reduction$OOBerror[var.pertinentes]^2))/sum(var.pertinentes),"%\n")
    
    res<-reduction$ximp
    td<-which(var.pertinentes==FALSE)
    return(list(imp=res, toDelete=td))
    
  }
  
  return(res)
}

#Traite les dates, faut-il les supprimer ? garder brutes ? les supprimer mais garder la diffÃ©rence entre deux dates dans une nouvelle variable ?
TraiterLesDates <- function(data){
  
  meth<-'age'
  res <- data
  
  if(meth=='clear'){
    #supprimer les vecteurs date
    res$Date_Drilling<-NULL
    res$Date_Completion<-NULL
    res$Date_Production<-NULL
  }else if(meth=='age'){
    #remplace par la distance des trois date
    dates<-res[,c('Date_Drilling',
                  'Date_Completion',
                  'Date_Production')]
    dates[]<-lapply(dates, as.character)
    num<-matrix(NA,ncol = 3,nrow=length(res$Date_Completion))
    for(i in 1:length(res$Date_Drilling)){
      for(j in 1:3){
        if(!is.na(dates[i,j])){
          num[i,j]<-(as.POSIXct(dates[i,j], format="%d/%m/%Y"))
          num[i,j]<-as.numeric(num[i,j])
        }
      }
    }
    num<-scale(num,center = TRUE, scale = TRUE)
    res[,c('Date_Drilling',
           'Date_Completion',
           'Date_Production')]<-num
  }
  
  return(res)
}

#Transforme les trois dates en age
CreerAge <-function(data){
  res<-data
  
  res$Age_Production_Completion<-data$Date_Production-data$Date_Completion
  res$Age_Production_Drilling<-data$Date_Production-data$Date_Drilling
  res$Age_Completion_Drilling<-data$Date_Completion-data$Date_Drilling
  
  res$Date_Production<-NULL
  res$Date_Completion<-NULL
  res$Date_Drilling<-NULL
  return(res)
}	


#Traite les données qualtiatives
TraiterQualitatives<-function(data){
  library(dbscan)
  #surf_X et surf_Y
  eps<-0.1#distance minimal pour considérer comme voisin, empirique
  pos<-cbind(data$Surf_X, data$Surf_Y)
  clusterSurf<<-dbscan(pos, eps = eps, minPts = 1)
  
  cat("Cluster de Surf_X et Surf_Y : ",max(clusterSurf$cluster))
  
  data$Surf_X<-NULL
  data$Surf_Y<-NULL
  data$Surf<-cl$cluster
  
  return(data)
}
