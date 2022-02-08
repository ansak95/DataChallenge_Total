passage<<-NULL

#Execute le prétraitement, pour le jeu de donnée 'data' de typeOfData 'train' ou 'test'
preTraitement<-function(data, typeOfData){
  if(typeOfData=='train'){
    oil<-data$OilCum360
    gas<-data$GasCum360
    data$OilCum360<-NULL
    data$GasCum360<-NULL
  }
  x<-data
  
  #enlevons la variable API parce qu'elle ne sert à rien !
  x<-x[,-1]
  
  ####formatage des dates
  x<-FormaterDates(x, typeOfData)
  
  ####enlever outliers
  x<-EnleverOutliers(x, typeOfData)
  
  ####traitement des predicteurs
  x<-CreerVariablesQuali(x, typeOfData)
  x<-RemplirValeursVides(x)$imp
  
  
  x<-TraiterSurf(x, typeOfData)
  x<-CreerAge(x, typeOfData)
  
  
  if(typeOfData=='train'){
    x$OilCum360<-oil
    x$GasCum360<-gas
  }
  
  return(x)
}

#Enlever outliers
#C'est un outliers si l'enlever change significativement la variance
EnleverOutliers<-function(data, type){
  seuil<-0.02 #seuil d'amputation en %
  
  if(type=='test'){
    nech<-nrow(data)
    data<-rbind(data, dataTrainOutliers)
  }
  outliers<-matrix(FALSE, nrow = nrow(data), ncol=ncol(data))
  newData<-data

  #pour tous les prédicteurs...
  for(i in 1:ncol(data)){
    dd<-newData[,i]
    n<-length(dd)
    m<-round(n%*% 0.05)[1]
    o<-order(newData[,i])
    dd<-sort(dd,na.last = TRUE)
    nna<-length(sort(dd))
    
    v<-var(dd,na.rm = TRUE )
    
    #pour chaque individus...
    for(j in 1:m){
      if(abs(var(dd[-j], na.rm = TRUE)/v-1)> seuil){
        outliers[o[j],i]<-TRUE
        newData[o[j],i]<-TRUE
      }
      if(abs(var(dd[-(nna-j+1)], na.rm = TRUE)/v-1)> seuil){
        outliers[o[nna-j+1],i]<-TRUE
        newData[o[nna-j+1],i]<-TRUE
      }
    }
  }
  
  if(type=='train'){  
    dataTrainOutliers<<-newData
  }else if(type=='test'){
    newData<-newData[1:nech,]
  }
  
  
  
  cat("Nombre d'outliers pour ", type,": ", sum(outliers), "\n")
  vx<-apply(data, 2, function(xxx) return(var(xxx, na.rm=TRUE)))
  vnewx<-apply(newData, 2, function(xxx) return(var(xxx, na.rm=TRUE)))
  cat("% de variance gagnée : ",  sum( (vx-vnewx)^2/vx^2 ),"\n" )
  
  return(newData)
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
    seuil.polution<-0.7
    
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

FormaterDates<-function(data, type){
  res<-data
  datesPredictors<-c('Date_Drilling',
                     'Date_Completion',
                     'Date_Production')
  
  #on selectionne les dates
  dates<-res[,datesPredictors]
  
  #on convertie toutes les dates en numerique
  dates[]<-lapply(dates, as.character)
  num<-matrix(NA,ncol = 3,nrow=length(res$Date_Completion))
  mini<-c(0,0,0)
  for(i in 1:length(res$Date_Drilling)){
    for(j in 1:3){
      if(!is.na(dates[i,j])){
        num[i,j]<-(as.POSIXct(dates[i,j], format="%d/%m/%Y"))
        num[i,j]<-as.numeric(num[i,j])
      }
    }
  }
  
  if(type=='train'){
    FormaterDates.m<<-mean(num, na.rm = TRUE)
    FormaterDates.v<<-sd(num, na.rm = TRUE)
  }
  
  num<-(num-FormaterDates.m)/FormaterDates.v
    
  res[,datesPredictors]<-num
  return(res)
}

CreerVariablesQuali<-function(data, type){
  if(type=='train'){
    p<-ncol(data)
    maxQuali<-53
    nbClasses <- rep(0,p)
    for(i in 1:p){
      nbClasses[i] <- dim(table(data[,i]))
    }
    
    indexVarQuali<<-which(nbClasses<maxQuali)
    cat(length (indexVarQuali), " variables qualitatives\n")
  }
  
  for(i in indexVarQuali){
    data[,i]<-as.factor(data[,i])
  }
  
  return(data)
}

#Transforme les trois dates en age
CreerAge <-function(data, type){
  res<-data
  
  res$Age_Production_Completion<- data$Date_Production - data$Date_Completion
  #res$Age_Production_Drilling<-   data$Date_Production - data$Date_Drilling
  res$Age_Completion_Drilling<-   data$Date_Completion - data$Date_Drilling
  
  res$Date_Production<-NULL
  res$Date_Completion<-NULL
  #res$Date_Drilling<-NULL
  return(res)
}	

#Traite les données qualtiatives
TraiterSurf<-function(data, type){
  library(dbscan)
  pos<-cbind(data$Surf_X, data$Surf_Y)
  
  if(type=='train'){
    TraiterSurf.modele<<-NULL
    #surf_X et surf_Y
    eps<-0.1#distance minimal pour considérer comme voisin, empirique
    TraiterSurf.modele<<-dbscan(pos, eps = eps, minPts = 1)
    TraiterSurf.train<<-pos
    TraiterSurf.mean<<-mean(TraiterSurf.modele$cluster)
    TraiterSurf.sd<<-sd(TraiterSurf.modele$cluster)
    
    cat("Nombre de cluster de Surf : ",max(TraiterSurf.modele$cluster),"\n")
    
    res<-TraiterSurf.modele$cluster
  }else{
    res<-predict(object = TraiterSurf.modele, newdata=pos, data=TraiterSurf.train)
  }
  
  res<-(res-TraiterSurf.mean)/TraiterSurf.sd
  
  data$Surf_X<-NULL
  data$Surf_Y<-NULL
  data$Surf<-as.factor(res)
  
  return(data)
}
