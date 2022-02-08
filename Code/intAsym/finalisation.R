#Sauvegarde les données dans un fichier csv à destination du site web total. Le nom du fichier pourrait sans doute être adapté aux paramêtres ?
sauvegarder <- function(data){
  if(!exists("nomFichier")){
    nomFichier<<-'submit'
  }
  write.table(data, paste("data/",nomFichier,".csv",sep=''), sep = ";",
              quote = FALSE, row.names = FALSE)
  cat("\nSaved at",format(Sys.time(), "%X"),"in : ",nomFichier,".csv\n")
  cat("dimension de la sortie : ", dim(data), "\n")
  cat("variance des colonnes :\n", apply(prediction[-1], 2, var))
}




#Calcul la metrique d'une solution
Metric <- function(prediction, solution) {
  #On peut calculer la metrique seulement si la solution est de la même taille que la prédiction
  if(length(solution)==1){
    cat("(metric pas dispo)\n")
    return()
    }
  
  marge <- (prediction$OIL360_SUP - prediction$OIL360_INF) * (prediction$GAS360_SUP - prediction$GAS360_INF)
  ecart <- ifelse(
    (solution$OilCum360 >= prediction$OIL360_INF & solution$OilCum360 <= prediction$OIL360_SUP)
    & (solution$GasCum360 >= prediction$GAS360_INF & solution$GasCum360 <= prediction$GAS360_SUP)
    , marge, 10)
  metric = mean(ecart)
  
  #On le plot
  abscissesValues <-seq(1:length(solution$OilCum360))
  plot(abscissesValues,solution$OilCum360, ylab = "Oil solution", xlab = "#")
  arrows(abscissesValues, prediction$OIL360_INF, abscissesValues, prediction$OIL360_SUP, code = 3, angle = 90, length = 0.05,col = 'grey')
  title("OilCum360")
  
  abscissesValues <-seq(1:length(solution$GasCum360))
  plot(abscissesValues,solution$GasCum360, ylab = "Gas solution", xlab = "#")
  arrows(abscissesValues, prediction$GAS360_INF, abscissesValues, prediction$GAS360_SUP, code = 3, angle = 90, length = 0.05,col = 'grey')
  title("GasCum360")
  
  
  cat("metric=",metric,"\n")
  return(metric)
}