oil<-dataTrain$OilCum360
gas<-dataTrain$GasCum360
x<-dataTrain[,-c(46,47)]
all<-dataTrain
test<-dataTest

p<-dim(x)[2]
n<-dim(x)[1]



library(corrplot)
library(ggplot2)
#Analyse exploratoire sur données complètes
#valeurs manquantes
barplot(sort(sapply(x, function(xx) sum(is.na(xx))/nrow(x)), decreasing = TRUE), xlab = "prédicteurs", ylab = "% de NA")
title("% de données manquantes dataTrain")
sort(sapply(x, function(xx) sum(is.na(xx))/nrow(x)), decreasing = TRUE)

barplot(sort(sapply(test, function(xx) sum(is.na(xx))/nrow(test)), decreasing = TRUE), xlab = "prédicteurs", ylab = "% de NA")
title("% de données manquantes dataTest")
sort(sapply(test, function(xx) sum(is.na(xx))/nrow(test)), decreasing = TRUE)

#Création des familles
pairs(x[,1:10])
families<-c(0,1,1,2,2,2,rep(3, 20), rep(4,17), 5,5)
families[27]<-1
families[c(11,12,13,14)]<-31
families[c(10,9,15)]<-32
families[c(7,8,28)]<-33
families[16:26]<-32
families[39:41]<-41
families[36:38]<-42
families[42:43]<-43
families[33:35]<-44
families[30:31]<-45

#Affichage des données
if(TRUE){
setwd("../Rapport/images/familles")
png(filename="pairsFamille1.png")
plot(x[,families==1], main="Les positions")
dev.off()
png(filename="pairsFamille2.png")
plot(x[,families==2], main="Les dates")
dev.off()
png(filename="pairsFamille4.png")
plot(x[,families==4], main="Autres données d'exploitation")
dev.off()
png(filename="pairsFamille5.png")
plot(oil, gas, main="Production")
dev.off()
png(filename="pairsFamille31.png")
plot(x[,families==31], main="Les volumes introduis")
dev.off()
png(filename="pairsFamille32.png")
plot(x[,families==32], main="Caract mesurées réservoir")
dev.off()
png(filename="pairsFamille33.png")
plot(x[,families==33], main="Caract physiques réservoir")
dev.off()
png(filename="pairsFamille41.png")
plot(x[,families==41], main="Donnees sur le shut-in")
dev.off()
png(filename="pairsFamille42.png")
plot(x[,families==42], main="Plage de ratio de pompage de fluide")
dev.off()
png(filename="pairsFamille43.png")
plot(x[,families==43], main="Données sur les shots")
dev.off()
png(filename="pairsFamille44.png")
plot(x[,families==44], main="Plage de pression d'injection")
dev.off()
png(filename="pairsFamille45.png")
plot(x[,families==45], main="Utilisation proppant")
  dev.off()


png(filename="reponseSorted.png")
plot(sort(oil), type='l', col='blue', ylab = "Valeurs ordonées des réponses")
lines(sort(gas), type='l', col='red')
title("Valeurs ordonnées de Oil (bleu) et Gas (rouge)")
dev.off()

setwd("../../../Code")
}

#Missing data
plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}
plot_Missing(x)
plot_Missing(test)

#Enlever outliers
#outliers spotted:
#  x$Frac_Gradient..PSI.ft.[51]<-NA
#  x$Min_Rate_Slurry..bpm.[388]<-NA
#  x$Res_PPLS..Ohmm.[160]<-NA
#C'est un outliers si l'enlever change significativement la variance
outliers<-matrix(FALSE, nrow = nrow(x), ncol=ncol(x))
newx<-x
seuil<-0.02
for(i in 1:ncol(x)){
  dd<-x[,i]
  n<-length(dd)
  m<-round(n%*% 0.05)[1]
  o<-order(x[,i])
  dd<-sort(dd,na.last = TRUE)
  nna<-length(sort(dd))
  
  v<-var(dd,na.rm = TRUE )
  
  for(j in 1:m){
    if(abs(var(dd[-j], na.rm = TRUE)/v-1)> seuil){
      outliers[o[j],i]<-TRUE
      newx[o[j],i]<-TRUE
    }
    if(abs(var(dd[-(nna-j+1)], na.rm = TRUE)/v-1)> seuil){
      outliers[o[nna-j+1],i]<-TRUE
      newx[o[nna-j+1],i]<-TRUE
    }
  }
}
cat("Nombre d'outliers : ", sum(outliers), "\n")
vx<-apply(x, 2, function(xxx) return(var(xxx, na.rm=TRUE)))
vnewx<-apply(newx, 2, function(xxx) return(var(xxx, na.rm=TRUE)))
cat("% de variance gagnée : ",  sum( (vx-vnewx)^2/vx^2 ),"\n" )

#Variables qualitatives
if(TRUE){
  maxQuali<-80
  # maxQuasiQuali<-200
  
  nbClasses <- rep(0,p)
  for(i in 1:p){
    nbClasses[i] <- dim(table(x[,i]))
  }
  
  barplot(sort(nbClasses), xlab = "prédicteurs (ordonnés)", ylab = "cardinal de l'espace image")
  abline(maxQuali,0, col='red')
  # abline(maxQuasiQuali,0, col='blue')
  title("Nombre de valeurs prises par les prédicteurs")
  
  indexVarQuali<-which(nbClasses<maxQuali)
  varQualitatives <- names(x)[indexVarQuali]
  cat(length(varQualitatives),"variables Qaulitatives :\n",varQualitatives,"\n")
  
}

#Transformation des predicteurs
#1
plot(x$Vcarb_PPLS, x$Vsand_PPLS, main = "Relation quasi linéaire entre Vcarb et Vsand")
m<-lm(Vcarb_PPLS~Vsand_PPLS,x)
abline(0,m$coefficients[2])
summary(m)
anova(m)

#2
Med_rate_Slurry = 0.5*(x$Max_Rate_Slurry..bpm. + x$Min_Rate_Slurry..bpm.)
plot(Med_rate_Slurry, x$Avg_Rate_Slurry..bpm., main="Plage de ratio de pompage de fluide")

#3
Med_TreatingPressure = 0.5*(x$Max_Treating_pressure..KPa. + x$Min_Treating_Pressure..KPa.)
plot(Med_rate_Slurry, x$Avg_Treating_Pressure..KPa., main="Plage de pression d'injection")

#4
pairs(cbind(Shot_Density=x$Shot_Density..shots.ft., Shot_Total = x$Shot_Total, Shot_Dist=dist[,1]),main="Transformation des pred Shot")


#approx oil et gas
?approx
g<-gas+0.01-min(gas)#positive le gas
o<-oil+0.01-min(oil)#positive le gas
plot(sort(exp(-g)), sort(exp(-o)))
plot(exp(-oil), exp(-gas))
