data <- read.csv2('TrainSample.csv')
nums <- sapply(data, is.numeric) 

data <- data[ , nums] # variables numériques uniquement
data$API <- NULL       # on enlève l'identifiant
###############
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

# data test et training data 

ind.test <- sample(1:460, size=100, replace = FALSE)
data.test <- data[ind.test,]
data.train <- data[-ind.test,]

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

fviz_pca_biplot(model.pca)

cumul_EIG <- model.pca$eig$`cumulative percentage of variance`
barplot(cumul_EIG,main = "pourcentage cumule ",xlab = "composante",ylab = "pourcentage explique")
abline(h=85,col="red")
barplot(model.pca$eig[,1],main="Eigenvalues",names.arg=1:nrow(model.pca$eig))
contribution_var<-model.pca$var$coord
plot(model.pca$ind$coord[,1],model.pca$ind$coord[,2])
plot3d(model.pca$ind$coord[,1],model.pca$ind$coord[,2],model.pca$ind$coord[,3])
plot3d(model.pca$ind$coord[,1],model.pca$ind$coord[,3],model.pca$ind$coord[,4])

# projection des individus sur le nouvel espace
new_individus <- model.pca$ind$coord

#pca.predict <- predict(model.pca, newdata= data.test)