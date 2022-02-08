
exemple_monModel<-function(bootIndex){
  inp<-VAR
  out<-RESP
  
  #model sur les données bootstrapées
  model<-lm(out~x, data.frame(x = inp[bootIndex], out = out[bootIndex]))
  
  #prediction sur l'ensemble des données
  predi<-predict(model, newdata = data.frame(x = inp))$fit
  
  #on retourne les deux
  return(list(residus = model$residuals, pred=predi))
}

predInterval <- function(FUN, repLength,n_data, alpha=0.95, nboot=100, option ='parametric'){
  # par bootstrap sur les donnees
  n<-repLength
  Mboot <- matrix(NA, nrow = n, ncol = nboot)
  
  for (i in 1:nboot){
    index <- sample(n_data, replace = TRUE)
    f<-FUN(index)
    #residus = gas - pred_gas
    
    Mboot[, i] <- f$pred
    if (option=="parametric"){
      Mboot[, i] <- Mboot[, i] + rnorm(n, 0, sd(f$residus))
    } else if (option == "non parametric"){
      Mboot[, i] <- Mboot[, i] + sample(f$residus, size = n, replace = TRUE)
    } else stop()
  }
  #alpha=0.05
  pboot <- matrix(NA, nrow = n, ncol = 3)
  pboot[, 1] <- rowMeans(Mboot)
  pboot[, 2] <- apply(Mboot, 1, quantile, 1-(1-alpha/2)) 
  pboot[, 3] <- apply(Mboot, 1, quantile, (1-alpha/2))
  colnames(pboot)<-c("fit", "lwr", "upr")
  
  return(pboot)
}

#exemple
if(FALSE){
  #creation de VARiables et de leur RESPonse
  n <- 20
  sigma <- 0.05
  x <- seq(0, 1, length.out = n)
  X <- cbind(rep(1,n), x)
  beta <- matrix(c(1, 0.5), ncol = 1)
  set.seed(0)
  y <- X%*%beta + rnorm(n, mean = 0, sd = sigma)
  
  df <- data.frame(x = X, y = y)
  m <- lm(y~x, data = df) 
  
  nNew <- 20
  xnew = x;
  p <- predict(m, newdata = data.frame(x = xnew), interval = "pred", level = 0.95)
  
  VAR<-x
  RESP<-y
  
  x<-NULL
  y<-NULL

  #Go !
  p_estime<-predInterval(exemple_monModel, 20)#20 car nous avons 20 individus
  plot(p_estime[,1], p[,1])
  abline(0,1)
}