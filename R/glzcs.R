##FUNCIÓN glzcs PARA ENCONTRAR EL SOPORTE Y PROBABILIDAD DE LOS GRUPOS----
glzcs<-function(GRPs=GRPs, M2=M2){
  #if(threshold < 0.60 | threshold > 0.90) {threshold <- 0.75
  #warning("You defined threshold outside the resonable limts (0.6 <= threshold <= 0.9), therefore octopucs forced it to 0.75.")}
  M2 <- M2[-length(M2)]
  MAT_COMPLE_SIM_F<- matrix(NA,length(M2), 3)
  SOPORTE <- matrix(NA, length(GRPs), 4)
  colnames(SOPORTE) <- c("Integrity", "Contamination", "Support", "Cluster size")
    KK0<- list()[1:length(GRPs)]
    MAT_COMPLE_SIM <- matrix(NA, length(GRPs), 3)
    TODOS <- lapply(M2, IDscluster2)
    pb <- progress_bar$new(total = length(GRPs), clear = FALSE, width= 60)
    for(k in 1:length(GRPs)){
      pb$tick();Sys.sleep(1/100)
      for(j in 1:length(TODOS)){
        MATCHES <- lapply(TODOS[[j]], match, GRPs[[k]])
        MATCHES <- lapply(MATCHES, function(x){!is.na(x)})
        MATCH <-  (2*unlist(lapply(MATCHES, sum)))/(length(GRPs[[k]]) + unlist(lapply(MATCHES, length)))
        MATCH <- which(abs(1-MATCH)==min(abs(1-MATCH)))[1]
        PAREADAS <- lapply(TODOS[[j]][MATCH], F1, GRPs[[k]])
        MAT_COMPLE_SIM_F[j,1:3] <- t(sapply(PAREADAS, compsim2))
      }
      SOPORTE[k, 1:2] <- colMeans(MAT_COMPLE_SIM_F, na.rm= TRUE)[1:2]
      SOPORTE[k, 4] <- length(GRPs[[k]])
    }

  SOPORTE[, 1] <- 1-SOPORTE[,1]
  #SOPORTE[, 3] <- (0.5*SOPORTE[,1]) / ( (0.5*SOPORTE[,1]) +  (0.5*SOPORTE[,2]))
  #P <- 1/unlist(lapply(GRPs, length))
  #SOPORTE[, 3] <- (P*SOPORTE[,1]) / ( (P*SOPORTE[,1]) +  ((1-P)*SOPORTE[,2]))
  SOPORTE[, 3] <- ((unlist(lapply(GRPs, length))/length(GRPs[[k]]))*SOPORTE[,1]) / ( ((unlist(lapply(GRPs, length))/length(GRPs[[k]]))*SOPORTE[,1]) +  ((1-(unlist(lapply(GRPs, length))/length(GRPs[[k]])))*SOPORTE[,2]))
  SOPORTE[, 1:3] <- round(SOPORTE[, 1:3], 3)
  SOPORTE
}
