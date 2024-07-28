#FUNCIÓNES IDscluster PARA IDENTIFICAR LOS ELEMENTOS DE LOS GRUPOS----
IDscluster <- function(M2=M2) {
  if(is.list(M2)) M2<- M2[[length(M2)]]
  G1 <- list()
  for(i in 1:nrow(M2$merge)){
    if(M2$merge[i,1]< 0 &  M2$merge[i,2] < 0){
      G1[[i]] <- M2$merge[i,]
    }

    if(M2$merge[i,1] < 0 &  M2$merge[i,2] > 0){
      G1[[i]] <- c(M2$merge[i,1], G1[[M2$merge[i,2]]])
    }

    if(M2$merge[i,1] > 0 &  M2$merge[i,2]> 0){
      G1[[i]] <- c(G1[[M2$merge[i,1]]], G1[[M2$merge[i,2]]])
    }
  }
  G1
}
