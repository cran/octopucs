###FUNCIÓN glzcluster PARA GENERAR EL DENDOGRAMA GENERALIZADO CAMBIANDO VALORES DE LA MATRIZ POR EL PROMEDIO DE LA COLUMNA ----
glzcluster2 <- function(data=data, n = NULL, prop=NULL, distance =NULL, method=NULL, Name=NULL){
  n <- ifelse(is.null(n),1000, n)
  prop <- ifelse(is.null(prop), 0.8, prop)
  distance <- ifelse(is.null(distance), "bray",distance)
  method <- ifelse(is.null(method), "average",method)
  Name <- ifelse(is.null(Name), deparse(substitute(data)),Name)

  met1 <- c("manhattan", "euclidean", "canberra", "bray", "kulczynski", "gower", "morisita", "horn", "mountford", "jaccard", "raup", "binomial", "chao", "altGower", "cao", "mahalanobis")
  met2 <- data.frame(No=1:24, beta=c("w", "-1", "c", "wb", "r", "I", "e", "t", "me", "j", "sor", "m", "-2", "co", "cc", "g", "-3", "1", "", "hk", "rlb", "sim", "gl", "z"))

  if((sum(data==1)+sum(data==0)) == ncol(data)*nrow(data)) incidence <- TRUE else incidence <- FALSE
  if(!is.na(match(distance, met1))) PROC <- distance else {
    if(!is.na(which(distance == met2, arr.ind = TRUE)[1] >0)) PROC <- which(distance == met2, arr.ind = TRUE)[1] }
  DISTM <- as.dist(matrix(0, nrow(data), nrow(data)))
  LIST_HCLUST  <- list()
  for(i in 1: n){
    datasample<-data
    FF <- cbind(sample(1:nrow(data), size=round((nrow(data)*ncol(data))*(1-prop)), replace = TRUE), sample(1:ncol(data), size=round((nrow(data)*ncol(data))*(1-prop)), replace = TRUE))
    FF2<- table(paste(FF[,1], FF[,2]))
    estos <- names(which(FF2 >1))
    if(length(estos) >0) {
      for(ww in 1:length(estos)){
        estosI <- as.numeric(str_split(estos[ww], " ")[[1]])
        estosL <- which(match(FF[,1], estosI[1]) & match(FF[,2], estosI[2]))
        FF <- FF[-estosL[2:length(estosL)], ]
      }}
    if(incidence == FALSE){
    for(r in 1: nrow(FF)) {datasample[FF[r, 1], FF[r, 2]] <- mean(data[-FF[r,1], FF[r, 2]]) }} else {for(r in 1: nrow(FF)) {datasample[FF[r, 1], FF[r, 2]] <- round(mean(data[-FF[r,1], FF[r, 2]]))}}
    ESTASCERO <-which(rowSums(datasample)==0)
    ESTASCOLPORCERO <- sample(1:ncol(datasample), size=length(ESTASCERO))
    if(incidence == FALSE) datasample[ESTASCERO, ESTASCOLPORCERO] <- colMeans(datasample)[ESTASCOLPORCERO] else datasample[ESTASCERO, ESTASCOLPORCERO] <- 1

    if(is.character(PROC))  bc_S <- vegdist(datasample, method = PROC)  else  bc_S <- betadiver(datasample, PROC)
    LIST_HCLUST[[i]] <- hclust(bc_S, method=method)
    DISTM <- DISTM + cophenetic(LIST_HCLUST[[i]])
  }
  M2 <- hclust(as.dist(DISTM/i), method=method)
  LIST_HCLUST[[i+1]] <- M2
  LIST_HCLUST[[i+1]]$dist.method <- distance
  LIST_HCLUST[[i+1]]$n <- n
  LIST_HCLUST[[i+1]]$prop <- prop
  LIST_HCLUST[[i+1]]$name <- Name
  class(LIST_HCLUST) <- list("list", "octopucs")
  LIST_HCLUST
}
