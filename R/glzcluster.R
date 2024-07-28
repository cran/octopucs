###FUNCIÓN glzcluster PARA GENERAR EL DENDOGRAMA GENERALIZADO ELIMINANDO UN PROPORCIÓN DE COLUMNAS: UTIL CUANDO HAY MUCHAS COLUMNAS----
glzcluster <- function(data, n = NULL, prop=NULL, distance =NULL, method=NULL, Name=NULL){
  n <- ifelse(is.null(n),1000, n)
  prop <- ifelse(is.null(prop), 0.95, prop)
  distance <- ifelse(is.null(distance), "bray",distance)
  method <- ifelse(is.null(method), "average",method)
  Name <- ifelse(is.null(Name), deparse(substitute(data)),Name)

  met1 <- c("manhattan", "euclidean", "canberra", "bray", "kulczynski", "gower", "morisita", "horn", "mountford", "jaccard", "raup", "binomial", "chao", "altGower", "cao", "mahalanobis")

  met2 <- data.frame(No=1:24, beta=c("w", "-1", "c", "wb", "r", "I", "e", "t", "me", "j", "sor", "m", "-2", "co", "cc", "g", "-3", "1", "", "hk", "rlb", "sim", "gl", "z"))
  incidence <- ifelse((sum(data==1)+sum(data==0)) == ncol(data)*nrow(data), TRUE, FALSE)

  if(!is.na(match(distance, met1))) PROC <- distance else {
    if(!is.na(which(distance == met2, arr.ind = TRUE)[1] >0)) PROC <- which(distance == met2, arr.ind = TRUE)[1] }
  DISTM <- as.dist(matrix(0, nrow(data), nrow(data)))
  LIST_HCLUST  <- list()
  for(i in 1: n){
    datasample <- data[, sample(1:ncol(data), size = ceiling(ncol(data)*prop))]
    ESTASCERO <-which(rowSums(datasample)==0)
    ESTASCOLPORCERO <- sample(1:ncol(datasample), size=length(ESTASCERO))
    if(incidence == FALSE) datasample[ESTASCERO, ESTASCOLPORCERO] <- colMeans(datasample)[ESTASCOLPORCERO] else datasample[ESTASCERO, ESTASCOLPORCERO] <- 1
    if(is.character(PROC))  bc_S <- vegdist(datasample, method = PROC)  else  bc_S <- betadiver(datasample, PROC)
    LIST_HCLUST[[i]] <- hclust(bc_S, method=method)
    #DISTM <- DISTM + cophenetic(LIST_HCLUST[[i]])
    DISTM<- DISTM+ (bc_S)
  }
  #M2 <- hclust((DISTM/i), method=method)
  LIST_HCLUST[[i+1]] <- hclust((DISTM/i), method=method)
  LIST_HCLUST[[i+1]]$dist.method <- distance
  LIST_HCLUST[[i+1]]$n <- n
  LIST_HCLUST[[i+1]]$prop <- prop
  LIST_HCLUST[[i+1]]$name <- Name
  class(LIST_HCLUST) <- list("list", "octopucs")
  LIST_HCLUST
}

