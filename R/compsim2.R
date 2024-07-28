compsim2 <- function(M) {
  a <- sum(rowSums(M>0) == 2)
  b <- sum(rowSums(M) == M[,1]) - sum(rowSums(M) == 0)
  c <- sum(rowSums(M) == M[,2]) - sum(rowSums(M) == 0)
  Sb <- sum(M[,1]>0)
  Sc <- sum(M[,2]>0)
  A <- sum(ifelse(!is.infinite((M[,1] - M[,2])^2 / (M[,2] + M[,1])^2), (M[,1] - M[,2])^2 / (M[,2] + M[,1])^2, 0), na.rm = TRUE)
  Mb.a.Mc <- (A*Sb - c*Sb) / (Sb^2 +  c*Sb)
  Mc.a.Mb <- (A*Sc - b*Sc) / (Sc^2 +  b*Sc)
  DISIMIL <- ifelse(1 - (Mb.a.Mc + Mc.a.Mb)>=0, 1 - (Mb.a.Mc + Mc.a.Mb), 0)
  RES <- c("Comp 1 to 2"=Mb.a.Mc, "Comp 2 to 1"=Mc.a.Mb, Disimilitud=DISIMIL)
  RES
}