F1 <- function(A, B){
  IDJUNTOS <- table(c(B, A), rep(c("G1", "G2"), c(length(B), length(A))))
  IDJUNTOS
  }
