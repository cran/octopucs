#Función global
octopucs <- function(data, n=NULL, prop=NULL, resample = TRUE, distance =NULL, 
method=NULL, orientation=NULL, disp = NULL, lab.pos=NULL, sep.lab=NULL, 
xlim=NULL, ylim=NULL, verbose=NULL){
  ESTASNONUM <- which(sapply(data, class) != "integer"  & sapply(data,class) != "numeric")
  if(length(ESTASNONUM)>0) stop("The dataset contains nonenumeric/noneinteger 
                                columns")
  n <- ifelse(is.null(n),1000, n)
  prop <- ifelse(is.null(prop), 0.8, prop)
  distance <- ifelse(is.null(distance), "bray",distance)
  method <- ifelse(is.null(method), "average",method)
  orientation <- ifelse(is.null(orientation), "r",orientation)
  disp <- ifelse(is.null(disp), "sig", disp)
  lab.pos <- ifelse(is.null(lab.pos), 0.09,lab.pos)
  sep.lab <- ifelse(is.null(sep.lab), 0.15,sep.lab)
  verbose <- ifelse(is.null(verbose), TRUE,verbose)
  Name <- deparse(substitute(data))

  data <- data[,which(colSums(data)>0)]
  if(orientation=="d" & !is.null(xlim)) stop("for vertical orinetation 
  specify y-axis  limits <<ylim>> not x-axis limits")

  if(resample== TRUE & ncol(data) > 17 ) M2 <- glzcluster(data=data, n = n, 
    prop = prop, distance =distance, method = method, Name =Name) else M2 <- glzcluster2(data=data, n = n, prop = prop, distance = distance, method = method, 
Name =Name)
  GRPs <- IDscluster(M2=M2)
  SUPPORT <- glzcs(GRPs, M2)
  M2[[length(M2)]]$name <- Name

  ENDRESULT <- list(gcluster= M2[[length(M2)]], support=SUPPORT, grps=GRPs)
  class(ENDRESULT) <- "octopucs"

    draw.octopucs(ENDRESULT, cex=1, lab.pos=lab.pos, n = n, distance=distance, 
                  method = method, prop = prop, Name=Name, ylim=ylim, 
                  xlim=NULL, verbose=verbose, orientation = orientation)
    
  ENDRESULT
}

