##FUNCIÓN PARA GRAFICAR EL glzcluster ----
draw.octopucs <- function(M2, cex=1, lab.pos=0.09, n=NULL, distance=NULL,
method=NULL, prop=NULL, Name=NULL, ylim=NULL, xlim=NULL, verbose=NULL,
orientation=NULL, support=NULL, sep.lab=NULL, disp = NULL){
  userpars <-par(no.readonly = TRUE) #collect users parameters
  on.exit(par(userpars)) #reset users parameters on exit of the function
  
  if(class(M2)[1]=="octopucs") {
    support <- M2$support
    M2 <- M2$gcluster
 
  } else {
    support <- support
    M2 <- M2[[length(M2)]]
    }
  n <- ifelse(is.null(n), M2$n, n)
  distance <- ifelse(is.null(distance), M2$dist.method, distance)
  method <- ifelse(is.null(method), M2$method, method)
  prop <- ifelse(is.null(prop), M2$prop, prop)
  Name <- ifelse(is.null(Name), M2$name, Name)
  verbose <- ifelse(is.null(verbose), TRUE, verbose)
  orientation <- ifelse(is.null(orientation), "r", orientation)
  sep.lab <- ifelse(is.null(sep.lab), 0.15, sep.lab)
  
  if(orientation =="d"){
  if(is.null(ylim)){
    MAX <- max(M2$height)
    if(MAX<=1) {
      MAX <- max(M2$height)
      MIN <- min(M2$height)
      ylim=c(floor(MIN*10)/10, ceiling(MAX*10)/10)
      axeslabel= "Dissimilarity"} else {
        M2$height <- M2$height/max(M2$height)
        MIN<- min(M2$height)
        MAX <- max(M2$height)
        ylim=c(floor(MIN*10)/10, ceiling(MAX*10)/10)
        axeslabel= "Dissimilarity (rescaled)"
      }
  }  else {
    if(max(M2$height) <=1) {
      axeslabel= "Dissimilarity"} else {
        M2$height <- M2$height/max(M2$height)
        axeslabel= "Dissimilarity (rescaled)"
      }
  }

  if(verbose==TRUE) {
    MAR <- c(0.5,4,3,1)
    layout(matrix(c(1,2),2, 1), heights =c(0.9, 0.1))
    par(mar=MAR)
    
    plot(c(0, length(M2$order)), c(ylim[1]-0.08, ylim[2]), las=1, type ="n", axes=FALSE, ylab =axeslabel, xlab = "", main = "Name")} else {
      MAR <- c(0.5,4,1,1)
      par(mfrow=c(1,1), mar=MAR)
      
      plot(c(0, length(M2$order)), c(ylim[1]-0.08, ylim[2]), las=1, type ="n", axes=FALSE, ylab =axeslabel, xlab = "", main = "")}

  SEQ<-seq(round(ylim[1]*10,0)/10, round(ylim[2]*10,0)/10, 0.1)
  SEQ<-SEQ[SEQ<=1]

  axis(2, labels = SEQ, at=SEQ, las=1)
  DISTX <- numeric()
  POS2 <- numeric()
  for(i in 1:nrow( M2$merge)){
    if(M2$merge[i,1]<0 &  M2$merge[i,2]< 0){
      POS <- match(abs(M2$merge[i,]), M2$order)
      POS2[i] <- mean(POS)
      DISTX[i] <- mean(POS)
      lines(c(POS[1], POS[2]), c(M2$height[i], M2$height[i]))
      lines(c(POS[1], POS[1]), c(M2$height[i], M2$height[i]-0.05))
      lines(c(POS[2], POS[2]), c(M2$height[i], M2$height[i]-0.05))

      text(c(POS[1], POS[2]), c(M2$height[i]-lab.pos, M2$height[i]-lab.pos), M2$labels[M2$order[POS]], srt =90, cex=cex)
    }

    if(M2$merge[i,1] < 0 &  M2$merge[i,2]> 0){
      POS <- match(abs(M2$merge[i,1]), M2$order)
      DISTX[i] <- mean(c(POS, DISTX[M2$merge[i,2]]))
      lines(c(POS, POS), c(M2$height[i],M2$height[i]-0.05))
      lines(c(POS, DISTX[M2$merge[i,2]]), c(M2$height[i],M2$height[i]))
      lines(c(DISTX[M2$merge[i,2]], DISTX[M2$merge[i,2]]), c(M2$height[i], M2$height[M2$merge[i,2]]))

      text(POS, M2$height[i]-lab.pos, M2$labels[M2$order[POS]], srt =90,
           cex=cex)
    }


    if(M2$merge[i,1] > 0 &  M2$merge[i,2]> 0){
      POS <- c(M2$merge[i,1], M2$merge[i,2])
      DISTX[i] <- mean(c(DISTX[M2$merge[i,1]], DISTX[M2$merge[i,2]]))
      lines(c(DISTX[M2$merge[i,1]], DISTX[M2$merge[i,1]]), 
            c(M2$height[M2$merge[i,1]], M2$height[i]))
      lines(c(DISTX[M2$merge[i,2]], DISTX[M2$merge[i,2]]), 
            c(M2$height[M2$merge[i,2]], M2$height[i]))
      lines(c(DISTX[M2$merge[i,2]], DISTX[M2$merge[i,1]]), c(M2$height[i], M2$height[i]))
    }
  }

  if(verbose==TRUE){
    mtext("Support", side=3, line = 2, col = "blue", at=0, adj=0)
    mtext("Integrity / Contamination", side=3, line = 1, col = "red", 
          at=0, adj=0)

    mtext(paste("N-bootstrap =", M2$n), side=3, line = 2, col = "grey", 
          at = length(M2$order), adj=1)
    mtext(paste("Distance =", M2$dist.method), side=3, line = 1, 
          col = "grey", at = length(M2$order), adj=1)
    mtext(paste("Aglom. Meth =", M2$method), side=3, line = 0, 
          col = "grey", at = length(M2$order), adj=1)
    mtext(paste("Ratio changed =", 1-M2$prop), side=3, line = -1, 
          col = "grey", at = length(M2$order), adj=1)
  }
  }
  
  
  
  if(orientation =="r"){ 
    if(is.null(xlim)){
    MAX <- max(M2$height)
    if(MAX<=1) {
      MIN <- 1-max(M2$height)
      MAX <- 1-min(M2$height)
      xlim=c(floor(MIN*10)/10, ceiling(MAX*10)/10)
      axeslabel= "Similarity"} else {
        M2$height <- M2$height/max(M2$height)
        MIN<- 1-max(M2$height)
        MAX <- 1-min(M2$height)
        xlim=c(floor(MIN*10)/10, ceiling(MAX*10)/10)
        axeslabel= "Similarity (rescaled)"
      }
  } else {
    if(max(M2$height) <=1) {
      axeslabel= "Similarity"} else {
        M2$height <- M2$height/max(M2$height)
        axeslabel= "Similarity (rescaled)"
      }
  }

    if(verbose==TRUE){
      layout(matrix(c(1,2),1, 2), widths = c(0.9, 0.1))
      MAR <- c(4,1,3,0.5)
      par(mar=MAR) #Modify graphical layout
      
      plot(c(xlim[1],xlim[2]+0.08), c(0, length(M2$order)), las=1, type ="n", axes=FALSE, xlab =axeslabel, ylab = "", main = Name)} else {
        MAR <- c(4,1,1,0.5)
        par(mfrow=c(1,1), mar=MAR)
        
        plot(c(xlim[1],xlim[2]+0.08), c(0, length(M2$order)), las=1, 
             type ="n", axes=FALSE, xlab =axeslabel, ylab = "", main = "")
        }

    SEQ <- seq(round(xlim[1]*10,0)/10, round(xlim[2]*10,0)/10, 0.1)
    SEQ<-SEQ[SEQ<=1]
    M2$height <- 1 - M2$height
    axis(1, labels = SEQ, at=SEQ, las=1)
    DISTX <- numeric()
    POS2 <- numeric()
    for(i in 1:nrow( M2$merge)){
      if(M2$merge[i,1] < 0 &  M2$merge[i,2] < 0){
        POS <- match(abs(M2$merge[i,]), M2$order)
        POS2[i] <- mean(POS)
        DISTX[i] <- mean(POS)
        lines(c(M2$height[i], M2$height[i]), c(POS[1], POS[2]))
        lines(c(M2$height[i], M2$height[i]+0.05), c(POS[1], POS[1]))
        lines( c(M2$height[i], M2$height[i]+0.05), c(POS[2], POS[2]))

        text(c(M2$height[i]+lab.pos, M2$height[i]+lab.pos), c(POS[1], POS[2]), M2$labels[M2$order[POS]], srt =0, cex=cex)
      }

      if(M2$merge[i,1] < 0 &  M2$merge[i,2]> 0){
        POS <- match(abs(M2$merge[i,1]), M2$order)
        DISTX[i] <- mean(c(POS, DISTX[M2$merge[i,2]]))
        lines(c(M2$height[i],M2$height[i]+0.05), c(POS, POS))
        lines(c(M2$height[i],M2$height[i]), c(POS, DISTX[M2$merge[i,2]]))
        lines(c(M2$height[i], M2$height[M2$merge[i,2]]), c(DISTX[M2$merge[i,2]], DISTX[M2$merge[i,2]]))

        text(M2$height[i]+lab.pos, POS, M2$labels[M2$order[POS]], srt =0, 
             cex=cex)
      }


      if(M2$merge[i,1] > 0 &  M2$merge[i,2]> 0){
        POS <- c(M2$merge[i,1], M2$merge[i,2])
        DISTX[i] <- mean(c(DISTX[M2$merge[i,1]], DISTX[M2$merge[i,2]]))
        lines(c(M2$height[M2$merge[i,1]], M2$height[i]), c(DISTX[M2$merge[i,1]], DISTX[M2$merge[i,1]]))
        lines(c(M2$height[M2$merge[i,2]], M2$height[i]), c(DISTX[M2$merge[i,2]], DISTX[M2$merge[i,2]]))
        lines(c(M2$height[i], M2$height[i]), c(DISTX[M2$merge[i,2]], 
                                               DISTX[M2$merge[i,1]]))
      }
    }

    if(verbose==TRUE){
      mtext("Support", side=3, line = 2, col = "blue", at=xlim[1], adj=0)
      mtext("Integrity / Contamination", side=3, line = 1, col = "red", 
            at=xlim[1], adj=0)

      mtext(paste("N-bootstrap =", M2$n), side=3, line = 2, col = "grey", 
            at = xlim[2], adj=1)
      mtext(paste("Distance =", M2$dist.method), side=3, line = 1, 
            col = "grey", at = xlim[2], adj=1)
      mtext(paste("Aglom. Meth =", M2$method), side=3, line = 0, 
            col = "grey", at = xlim[2], adj=1)
      mtext(paste("Ratio changed =", 1-M2$prop), side=3, line = -1, 
            col = "grey", at = xlim[2], adj=1)
    }
    }
  
  
  if(orientation =="l"){
    if(is.null(xlim)){
    MAX <- max(M2$height)
    if(MAX<=1) {
      MAX <- max(M2$height)
      MIN <- min(M2$height)
      xlim=c(floor(MIN*10)/10, ceiling(MAX*10)/10)
      axeslabel= "Dissimilarity"} else {
        M2$height <- M2$height/max(M2$height)
        MIN<- min(M2$height)
        MAX <- max(M2$height)
        xlim=c(floor(MIN*10)/10, ceiling(MAX*10)/10)
        axeslabel= "Dissimilarity (rescaled)"
      }
  } else {
    if(max(M2$height) <=1) {
      axeslabel= "Dissimilarity"} else {
        M2$height <- M2$height/max(M2$height)
        axeslabel= "Dissimilarity (scaled)"
      }
  }

    if(verbose==TRUE){
      MAR <- c(4,0.5,3,1)
      layout(matrix(c(2,1),1, 2), widths = c(0.1, 0.9))
      par(mar=MAR)
      
      plot(c(xlim[1]-0.08, xlim[2]), c(0, length(M2$order)), las=1, 
           type ="n", axes=FALSE, xlab =axeslabel, ylab = "", 
           main = Name)} else {
             MAR <- c(4,0.5,1,1)
        par(mfrow=c(1,1), mar=MAR)
        
        plot(c(xlim[1]-0.08, xlim[2]), c(0, length(M2$order)), las=1, 
             type ="n", axes=FALSE, xlab =axeslabel, ylab = "", main = "")}

    SEQ<-seq(round(xlim[1]*10,0)/10, round(xlim[2]*10,0)/10, 0.1)
    SEQ<-SEQ[SEQ<=1]

    axis(1, labels = SEQ, at=SEQ, las=1)
    DISTX <- numeric()
    POS2 <- numeric()
    for(i in 1:nrow( M2$merge)){
      if(M2$merge[i,1]<0 &  M2$merge[i,2]< 0){
        POS <- match(abs(M2$merge[i,]), M2$order)
        POS2[i] <- mean(POS)
        DISTX[i] <- mean(POS)
        lines(c(M2$height[i], M2$height[i]), c(POS[1], POS[2]))
        lines(c(M2$height[i], M2$height[i]-0.05), c(POS[1], POS[1]))
        lines( c(M2$height[i], M2$height[i]-0.05), c(POS[2], POS[2]))

        text(c(M2$height[i]-lab.pos, M2$height[i]-lab.pos), c(POS[1], POS[2]), M2$labels[M2$order[POS]], srt =0, cex=cex)
      }

      if(M2$merge[i,1] < 0 &  M2$merge[i,2]> 0){
        POS <- match(abs(M2$merge[i,1]), M2$order)
        DISTX[i] <- mean(c(POS, DISTX[M2$merge[i,2]]))
        lines(c(M2$height[i],M2$height[i]-0.05), c(POS, POS))
        lines(c(M2$height[i],M2$height[i]), c(POS, DISTX[M2$merge[i,2]]))
        lines(c(M2$height[i], M2$height[M2$merge[i,2]]), c(DISTX[M2$merge[i,2]], DISTX[M2$merge[i,2]]))

        text(M2$height[i]-lab.pos, POS, M2$labels[M2$order[POS]], 
             srt =0, cex=cex)
      }


      if(M2$merge[i,1] > 0 &  M2$merge[i,2]> 0){
        POS <- c(M2$merge[i,1], M2$merge[i,2])
        DISTX[i] <- mean(c(DISTX[M2$merge[i,1]], DISTX[M2$merge[i,2]]))
        lines(c(M2$height[M2$merge[i,1]], M2$height[i]), c(DISTX[M2$merge[i,1]], DISTX[M2$merge[i,1]]))
        lines(c(M2$height[M2$merge[i,2]], M2$height[i]), c(DISTX[M2$merge[i,2]], DISTX[M2$merge[i,2]]))
        lines(c(M2$height[i], M2$height[i]), c(DISTX[M2$merge[i,2]], 
                                               DISTX[M2$merge[i,1]]))
      }
    }

    if(verbose==TRUE){
      mtext("Support", side=3, line = 2, col = "blue", at=xlim[1], adj=0)
      mtext("Integrity / Contamination", side=3, line = 1, col = "red", 
            at=xlim[1], adj=0)

      mtext(paste("N-bootstrap =", M2$n), side=3, line = 2, col = "grey", 
            at = xlim[2], adj=1)
      mtext(paste("Distance =", M2$dist.method), side=3, line = 1, 
            col = "grey", at = xlim[2], adj=1)
      mtext(paste("Aglom. Meth =", M2$method), side=3, line = 0, 
            col = "grey", at = xlim[2], adj=1)
      mtext(paste("Ratio changed =", 1-M2$prop), side=3, line = -1, 
            col = "grey", at = xlim[2], adj=1)
    }
  }
  
  
  
  
  
  
  #populate.octopucs <- function(M2=M2, support=NULL, sep.lab=NULL, disp = NULL, verbose=NULL, orientation=NULL)
    if(verbose==TRUE){
    sep.lab <- ifelse(is.null(sep.lab), 0.15, sep.lab)
    disp <- ifelse(is.null(disp), "sig", disp)
    
  
#    if(class(M2)[1]=="octopucs" & is.null(support)) {
#     support<-M2$support
#     } else M2 <- M2[[length(M2)]]
    
    if(orientation=="d"){
      if(sum(support[,3] >= 0.95)>1){
        if(max(M2$height)>1) M2$height<- M2$height/max(M2$height)
        DISTX <- numeric()
        RECTANGLES <- matrix(NA, nrow(M2$merge), 2)
        for(i in 1:nrow( M2$merge)){
          if(M2$merge[i,1]<0 &  M2$merge[i,2]< 0){
            DISTX[i] <- mean(match(abs(M2$merge[i,]), M2$order))
            RECTANGLES[i, ] <- match(abs(M2$merge[i,]), M2$order)
          }
          if(M2$merge[i,1] < 0 &  M2$merge[i,2]> 0){
            DISTX[i] <- mean(c(match(abs(M2$merge[i,1]), M2$order), DISTX[M2$merge[i,2]]))
            RECTANGLES[i, ] <- c(match(abs(M2$merge[i,1]), M2$order), max(RECTANGLES[M2$merge[i,2], ]))
          }
          if(M2$merge[i,1] > 0 &  M2$merge[i,2]> 0){
            DISTX[i] <- mean(c(DISTX[M2$merge[i,1]], DISTX[M2$merge[i,2]]))
            RECTANGLES[i, ] <- c(min(c(RECTANGLES[M2$merge[i,1],], RECTANGLES[M2$merge[i,2],])), max(c(RECTANGLES[M2$merge[i,1],], RECTANGLES[M2$merge[i,2],])))
          }
        }
        
        if(disp == "sig") {ESTOS <- which(support[,3] >= 0.95); ESTOS<- ESTOS[-length(ESTOS)]} else ESTOS <- which(support[,3] <= 1)
        if(verbose==TRUE) points(DISTX[ESTOS], M2$height[ESTOS], pch=15, col="#64646432", cex=4)
        COL2 <- rep("red", length(M2$height[ESTOS]))
        COL2[which(support[ESTOS,3] < 0.95)] <- "black"
        if(verbose==TRUE) text(DISTX[ESTOS], M2$height[ESTOS]-sep.lab/100, paste(support[ESTOS,1], "/",support[ESTOS,2]), cex=0.6, col = COL2)
        COL2 <- rep("blue", length(M2$height[ESTOS]))
        COL2[which(support[ESTOS,3] < 0.95)] <- "black"
        if(verbose==TRUE) text(DISTX[ESTOS], M2$height[ESTOS]+sep.lab/100, support[ESTOS,3], cex=0.7, col = COL2) else {ESTOS <- which(support[,3] >= 0.95); ESTOS<- ESTOS[-length(ESTOS)];text(DISTX[ESTOS], M2$height[ESTOS]+sep.lab/100, "*", cex=2, col = COL2)}
        
        LEVEL <- numeric()
        ESTOS <- which(support[,3] >= 0.95)
        ESTOS<- ESTOS[-length(ESTOS)]
        RECTANGLES <- matrix(RECTANGLES[ESTOS,],,2)
        #RECTANGLES <- RECTANGLES[-nrow(RECTANGLES), ]
        if(verbose==TRUE) {
          if(sum(par()$mfrow)==3){
            MAR <-par()$mar
            MAR[c(1,3)] <- 0
            par(mar=MAR)
            plot(c(0, length(M2$order)), c(0, 1), las=1, type ="n", axes=FALSE, ylab ="", xlab = "")
            for(i in nrow(RECTANGLES):1){
              LEVEL[i] <- sum(rowSums(cbind(RECTANGLES[i, 1] >= RECTANGLES[-i, 1], RECTANGLES[i, 2] <= RECTANGLES[-i, 2]))==2)
              polygon(c(RECTANGLES[i,1], RECTANGLES[i, 2], RECTANGLES[i,2], RECTANGLES[i,1]), c(0, 0, 1-(0.1*LEVEL[i]), 1-(0.1*LEVEL[i])), col = paste("#104E8B",50, sep=""), border=NULL)
            }
          }
        }
      }
    }
    
    
    if(orientation=="r"){  
      if(sum(support[,3] >= 0.95)>1){
      if(max(M2$height)>1) M2$height<- M2$height/max(M2$height)
      #M2$height <- 1 - M2$height
      DISTX <- numeric()
      RECTANGLES <- matrix(NA, nrow(M2$merge), 2)
      for(i in 1:nrow( M2$merge)){
        if(M2$merge[i,1]<0 &  M2$merge[i,2]< 0){
          DISTX[i] <- mean(match(abs(M2$merge[i,]), M2$order))
          RECTANGLES[i, ] <- match(abs(M2$merge[i,]), M2$order)
        }
        if(M2$merge[i,1] < 0 &  M2$merge[i,2]> 0){
          DISTX[i] <- mean(c(match(abs(M2$merge[i,1]), M2$order), DISTX[M2$merge[i,2]]))
          RECTANGLES[i, ] <- c(match(abs(M2$merge[i,1]), M2$order), max(RECTANGLES[M2$merge[i,2], ]))
        }
        if(M2$merge[i,1] > 0 &  M2$merge[i,2]> 0){
          DISTX[i] <- mean(c(DISTX[M2$merge[i,1]], DISTX[M2$merge[i,2]]))
          RECTANGLES[i, ] <- c(min(c(RECTANGLES[M2$merge[i,1],], RECTANGLES[M2$merge[i,2],])), max(c(RECTANGLES[M2$merge[i,1],], RECTANGLES[M2$merge[i,2],])))
        }
      }
      
      if(disp == "sig") {
        ESTOS <- which(support[,3] >= 0.95); ESTOS<- ESTOS[-length(ESTOS)] } else ESTOS <- which(support[,3] <= 1)
      if(verbose==TRUE) points(M2$height[ESTOS], DISTX[ESTOS], pch=15, col="#64646432", cex=4)
      COL2 <- rep("red", length(M2$height[ESTOS]))
      COL2[which(support[ESTOS,3] < 0.95)] <- "black"
      if(verbose==TRUE) text(M2$height[ESTOS], DISTX[ESTOS]-sep.lab, paste(support[ESTOS,1], "/",support[ESTOS,2]), cex=0.6, col = COL2)
      COL2 <- rep("blue", length(M2$height[ESTOS]))
      COL2[which(support[ESTOS,3] < 0.95)] <- "black"
      if(verbose==TRUE) text(M2$height[ESTOS], DISTX[ESTOS]+sep.lab, support[ESTOS,3], cex=0.7, col = COL2) else {ESTOS <- which(support[,3] >= 0.95); ESTOS<- ESTOS[-length(ESTOS)];text(M2$height[ESTOS], DISTX[ESTOS]+sep.lab, "*", cex=2, col = COL2)}
      
      LEVEL <- numeric()
      ESTOS <- which(support[,3] >= 0.95)
      ESTOS<- ESTOS[-length(ESTOS)]
      RECTANGLES <- matrix(RECTANGLES[ESTOS,],,2)
      #RECTANGLES <- RECTANGLES[-nrow(RECTANGLES), ]
      if(verbose==TRUE) {
        if(sum(par()$mfrow)==3){
          MAR <- par()$mar
          MAR[c(2,4)] <- 0
          par(mar=MAR)
          plot(c(0, 1), c(0, length(M2$order)), las=1, type ="n", axes=FALSE, ylab ="", xlab = "")
          for(i in nrow(RECTANGLES):1){
            LEVEL[i] <- sum(rowSums(cbind(RECTANGLES[i, 1] >= RECTANGLES[-i, 1], RECTANGLES[i, 2] <= RECTANGLES[-i, 2]))==2)
            polygon(c(0, 0, 1-(0.1*LEVEL[i]), 1-(0.1*LEVEL[i])), c(RECTANGLES[i,1], RECTANGLES[i, 2], RECTANGLES[i,2], RECTANGLES[i,1]), col = paste("#104E8B",90-5*LEVEL[i], sep=""), border=NULL)
        }
      }
    }
  }
}
    

    if(orientation=="l"){if(sum(support[,3] >= 0.95)>1){
      if(max(M2$height)>1) M2$height<- M2$height/max(M2$height)
      DISTX <- numeric()
      RECTANGLES <- matrix(NA, nrow(M2$merge), 2)
      for(i in 1:nrow(M2$merge)){
        if(M2$merge[i,1]< 0 &  M2$merge[i,2]< 0){
          DISTX[i] <- mean(match(abs(M2$merge[i,]), M2$order))
          RECTANGLES[i, ] <- match(abs(M2$merge[i,]), M2$order)
        }
        if(M2$merge[i,1] < 0 &  M2$merge[i,2]> 0){
          DISTX[i] <- mean(c(match(abs(M2$merge[i,1]), M2$order), DISTX[M2$merge[i,2]]))
          RECTANGLES[i, ] <- c(match(abs(M2$merge[i,1]), M2$order), max(RECTANGLES[M2$merge[i,2], ]))
        }
        if(M2$merge[i,1] > 0 &  M2$merge[i,2]> 0){
          DISTX[i] <- mean(c(DISTX[M2$merge[i,1]], DISTX[M2$merge[i,2]]))
          RECTANGLES[i, ] <- c(min(c(RECTANGLES[M2$merge[i,1],], RECTANGLES[M2$merge[i,2],])), max(c(RECTANGLES[M2$merge[i,1],], RECTANGLES[M2$merge[i,2],])))
        }
      }
      
      if(disp == "sig") {ESTOS <- which(support[,3] >= 0.95); ESTOS<- ESTOS[-length(ESTOS)]} else ESTOS <- which(support[,3] <= 1)
      if(verbose==TRUE) points(M2$height[ESTOS], DISTX[ESTOS], pch=15, col="#64646432", cex=4)
      COL2 <- rep("red", length(M2$height[ESTOS]))
      COL2[which(support[ESTOS,3] < 0.95)] <- "black"
      if(verbose==TRUE) text(M2$height[ESTOS], DISTX[ESTOS]-0.5, paste(support[ESTOS,1], "/",support[ESTOS,2]), cex=0.6, col = COL2)
      COL2 <- rep("blue", length(M2$height[ESTOS]))
      COL2[which(support[ESTOS,3] < 0.95)] <- "black"
      if(verbose==TRUE) text(M2$height[ESTOS], DISTX[ESTOS]+0.5, support[ESTOS,3], cex=0.7, col = COL2) else {ESTOS <- which(support[,3] >= 0.95); ESTOS<- ESTOS[-length(ESTOS)];text(M2$height[ESTOS]+0.01, DISTX[ESTOS], "*", cex=2, col = COL2)}
      
      LEVEL <- numeric()
      ESTOS <- which(support[,3] >= 0.95)
      ESTOS<- ESTOS[-length(ESTOS)]
      RECTANGLES <- matrix(RECTANGLES[ESTOS,],,2)
      #RECTANGLES <- RECTANGLES[-nrow(RECTANGLES), ]
      if(verbose==TRUE) {
        if(sum(par()$mfrow)==3){
          MAR <- par()$mar
          MAR[c(2,4)] <- 0
          par(mar=MAR)
          plot(c(0, 1), c(0, length(M2$order)), las=1, type ="n", axes=FALSE, ylab ="", xlab = "")
          for(i in nrow(RECTANGLES):1){
            LEVEL[i] <- sum(rowSums(cbind(RECTANGLES[i, 1] >= RECTANGLES[-i, 1], RECTANGLES[i, 2] <= RECTANGLES[-i, 2]))==2)
            polygon(c(0, 0, 1-(0.1*LEVEL[i]), 1-(0.1*LEVEL[i])), c(RECTANGLES[i,1], RECTANGLES[i, 2], RECTANGLES[i,2], RECTANGLES[i,1]), col = paste("#104E8B",90-5*LEVEL[i], sep=""), border=NULL)
            }
          }
        }
      }
    }
  }
}
