trteffectPLOT_gg_disc <-
function(x, ci, ci.bounds, get.F, fixed.values, conf.bands,  rho, xlab, ylab, xlim, ylim, main, markerTWO=FALSE, lty = 1, mar, p=NULL,  ...){ 
  
  trt.effect <- x$derived.data$trt.effect
  marker <- x$derived.data$marker
  event <- x$derived.data$event
  trt <- x$derived.data$trt
  n = length(trt.effect)
  
  F.D <- get.F(trt.effect, event, trt, rho = rho)*100
  
  mydata = data.frame(trt.effect, F.D,  marker )
  mydata = mydata[with(mydata, order(F.D)),]

  mydata <- unique(mydata)
  mydata$lower <- rep(NA, nrow(mydata))
  mydata$upper <- rep(NA, nrow(mydata))

 if(!markerTWO){
   cen <- mean(c(min(trt.effect, na.rm=TRUE), max(trt.effect, na.rm=TRUE)))
   ran <- max(trt.effect, na.rm=TRUE) - min(trt.effect, na.rm=TRUE)
   
   if(substr(ci, 1,1) =="v"){ cen <- mean(c(min(c(trt.effect, ci.bounds), na.rm=TRUE), max(c(trt.effect, ci.bounds), na.rm=TRUE))); ran <- max(c(trt.effect, ci.bounds), na.rm=TRUE) - min(c(trt.effect, ci.bounds), na.rm=TRUE)}
   
   
   ran <- ran*1.1
   mylim <- c(cen-ran/2, cen+ran/2)
   
   if(is.null(xlab)) xlab <- "% population below treatment effect"
   if(is.null(ylab)) ylab <- "treatment effect"
   if(is.null(xlim)) xlim <- c(0,100)
   if(is.null(ylim)) ylim <- mylim
   if(is.null(main)) main <- "Treatment effect distribution"
   p <- ggplot(mydata)     
   if(substr(ci, 1,1) =="h") p <- p + coord_flip()
  }

  if(!is.null(ci.bounds)){
    mval = sort(unique(marker))
    #order matters here!
    mydata[mydata$marker==mval[1], 4:5] <- ci.bounds[,1]
    mydata[ mydata$marker==mval[2], 4:5] <- ci.bounds[,2]

    
 
  if(substr(ci, 1,1)=="v"){
    fixed.values = fixed.values*100
    index.fix  <- (fixed.values<= max(F.D) & fixed.values >= min(F.D)) 
  }else{
    ci.bounds = ci.bounds*100
    index.fix  <- (fixed.values<= max(trt.effect) & fixed.values >= min(trt.effect)) 
  }
    
    if(substr(ci, 1,1)=="h"){
      mydata[,4:5] <- mydata[,4:5]*100
      p <- ggplot(mydata, aes(y = F.D, x =trt.effect, ymin = lower, ymax = upper ))
      p <- p + geom_pointrange(size = 1)
      p <- p + coord_flip()
    }else{
      p <- ggplot(mydata, aes(x = F.D, y =trt.effect, ymin = lower, ymax = upper ))
      p <- p + geom_pointrange(size = 1)
      p <- p 
    }
}
  
  

  if(substr(ci, 1, 1)=="h"){
    #we used coord_flip, so we switch x and y, otherwise dont

        #add x/y labels and main
    
    p <- p + ylab(xlab) + xlab(ylab) + xlim(ylim[1], ylim[2]) + ggtitle(main) 
    
    #add vlines 
    #tmpdat <- data.frame( value = c(0, mean(event[trt==0])-mean(event[trt==1])), line = c(4, 3))
    p <- p + stat_vline(xintercept  = mean(trt.effect), aes(linetype = factor(3)))+
             stat_vline(xintercept = 0, aes( linetype = factor(4))) + scale_linetype_manual(name = "Treatment Effect", breaks = c("3", "4"), values = c(3, 4), labels = c("Mean", "Zero"))
    
    #p + scale_linetype_discrete(breaks = c("3", "4"), labels = c("a", "b"))
    #add the legend, increase text size
    p <- p + theme( text = element_text(size=18))

    p <- p + scale_y_continuous(limits = xlim) 

  }else{


    #add x/y labels and main
    p <- p + xlab(xlab) + ylab(ylab) + ylim(ylim[1], ylim[2])+ ggtitle(main) 

    #change the names for the legend
    p <- p + stat_hline(yintercept  = mean(trt.effect), aes(linetype = factor(3)))+
      stat_hline(yintercept = 0, aes( linetype = factor(4))) + scale_linetype_manual(name = "Treatment Effect",breaks = c("3", "4"), values = c(3, 4), labels = c("Mean", "Zero"))
    
    p <- p + theme( text = element_text(size=18)) #, 

    
    p <- p + scale_x_continuous(limits = xlim)
    
  }
  

  print(p) 
    
  return(list(p, mydata))
}
