trteffectPLOTcompare_gg_disc <-
function(x1, x2, ci.bounds, conf.bands, offset,  xlab, ylab, xlim, ylim, main, marker.names, lty = 1,  annotate.plot = TRUE){ 
  
  
  trt.effect1 <- x1$derived.data$trt.effect
  marker1 <- x1$derived.data$marker
  event <- x1$derived.data$event
  trt <- x1$derived.data$trt
  n = length(trt.effect1)
  mval1 = sort(unique(marker1))
  
  
  trt.effect2 <- x2$derived.data$trt.effect
  mkrvals <- unique(c(marker1, x2$derived.data$marker))
  marker2 <- x2$derived.data$marker + offset
  mval2 = sort(unique(marker2))
  markerValue <- markerName <- trt.effect <- lower <- upper <- NULL

  mydata = data.frame(trt.effect = c(trt.effect1, trt.effect2),
                      "markerValue" = c(marker1, marker2), 
                      "markerName" = c(rep(marker.names, c(n,n))))
  
  mydata <- unique(mydata)
  mydata$lower <- rep(NA, nrow(mydata))
  mydata$upper <- rep(NA, nrow(mydata))

    if(is.null(xlab)) xlab <- "marker value"
    if(is.null(ylab)) ylab <- "treatment effect"
    if(is.null(xlim)) xlim <- c(mean(mkrvals) -1*diff(range(mkrvals)), mean(mkrvals) +1*diff(range(mkrvals)))
    if(is.null(main)) main <- "Treatment effect distribution"
    p <- ggplot(mydata)     
    

  if(!is.null(ci.bounds)){
    
    #order matters here!
    mydata[mydata$markerValue==mval1[1] & mydata$markerName == marker.names[1], 4:5] <- ci.bounds[,1]
    mydata[ mydata$markerValue==mval1[2]& mydata$markerName == marker.names[1], 4:5] <- ci.bounds[,2]
    mydata[mydata$markerValue==mval2[1] & mydata$markerName == marker.names[2], 4:5] <- ci.bounds[,3]
    mydata[ mydata$markerValue==mval2[2]& mydata$markerName == marker.names[2], 4:5] <- ci.bounds[,4]
    
    
    
    
    
    
    
    p <- ggplot(data = mydata, aes(x = markerValue, y =trt.effect, shape = factor(markerName), linetype = factor(markerName), ymin = lower, ymax = upper ))
    p <- p + geom_errorbar( width = .05, aes(size = markerName)) + geom_point(size = 4)
    p <- p + stat_hline(yintercept  = as.numeric(mean(event[trt==0]) - mean(event[trt==1])), aes(linetype = factor(3), size = factor(3), shape = factor(3)))+
      stat_hline(yintercept = 0, aes( linetype = factor(4), size = factor(4), shape = factor(4))) 
    
    
  }else{
    
    p <- ggplot(data = mydata, aes(x = markerValue, y =trt.effect, shape = factor(markerName)))
    p <- p +geom_point(size = 4)
    p <- p + stat_hline(yintercept  = as.numeric(mean(event[trt==0]) - mean(event[trt==1])), aes(linetype = factor(3), size = factor(3)))+
      stat_hline(yintercept = 0, aes( linetype = factor(4), size = factor(4))) 
    
    
  }
  
  
  
  
#add x/y labels and main
  p <- p + xlab(xlab) + ylab(ylab) + ggtitle(main) 
  if(!is.null(ylim)) p <- p + ylim(ylim[1], ylim[2])

  #change the names for the legend

  p <- p + theme( text = element_text(size=18)) #, 

  
    mkrprop = round(c( mean(mydata$markerVal[1]==marker1), 
                 mean(mydata$markerVal[2]==marker1), 
                 mean(mydata$markerVal[3]==marker2), 
                 mean(mydata$markerVal[4]==marker2))*100, 1)
    mkrprop = paste("   (", mkrprop, "%)", sep = "")
  if(annotate.plot){
  p <- p + annotate("text", x= mydata$markerVal+0.1, y = mydata$trt.effect, label= mkrprop)
  }

  p <- p + scale_x_continuous(breaks = mkrvals, limits = xlim)
  

  
  return(list(p, mydata))

}
