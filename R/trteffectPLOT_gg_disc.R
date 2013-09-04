trteffectPLOT_gg_disc <-
function(x, ci, ci.bounds, get.F,  xlab, ylab, xlim, ylim, main, markerTWO=FALSE, lty = 1,  p=NULL, trt.names = NULL){ 

  trt.effect <- x$derived.data$trt.effect
  marker <- x$derived.data$marker
  event <- x$derived.data$event
  trt <- x$derived.data$trt
  n = length(trt.effect)
  mval = sort(unique(marker))
  lower <- upper <- NULL ## appease check
  mydata = data.frame(trt.effect, marker )

  mydata <- unique(mydata)
  mydata$lower <- rep(NA, nrow(mydata))
  mydata$upper <- rep(NA, nrow(mydata))

 if(!markerTWO){
   cen <- mean(c(min(trt.effect, na.rm=TRUE), max(trt.effect, na.rm=TRUE)))
   ran <- max(trt.effect, na.rm=TRUE) - min(trt.effect, na.rm=TRUE)
   
   if(substr(ci, 1,1) =="v"){ cen <- mean(c(min(c(trt.effect, ci.bounds), na.rm=TRUE), max(c(trt.effect, ci.bounds), na.rm=TRUE))); ran <- max(c(trt.effect, ci.bounds), na.rm=TRUE) - min(c(trt.effect, ci.bounds), na.rm=TRUE)}
   
   
   ran <- ran*1.1
   mylim <- c(cen-ran/2, cen+ran/2)
   
   if(is.null(xlab)) xlab <- "marker value"
   if(is.null(ylab)) ylab <- "treatment effect"
   if(is.null(xlim)) xlim <- c(0,100)
   if(is.null(ylim)) ylim <- mylim
   if(is.null(main)) main <- "Treatment effect distribution"
   p <- ggplot(mydata)     

  }

  if(!is.null(ci.bounds)){

    #order matters here!
    mydata[mydata$marker==mval[1], 3:4] <- ci.bounds[,1]
    mydata[ mydata$marker==mval[2], 3:4] <- ci.bounds[,2]

    
 



      p <- ggplot(mydata, aes(x = factor(marker), y =trt.effect, ymin = lower, ymax = upper ))
      p <- p + geom_errorbar(size = 1, width = .1) + geom_point(size = 4)
      
    
}
  
  


    #add x/y labels and main
    p <- p + xlab(xlab) + ylab(ylab) + ylim(ylim[1], ylim[2])+ ggtitle(main) 

    #change the names for the legend
    p <- p + stat_hline(yintercept  = mean(trt.effect), aes(linetype = factor(3)))+
      stat_hline(yintercept = 0, aes( linetype = factor(4))) + scale_linetype_manual(name = "Treatment Effect",breaks = c("3", "4"), values = c(3, 4), labels = c("Mean", "Zero"))
    
    p <- p + theme( text = element_text(size=18)) #, 
  p <- p + scale_x_discrete(labels = c(paste(mval[1], "\n(", round(mean(marker==mval[1])*100, 1),"%)", sep = ""), 
                                       paste(mval[2], "\n(", round(mean(marker==mval[2])*100, 1),"%)", sep = "")))
  

  

  print(p) 
    
  return(list(p, mydata))
}
