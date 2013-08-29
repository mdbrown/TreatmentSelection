trteffectPLOT_gg <-
function(x, ci, ci.bounds, get.F, fixed.values, conf.bands,  rho, xlab, ylab, xlim, ylim, main, markerTWO=FALSE, lty = 1, mar, p=NULL,  ...){ 
  
  trt.effect <- x$derived.data$trt.effect
  marker <- x$derived.data$marker
  event <- x$derived.data$event
  trt <- x$derived.data$trt
  n = length(trt.effect)
  
  F.D <- get.F(trt.effect, event, trt, rho = rho)*100
  
  mydata = data.frame(trt.effect, F.D )
  mydata = mydata[with(mydata, order(F.D)),]
  

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
 
  ci.bounds <- matrix(ci.bounds, ncol=length(fixed.values), nrow = 2)

  if(substr(ci, 1,1)=="v"){

    index.fix  <- (fixed.values<= max(F.D) & fixed.values >= min(F.D)) 
  }else{

    index.fix  <- (fixed.values<= max(trt.effect) & fixed.values >= min(trt.effect)) 
  }
  
  p <- shade_gg(p, ci.bounds[,index.fix], fixed.values[index.fix], type = substr(ci, 1, 1), bands = conf.bands, lty)
  }
  
  

  if(substr(ci, 1, 1)=="h"){
    #we used coord_flip, so we switch x and y, otherwise dont
    p <- p+geom_step(data = mydata, aes(y = F.D, x = trt.effect), direction = "hv", size = 1)
    
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

    p <- p+geom_step(data = mydata, aes(x = F.D, y = trt.effect), direction = "vh", size = 1)
    
    #add x/y labels and main
    p <- p + xlab(xlab) + ylab(ylab) + ylim(ylim[1], ylim[2])+ ggtitle(main) 

    #change the names for the legend
    p <- p + stat_hline(yintercept  = mean(trt.effect), aes(linetype = factor(3)))+
      stat_hline(yintercept = 0, aes( linetype = factor(4))) + scale_linetype_manual(name = "Treatment Effect",breaks = c("3", "4"), values = c(3, 4), labels = c("Mean", "Zero"))
    
    p <- p + theme( text = element_text(size=18)) #, 

    
    p <- p + scale_x_continuous(limits = xlim)
    
  }
  

  print(p) 
    
  return(list(p, ci.bounds))
}
