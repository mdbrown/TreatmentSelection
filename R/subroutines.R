cover <-
  function(min, max, value)  !(max < value | min > value)


expit <-
  function(u) exp(u)/(1+exp(u))


logit <-
  function(u){
    
    sapply( u , logit.one)
  
  }


Theta.cutoff <- function(trt.effect, f.d){
  #calculate theta(cutoff) for each entry in trt.effect. 
  # trt.effect should already be sorted by 
  
  out <- cumsum(-trt.effect[order(f.d)])/length(trt.effect)
  out[rank(f.d)]
}


logit.one <-
  function(u){
    
    if(is.finite(u)){
      if(0<u & u<1){
        out <- log(u)-log(1-u)
      }
      else if( u ==0){
        u   <- u +.0001
        out <- log(u)-log(1-u)
      }
      else if(u==1){
        u   <- u -.0001
        out <- log(u)-log(1-u)
      }else {
        warning("error..negative value passed to logit function")
        out <- NA
      }
    }else{
      
      warning("error..NaN value passed to logit function")
      out <- NA
      
    }
    
    out
    
  }

sum.I <-
  function(yy,FUN,Yi){
    
    if (FUN=="<"|FUN==">=") { yy <- -yy; Yi <- -Yi}
    
    pos <- rank(c(yy,Yi),ties.method='max')[1:length(yy)]-rank(yy,ties.method='max')
    
    if (substring(FUN,2,2)=="=") pos <- length(Yi)-pos
    
    return(pos)
  }



shade <-
  function(bounds, fixed, type, lty = 1, bands){
    
    numbounds <- ncol(bounds)
    if(is.null(numbounds)) {
      numbounds <- 1
      bounds <- matrix(bounds, ncol = 1)
    } 
    
    if(type == "v"){
      if(bands == FALSE){
        for( i in 1:numbounds) { 
          lines( rep(fixed[i], 2), bounds[1:2, i], lwd = 1.5, col = "gray50", lty = lty) 
          points( rep(fixed[i], 2), bounds[1:2, i], pch = "_", col = "gray50", cex = 1.1)
        }
      }else{
        
        polygon( c(fixed, rev(fixed)), c(bounds[1,], rev(bounds[2,])),density = 25, angle = 90, col="grey50", border = NA, lty = lty  )
        lines( fixed, bounds[1,], col="grey50") 
        lines( fixed, bounds[2,], col="grey50")
        
      }
    }else if(type =="h"){
      if(bands == FALSE){
        for(i in 1:numbounds) { 
          lines( bounds[1:2, i], rep(fixed[i], 2), lwd = 1.5,col = "gray50", lty = lty) 
          points( bounds[1:2, i], rep(fixed[i], 2), pch = "|", col = "gray50", cex = 1.1)
        }
      }else{
        polygon( c(bounds[1,], rev(bounds[2,])), c(fixed,rev(fixed)),density = 25, angle = 0, col="grey50", border = NA , lty = lty  )
        lines( bounds[1,], fixed,  col="grey") 
        lines( bounds[2,], fixed,  col="grey")
        
        
      }
    }
  }


shade_gg <-
  function(p, bounds, fixed, type, lty = 1, bands, width=5){
    
    low <- high <- y <- NULL #appease check
    
    if(any(dim(bounds)==0)){
      
      return(p )
    }
    
  
    yend <- xend <- NULL #to appease check
    
    numbounds <- ncol(bounds)
    if(is.null(numbounds)) {
      numbounds <- 1
      bounds <- matrix(bounds, ncol = 1)
    } 
    
    mydatERRORBAR <- data.frame(t(bounds), fixed, row.names = NULL)
    names(mydatERRORBAR) = c("low", "high", "fixed")
    
    mydat <- data.frame( c(bounds[1,], rev(bounds[2,])), c(fixed, rev(fixed)))
    names(mydat) = c("bounds", "fixed")
    if(type == "v"){
      #vertical ci
      
      if(bands == FALSE){
        p <- p + geom_errorbar(data = mydatERRORBAR, aes(ymin = low, ymax = high, x = fixed), linetype = lty, color = "gray50", size = 1, width = width)
      }else{
        #p <- p + geom_ribbon(data = mydat, alpha =.2, aes(x = fixed, ymin = low, ymax = high))
        p <- p + geom_polygon(data = mydat, alpha = .2, aes(y = bounds, x = fixed))
      }
    }else if(type =="h"){
      if(bands == FALSE){
        
        #p <- p + geom_errorbar(data = mydatERRORBAR, aes(ymin = low, ymax = high, x = fixed), linetype = lty, color = "gray50", size = 1, width = width) 
        p <- p + geom_segment(data = mydatERRORBAR, aes(y = fixed, yend = fixed, x = low, xend = high), linetype = lty, color = "gray50", size = 1)#, width = width)
        mydat$y = mydat$fixed-width/2; mydat$yend = mydat$fixed + width/2
        p <- p + geom_segment(data = mydat, aes(y = y, yend=yend, x = bounds, xend = bounds), linetype = lty, color = "gray50", size = 1)
        
      }else{
        
        #p <- p + geom_ribbon(data = mydat, alpha =.2, aes(x = fixed, ymin = low, ymax = high)) #+ scale_x_reverse()
        p <- p + geom_polygon(data = mydat, alpha = .2, aes(x = bounds, y = fixed))
      }
    }
    return(p)
    
  }


