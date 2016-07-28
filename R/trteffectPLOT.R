

trteffectPLOT_gg <-
  function(x, ci, ci.bounds, get.F, fixed.values, conf.bands,  rho, xlab, ylab, xlim, ylim, main, markerTWO=FALSE, lty = 1,  p=NULL){ 
    
  
    trt.effect <- x$derived.data$trt.effect
    
    if(x$model.fit$link == "time-to-event"){
      event = rep(0, nrow(x$derived.data))
      event.name = x$formula[[2]]
    }else{
      event <- x$derived.data[[as.character(x$formula[[2]])]]
      event.name = as.character(x$formula[[2]])
    }
    trt <- x$derived.data[[x$treatment.name]]
    n = length(trt.effect)
    
    F.D <- get.F(trt.effect, event, trt, rho = rho)*100
    
    mydata = data.frame(trt.effect, F.D, lty )
    mydata = mydata[with(mydata, order(F.D)),]
    

    ## need to adjust these for scc and cc sample designs. 
    allMeasures <- x$functions$get.summary.measures(data = x$derived.data,  
                                                    event.name = event.name , 
                                                    treatment.name = x$treatment.name, 
                                                    rho = rho, x$model.fit$thresh)
   
    
    avglines <- cbind(0, sort(F.D), 4)
    avglines <- rbind(avglines,
                      cbind(allMeasures$ER.trt0.mod - allMeasures$ER.trt1.mod,
                            sort(F.D), 3))
    avglines = data.frame(avglines); names(avglines) = names(mydata)
    mydata <- rbind(mydata, avglines)
    
    
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
      p <- ggplot()     
      
      #add x/y labels and main
      p <- p + xlab(xlab) + ylab(ylab) + ylim(ylim[1], ylim[2])+ ggtitle(main) 
      
     # p <- p + theme( text = element_text(size=18)) #, 
      
      p <- p + scale_x_continuous(limits = xlim)
      
    }
    
    if(!is.null(ci.bounds)){
      
      ci.bounds <- matrix(ci.bounds, ncol=length(fixed.values), nrow = 2)
      
      if(substr(ci, 1,1)=="v"){
        
        index.fix  <- (fixed.values<= max(F.D) & fixed.values >= min(F.D))
        width = 5
      }else{
        width = .05
        index.fix  <- (fixed.values<= max(trt.effect) & fixed.values >= min(trt.effect)) 
      }
      
      p <- shade_gg(p, ci.bounds[,index.fix], fixed.values[index.fix], type = substr(ci, 1, 1), bands = conf.bands, lty, width = width)
    }
    
    
    
    
    
    p <- p+geom_step(data =  mydata[(1:(n)),], aes(x = F.D, y = trt.effect), direction = "vh")
    p <- p+geom_line(data =  mydata[-c(1:(n)),], aes(x = F.D, y = trt.effect, linetype = factor(lty)), size = 0.5)
    
    
    p <- p+ scale_linetype_manual(name = "Treatment Effect", breaks = c( "3", "4"), values = c( 3, 4), labels = c("Mean", "Zero"))
    
    
    print(p) 
    
    return(list(p=p))
  }


trteffectPLOT_gg_disc <-
  function(x, ci, ci.bounds, get.F,  xlab, ylab, xlim, ylim, main, markerTWO=FALSE, lty = 1,  p=NULL, trt.names = NULL){ 
    
    trt.effect <- x$derived.data$trt.effect
    marker <- x$derived.data$marker
    if(x$model.fit$link == "time-to-event"){
      event = rep(0, nrow(x$derived.data))
      event.name = x$formula[[2]]
      #setting event.name to treatment.name: this doesn't matter since we use model 
      #based estimates of event rates to get the marginal treatment effect for the plots. 
    }else{
      event <- x$derived.data[[as.character(x$formula[[2]])]]
      event.name = as.character(x$formula[[2]])
    }
   
    trt <- x$derived.data[[x$treatment.name]]
    n = length(trt.effect)
    mval = sort(unique(marker))
    Treatment.Effect <- lower <- upper <- NULL ## appease check
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
      
      
    }
    
    if(!is.null(ci.bounds)){
      
      #order matters here!
      mydata[mydata$marker==mval[1], 3:4] <- ci.bounds[,1]
      mydata[ mydata$marker==mval[2], 3:4] <- ci.bounds[,2]
      
      
      
      
      
      
      p <- ggplot(mydata, aes(x = factor(marker), y =trt.effect, ymin = lower, ymax = upper ))
      p <- p + geom_errorbar( width = .1) + geom_point()
      
      
    }else{
      p <- ggplot(mydata, aes(x = factor(marker), y =trt.effect ))
      p <- p + geom_point()
      
    }
    
    #so that the mean trt effect is scaled properly for subcohort designs
    allMeasures <- x$functions$get.summary.measures( data = x$derived.data, 
                                                     event.name = event.name, 
                                                     treatment.name = x$treatment.name, 
                                                     rho =x$model.fit$cohort.attributes, 
                                                     d= x$model.fit$thresh)
    
    
    #add x/y labels and main
    p <- p + xlab(xlab) + ylab(ylab) + ylim(ylim[1], ylim[2])+ ggtitle(main) 
    
    
    hlines.dat <- data.frame("Treatment.Effect" = c(allMeasures$ER.trt0.mod - allMeasures$ER.trt1.mod, 0), 
                             "lty" = factor(c(3,4)), 
                             "labels" = c("Mean", "Zero"))
    
    #change the names for the legend
    p <- p + geom_hline(data = hlines.dat, aes(yintercept  = Treatment.Effect, linetype = lty))+
      
      scale_linetype_manual(name = "Treatment Effect",
                            breaks = c("3", "4"), 
                            values = c(3, 4), 
                            labels = c("Mean", "Zero"))
    
    #p <- p + theme( text = element_text(size=14)) #, 
    p <- p + scale_x_discrete(labels = c(paste(mval[1], "\n(", round(mean(marker==mval[1])*100, 1),"%)", sep = ""), 
                                         paste(mval[2], "\n(", round(mean(marker==mval[2])*100, 1),"%)", sep = "")))
    
    
    
    
    print(p) 
    
    return(list(p, mydata))
  }


trteffectPLOTcompare_gg <-
  function(x1, x2, ci, ci.bounds, get.F, fixed.values, conf.bands,  rho, xlab, ylab, xlim, ylim, main, markerTWO=FALSE, lty = 1,  p=NULL){ 
    
    mylim <- NULL
    trt.effect <- x1$derived.data$trt.effect
    
    
    if(x1$model.fit$link == "time-to-event"){
      event = rep(0, nrow(x1$derived.data))
      event.name = x1$formula[[2]]
      #setting event.name to treatment.name: this doesn't matter since we use model 
      #based estimates of event rates to get the marginal treatment effect for the plots. 
    }else{
      event <- x1$derived.data[[as.character(x1$formula[[2]])]]
      event.name = as.character(x1$formula[[2]])
    }
    trt <- x1$derived.data[[x1$treatment.name]]
    
    trt.effect2 <- x2$derived.data$trt.effect
    
    if(x1$model.fit$link == "time-to-event"){
      event2 = rep(0, nrow(x1$derived.data))
      event.name = x1$formula[[2]]
      x1$derived.data$prediction.time = x1$prediction.time
      #setting event.name to treatment.name: this doesn't matter since we use model 
      #based estimates of event rates to get the marginal treatment effect for the plots. 
    }else{
      event <- x2$derived.data[[as.character(x2$formula[[2]])]]
      event.name = as.character(x2$formula[[2]])
    }
    trt2 <- x2$derived.data[[x2$treatment.name]]
    
    n = length(trt.effect)
    
    F.D <- get.F(trt.effect, event, trt, rho = rho)*100
    F.D2 <- get.F(trt.effect2, event2, trt2, rho = rho)*100
    
    mydata = data.frame(trt.effect, F.D, lty = 1, size =1)
    mydata = mydata[with(mydata, order(F.D)),]
    
    mydata2 = data.frame(trt.effect = trt.effect2, F.D = F.D2,lty =  2, size = 1 )
    mydata2 = mydata2[with(mydata2, order(F.D)),]
    mydata <- rbind(mydata, mydata2)
    # mydata <- rbind(mydata, mydata2, c(-100, -100, 3,.5), c(-100, 100, 4, .5))
    
    
    ## need to adjust these for scc and cc sampling designs
    allMeasures <- x1$functions$get.summary.measures( x1$derived.data, event.name, x1$treatment.name,  rho, x1$model.fit$thresh)

    avglines <- cbind(0, sort(F.D), 4, .5)
    avglines <- rbind(avglines, 
                      cbind(allMeasures$ER.trt0.emp-allMeasures$ER.trt1.emp, 
                            sort(F.D), 3, .5))
    avglines = data.frame(avglines); names(avglines) = names(mydata)
    mydata <- rbind(mydata, avglines)
    
    if(is.null(xlab)) xlab <- "% population below treatment effect"
    if(is.null(ylab)) ylab <- "treatment effect"
    if(is.null(xlim)) xlim <- c(0,100)
    if(is.null(ylim)) ylim <- mylim
    if(is.null(main)) main <- "Treatment effect distribution"
    p <- ggplot(mydata) 
    
    
    #add x/y labels and main
    p <- p + xlab(xlab) + ylab(ylab) + ylim(ylim[1], ylim[2])+ ggtitle(main) 
    
    # p <- p + stat_hline(yintercept  = mean(trt.effect), aes(linetype = factor(3), size = factor(3)), show.guide = FALSE)+
    #   stat_hline(yintercept = 0, aes( linetype = factor(4), size = factor(4)), show.guide = FALSE)
    
   # p <- p + theme( text = element_text(size=14)) #, 
    
    
    p <- p + scale_x_continuous(limits = xlim)
    
    
    if(!is.null(ci.bounds)){
      
      
      fixed.values1 <- fixed.values[1,]
      fixed.values2 <- fixed.values[2,]
      
      
      if(substr(ci, 1,1)=="v"){
        
        index.fix1  <- (fixed.values1<= max(F.D) & fixed.values1 >= min(F.D)) 
        index.fix2 <- (fixed.values2<= max(F.D2) & fixed.values2 >= min(F.D2))
        
        width = 5
        
      }else{
        index.fix1  <- (fixed.values1<= max(trt.effect) & fixed.values1 >= min(trt.effect)) 
        index.fix2  <- (fixed.values2<= max(trt.effect2) & fixed.values2 >= min(trt.effect2)) 
        width = .05
      }
      
      p <- shade_gg(p, ci.bounds[1:2,index.fix1], fixed.values1[index.fix1], type = substr(ci, 1, 1), bands = conf.bands, lty=1, width = width)
      p <- shade_gg(p, ci.bounds[3:4,index.fix2], fixed.values2[index.fix2], type = substr(ci, 1, 1), bands = conf.bands, lty=2, width = width)
      
    }
    
    
    p <- p+geom_step(data =  mydata[(1:(2*n)),], aes(x = F.D, y = trt.effect, linetype = factor(lty), size = factor(lty)), direction = "vh")
    p <- p+geom_line(data =  mydata[-c(1:(2*n)),], aes(x = F.D, y = trt.effect, linetype = factor(lty), size = factor(lty)))
    
    
    
    return(list(p=p))
  }

trteffectPLOTcompare_gg_disc <-
  function(x1, x2, ci.bounds, conf.bands, offset,  xlab, ylab, xlim, ylim, main, marker.names, lty = 1,  annotate.plot = TRUE){ 
    
    
    trt.effect1 <- x1$derived.data$trt.effect
    marker1 <- x1$derived.data$marker
    if(x1$model.fit$link == "time-to-event"){
      event = rep(0, nrow(x1$derived.data))
      event.name = x1$formula[[2]]
      #setting event.name to treatment.name: this doesn't matter since we use model 
      #based estimates of event rates to get the marginal treatment effect for the plots. 
    }else{
      event <- x1$derived.data[[as.character(x1$formula[[2]])]]
      event.name = as.character(x1$formula[[2]])
    }
    trt <- x1$derived.data[[x1$treatment.name]]
    n = length(trt.effect1)
    mval1 = sort(unique(marker1))
    
    
    trt.effect2 <- x2$derived.data$trt.effect
    mkrvals <- unique(c(marker1, x2$derived.data$marker))
    marker2 <- x2$derived.data$marker + offset
    mval2 = sort(unique(marker2))
    markerValue <- markerName <- trt.effect <- lower <- upper <- NULL
    
    mydata = data.frame("trt.effect" = c(trt.effect1, trt.effect2),
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

    allMeasures <- x1$functions$get.summary.measures( data = x1$derived.data, 
                                                     event.name = event.name, 
                                                     treatment.name = x1$treatment.name, 
                                                     rho =x1$model.fit$cohort.attributes, 
                                                     d= x1$model.fit$thresh)
    
    
    #add x/y labels and main
    hlines.dat <- data.frame("trt.effect" = c(allMeasures$ER.trt0.mod - allMeasures$ER.trt1.mod, 0), 
                             "markerName" = c("Mean", "Zero"))
    
    if(!is.null(ci.bounds)){
      
      #order matters here!
      mydata[mydata$markerValue==mval1[1] & mydata$markerName == marker.names[1], 4:5] <- ci.bounds[,1]
      mydata[ mydata$markerValue==mval1[2]& mydata$markerName == marker.names[1], 4:5] <- ci.bounds[,2]
      mydata[mydata$markerValue==mval2[1] & mydata$markerName == marker.names[2], 4:5] <- ci.bounds[,3]
      mydata[ mydata$markerValue==mval2[2]& mydata$markerName == marker.names[2], 4:5] <- ci.bounds[,4]
      
      
      p <- ggplot(data = mydata, aes(x = markerValue, y =trt.effect, shape = factor(markerName), linetype = factor(markerName), ymin = lower, ymax = upper ))
      p <- p + geom_errorbar( width = .05, size = .9) + geom_point()
      
      #change the names for the legend
      p <- p + 
        geom_hline(data = hlines.dat, aes(yintercept = trt.effect,  
                                          linetype = factor(markerName), 
                                          shape = factor(markerName)), size = .5) + 
        scale_shape_manual(name = "", values = c(16,17, 32,32)) + 
        scale_linetype_manual(name = "", values = c(1,2, 3, 4))
      
      
    }else{
      
      p <- ggplot(data = mydata, aes(x = markerValue, y =trt.effect, shape = factor(markerName)))
      p <- p +geom_point()
      
      p <- p + 
        geom_hline(data = hlines.dat, aes(yintercept = trt.effect,  
                                          linetype = factor(markerName)), size = .5) +  
        scale_shape_manual(name = "", values = c(16,17)) + 
        scale_linetype_manual(name = "", values = c(1,2)) + guides(shape = guide_legend(order = 1))
      
    }
    
    
    
    
    #add x/y labels and main
    p <- p + xlab(xlab) + ylab(ylab) + ggtitle(main) 
    if(!is.null(ylim)) p <- p + ylim(ylim[1], ylim[2])
    
    #change the names for the legend
    
   # p <- p + theme( text = element_text(size=14)) #, 
    
    
    mkrprop = round(c( mean(mydata$markerVal[1]==marker1), 
                       mean(mydata$markerVal[2]==marker1), 
                       mean(mydata$markerVal[3]==marker2), 
                       mean(mydata$markerVal[4]==marker2))*100, 1)
    mkrprop = paste("   (", mkrprop, "%)", sep = "")
    if(annotate.plot){
      p <- p + annotate("text", x= mydata$markerVal+offset, y = mydata$trt.effect, label= mkrprop)
    }
    
    p <- p + scale_x_continuous(breaks = mkrvals, limits = xlim)
    
    
    
    return(list(p, mydata))
    
  }

