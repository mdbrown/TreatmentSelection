myplotcompare.trtsel <-
function(x, bootstraps = 500, alpha = .05,
                           ci   = "horizontal", marker.names = c("Marker 1", "Marker 2"),
                           fixeddeltas.y1 =  NULL, fixeddeltas.y2,  
                           xlab = NULL, 
                           ylab = NULL, 
                           xlim = NULL, 
                           ylim = NULL, 
                           main = NULL, offset = offset, conf.bands)
{


  ts1 <- x$trtsel1
  ts2 <- x$trtsel2

  if(substr(ci, 1, 4) =="hori") {
           ## trt selcurve plot with horizontal ci bands
           fix.ind = 2    # fix delta
           out.ind = 1    # vary F.marker
        }else if(substr(ci, 1, 4) =="vert"){
           ## trt sel curve plot with vertical ci bands
           fix.ind = 1    #fix F.event
           out.ind = 2    #vary delta
        }

  fittedrisk.t0.y1 <- ts1$derived.data$fittedrisk.t0
  fittedrisk.t1.y1 <- ts1$derived.data$fittedrisk.t1
 
  delta.y1 <- ts1$derived.data$trt.effect
  link <- ts1$model.fit$link
  fittedrisk.t0.y2 <- ts2$derived.data$fittedrisk.t0
  fittedrisk.t1.y2 <- ts2$derived.data$fittedrisk.t1

  delta.y2 <-  ts2$derived.data$trt.effect


  rho  <- ts1$model.fit$cohort.attributes
  study.design <- ts1$model.fit$study.design
  trt <- ts1$derived.data[[ts1$treatment.name]]
  

  if( link  == "time-to-event"){
    event.name1 = ts1$formula[[2]]
    event.name2 = ts2$formula[[2]]
    
    
  }else{
    event.name1 = as.character(ts1$formula[[2]])
    event.name2 = as.character(ts2$formula[[2]])
  
  }
  

  boot.sample <- ts1$functions$boot.sample
  get.F <- ts1$functions$get.F

###
#old.par <- par(no.readonly = TRUE)


  if( length(fixeddeltas.y1) > 0 & bootstraps >0 ) {


    #bootstrapping done by fixing response and strapping marker 

   # if(is.null(fixed.values) & ci =="horizontal"){
   # fixeddeltas.y1 <- seq(from = min(delta.y1), to = max(delta.y1), length.out = 100)
   # fixeddeltas.y2 <- seq(from = min(delta.y2), to = max(delta.y2), length.out = 100)
   # }else if(is.null(fixed.values) & ci =="vertical"){
   # fixeddeltas.y1 <- 1:100/100
   # fixeddeltas.y2 <- 1:100/100
   
   # }else{
    #fixeddeltas.y1 <- fixed.values
    #fixeddeltas.y2 <- fixed.values
   # }
    if(link == "risks_provided"){
      
      provided_risk <- cbind(fittedrisk.t0.y1, 
                         fittedrisk.t1.y1, 
                         fittedrisk.t0.y2,
                         fittedrisk.t1.y2)
    }else{
      
      provided_risk = NULL
    }

    boot.dat <- replicate( bootstraps, one.boot.plot.compare(data1 = ts1$derived.data, data2 = ts2$derived.data,
                                                             formulas = list(ts1$formula, ts2$formula), 
                                                             event.names = c(event.name1, event.name2), 
                                                             treatment.names = c(ts1$treatment.name, ts2$treatment.name), 
                                                             ci = ci, 
                                                              fixeddeltas.y1 = fixeddeltas.y1, fixeddeltas.y2 = fixeddeltas.y2,
                                                              rho = rho, study.design = study.design,  obp.boot.sample = boot.sample, obp.get.F = get.F, fix.ind, out.ind, link = link, 
                                                             provided_risk = provided_risk, 
                                                             prediction.times = c(x$trtsel1$prediction.time, x$trtsel2$prediction.time)))
    

    if(length(fixeddeltas.y1)==1){
      bounds.delta.y1<- quantile(boot.dat[1,,], probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
      bounds.delta.y2<- quantile(boot.dat[2,,], probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)

      bounds.delta.y1 = t(t(bounds.delta.y1))
      bounds.delta.y2 = t(t(bounds.delta.y2))
    }else{
      bounds.delta.y1<- apply(boot.dat[1,,], 1, function(x, ...){quantile(unlist(x), ...)}, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
      bounds.delta.y2<- apply(boot.dat[2,,], 1, function(x, ...){quantile(unlist(x), ...)}, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
   

    }


    }else{
    bounds.delta.y1 <- NULL
    bounds.delta.y2 <- NULL
    }
    ## end bootstrap
    
    #set up the ylimit of the treatment effect plot
    if(is.null(ylim)){

    if(substr(ci, 1,1) %in% c("v", "V")){
    min.delta <- min(c(delta.y1, delta.y2, bounds.delta.y1, bounds.delta.y2), na.rm=TRUE)
    max.delta <- max(c(delta.y1, delta.y2, bounds.delta.y1, bounds.delta.y2), na.rm=TRUE)

    cen <- mean(c(min.delta, max.delta), na.rm=TRUE)
    }else{

    min.delta <- min(c(delta.y1, delta.y2), na.rm=TRUE)
    max.delta <- max(c(delta.y1, delta.y2), na.rm=TRUE)

    cen <- mean(c(min.delta, max.delta), na.rm=TRUE)
    
    }

    ran <- max.delta - min.delta
    ran <- ran*1.1
    ylim <- c(cen-ran/2, cen+ran/2)
    }
  


    ts1.curves <- trteffectPLOTcompare_gg(x1=ts1, x2 = ts2, ci = ci, ci.bounds = rbind(bounds.delta.y1, bounds.delta.y2), get.F = get.F, fixed.values = rbind(fixeddeltas.y1, fixeddeltas.y2+offset), conf.bands = conf.bands, rho=rho, xlab=xlab, ylab = ylab, xlim=xlim, ylim = ylim, main = main) 

    p <- ts1.curves[[1]]
    p <- p + scale_linetype_manual(name = "", breaks = c("1","2", "3", "4"), values = c(1, 2, 3, 4), labels = c(marker.names, "Mean", "Zero"))
    p <- p + scale_size_manual(name = "", breaks = c("1", "2", "3", "4"), values = c(1, 1, .5, .5), labels = c(marker.names, "Mean", "Zero"))
    
  print(p)
    #if(is.null(xlim)) xlim = c(0,100)
    #legend(x=xlim[2]+diff(xlim)/15, y = quantile(ylim, prob = .5), legend = marker.names, lty = c(1,2), col = c("black", "black"), bty="n", cex = 1, xpd = TRUE, lwd=c(2,2))

   
   if(bootstraps >0 & length(fixeddeltas.y1) > 0 ){
   conf.ints.y1 <- as.data.frame(cbind(fixeddeltas.y1, t(bounds.delta.y1)))
   names(conf.ints.y1) <- c("fixed", "lower", "upper")
   conf.ints.y2 <- as.data.frame(cbind(fixeddeltas.y2, t(bounds.delta.y2)))
   names(conf.ints.y2) <- c("fixed", "lower", "upper")


   result <- list("plot" = p, 
                  "trtsel1" = list( "conf.intervals" = conf.ints.y1), 
                  "trtsel2" = list( "conf.intervals" = conf.ints.y2))
 
   }else{

   result <- list("plot" = p)
 

   }

   invisible(result)
#par(old.par)

}


myplotcompare.trtsel_disc <-
  function(x, bootstraps = 500, alpha = .05,
           ci   = "horizontal", marker.names = c("Marker 1", "Marker 2"),  
           xlab = NULL, 
           ylab = NULL, 
           xlim = NULL, 
           ylim = NULL, 
           main = NULL, offset = offset, conf.bands,  annotate.plot)
  {
    

    quantile <- NULL #appease check
    ts1 <- x$trtsel1
    ts2 <- x$trtsel2
    fittedrisk.t0.y1 <- ts1$derived.data$fittedrisk.t0
    fittedrisk.t1.y1 <- ts1$derived.data$fittedrisk.t1
    
    marker1 = ts1$derived.data[[ts1$model.fit$marker.names]]
    delta.y1 <- ts1$derived.data$trt.effect
    link <- ts1$model.fit$link
    fittedrisk.t0.y2 <- ts2$derived.data$fittedrisk.t0
    fittedrisk.t1.y2 <- ts2$derived.data$fittedrisk.t1
    marker2 <- ts2$derived.data[[ts2$model.fit$marker.names]]
    delta.y2 <-  ts2$derived.data$trt.effect
    

    rho  <- ts1$model.fit$cohort.attributes
    study.design <- ts1$model.fit$study.design
    trt <- ts1$derived.data$trt
    
    if( ts1$model.fit$link == "time-to-event"){
      warning("plotting comparisons of two discrete markers with a time-to-event outcome is not implemented yet.")
      return(NULL)
    }else{
      event  <- ts1$derived.data[[as.character(ts1$formula[[2]])]]
    

    boot.sample <- ts1$functions$boot.sample
    
    
    
    
    
    one.boot.plot_disc <-
      function(event, trt, marker1, marker2, w1, w2,  rho = rho,  obp.boot.sample){
        
        myboot.sample <- obp.boot.sample( event, trt, rho)
        
        rho.b <- myboot.sample[1:7]
        ind   <- myboot.sample[-c(1:7)]
        
        event.b  <- event[ind]
        trt.b  <- trt[ind]
        marker1.b  <- marker1[ind] 
        marker2.b  <- marker2[ind] 
        mval1 <- sort(unique(marker1))
        mval2 <- sort(unique(marker2))
        
        
        c(   trteff.mkr10 =mean(event.b[trt.b==0 & marker1.b ==mval1[1]]) - mean(event.b[trt.b==1 & marker1.b ==mval1[1]]), 
             trteff.mkr11 =mean(event.b[trt.b==0 & marker1.b ==mval1[2]]) - mean(event.b[trt.b==1 & marker1.b ==mval1[2]]),
             trteff.mkr20 =mean(event.b[trt.b==0 & marker2.b ==mval2[1]]) - mean(event.b[trt.b==1 & marker2.b ==mval2[1]]), 
             trteff.mkr21 =mean(event.b[trt.b==0 & marker2.b ==mval2[2]]) - mean(event.b[trt.b==1 & marker2.b ==mval2[2]])
             
        )
        
      }
    
    if(conf.bands){
      boot.data <- replicate(bootstraps, one.boot.plot_disc( event, trt, marker1, marker2, rho,obp.boot.sample = boot.sample))
      mval1 <- sort(unique(marker1))
      mval2 <- sort(unique(marker2))
      
      row.names(boot.data) = c(paste("trteffect.1mkr", mval1[1], sep = ""), 
                               paste("trteffect.1mkr", mval1[2], sep = ""),
                               paste("trteffect.2mkr", mval2[1], sep = ""), 
                               paste("trteffect.2mkr", mval2[2], sep = ""))
      
      #horizontal
      if(substr(ci, 1,1) =="h") { warning("Horizontal CI bands are not allowed for treatment effect plots with a discrete marker. Vertical bands will be computed"); ci <- "vertical";}
      
      #vertical
      myconf.ints <- apply(boot.data, 1, quantile, probs = c(alpha/2, 1-alpha/2))
      
      ci = "vertical"
    }else{
      myconf.ints = NULL
    }
    
    
    ts1.curves <- trteffectPLOTcompare_gg_disc(x1=ts1, 
                                               x2 = ts2, 
                                               ci.bounds = myconf.ints, 
                                               conf.bands = conf.bands,
                                               offset = offset,
                                               xlab=xlab, ylab = ylab,
                                               xlim=xlim, ylim = ylim, 
                                               main = main, 
                                               marker.names = marker.names, 
                                               annotate.plot = annotate.plot) 
    
    p <- ts1.curves[[1]]
    print(p)
    #if(is.null(xlim)) xlim = c(0,100)
    #legend(x=xlim[2]+diff(xlim)/15, y = quantile(ylim, prob = .5), legend = marker.names, lty = c(1,2), col = c("black", "black"), bty="n", cex = 1, xpd = TRUE, lwd=c(2,2))
    
    
    result <- list("plot" = p, 
                   "ci.bounds" = ts1.curves[[2]])
    
    
    }
    
    invisible(result)
    #par(old.par)
    
  }

