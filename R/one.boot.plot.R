one.boot.plot <-
function(x, ci, fixed.values, fix.ind, out.ind){

  if( x$model.fit$link == "time-to-event"){
    mysurv <- with(x$derived.data, eval(x$formula[[2]]))
    event <- mysurv[,2]
  }else{
    event  <- x$derived.data[[as.character(x$formula[[2]])]]
  }
  
  myboot.sample <- x$functions$boot.sample( event, 
                                            x$derived.data[[x$treatment.name]],
                                            rho = x$model.fit$cohort.attributes)
  #this makes it work for step function
  if(substr(ci, 1,1)=="h") addind = 0 
  else addind = 1


  rho.b <- myboot.sample[1:7]
  ind   <- myboot.sample[-c(1:7)]

  if( x$model.fit$link == "time-to-event"){
    event.b <- 0
  }else{
   event.b  <- x$derived.data[[as.character(x$formula[[2]])]][ind]
  }
  trt.b    <- x$derived.data[[x$treatment.name]][ind]


  if(x$model.fit$link == "risks_provided"){
    obsrisk.t0.b <- x$derived.data$fittedrisk.t0[ind]
    obsrisk.t1.b <- x$derived.data$fittedrisk.t1[ind]
    linkinvfun <- NULL
    marker.b <- obsrisk.t0.b - obsrisk.t1.b#
  }else if(x$model.fit$link == "time-to-event"){
    
    coxfit <- do.call(coxph, list(x$formula, x$derived.data[ind,]))
  

    obsrisk.t0.b  <- get.risk.t_coxph(coxfit, x$treatment.name, x$derived.data[ind,], x$prediction.time, t = 0)
    obsrisk.t1.b  <- get.risk.t_coxph(coxfit, x$treatment.name, x$derived.data[ind,], x$prediction.time, t = 1)
    #we still need to incorporate the nelson aalen baseline haz to get absolute risk at t = 'prediction.time'

    
  }else{

    coef <- unname(get.coef(x$formula, x$treatment.name, x$derived.data[ind,], 
                            x$model.fit$study.design, 
                            rho.b, 
                            link = x$model.fit$link)[,1])
    
    linkinvfun <- binomial(link = x$model.fit$link)$linkinv
    obsrisk.t0.b  <-  get.risk.t(coef, x$formula, x$treatment.name, data = x$derived.data[ind,], linkinvfun, t = 0)
    obsrisk.t1.b  <-  get.risk.t(coef, x$formula, x$treatment.name, data = x$derived.data[ind,], linkinvfun, t = 1)
    wi = 0
  }
  
  obsdelta.b <-obsrisk.t0.b - obsrisk.t1.b#
  
  ## if there is a single marker, use that to calculate F.Y, otherwise use treatment 
  ## effect as the marker 
  marker.b <- x$derived.data[['marker']][ind]
  if(is.null(marker.b)) marker.b <- obsrisk.t0.b - obsrisk.t1.b#
  
  F.Y <- x$functions$get.F( marker.b,  event.b, trt.b,  rho.b)*100#
  F.D <- x$functions$get.F( obsdelta.b, event.b, trt.b, rho.b)*100#
  
  theta.c <- EventRateVec(obsrisk.t0.b, obsrisk.t1.b, F.D, rho.b, event.b, trt.b)

  #all 
  all  <- cbind( F.Y, obsrisk.t0.b, obsrisk.t1.b, F.D, obsdelta.b, theta.c)
  all <- unique(all)
#  browser()
  if(length(fix.ind) > 1){
  myorder <- apply(all[,fix.ind], 2, order)
  
  out <- matrix(0, ncol = length(fixed.values), nrow = length(fix.ind))
  
  for( i in 1:length(fix.ind)){
    #for decreasing pred curves with horizontal bands
    if(is.element(fix.ind[i], c(2,3)) & !(all.equal(order(all[,fix.ind[i]]), order(all[,1]))==TRUE)){ addind = 1} 
    
  tmpind <- sum.I(fixed.values, ">=", all[myorder[,i],fix.ind[i]])+addind
  tmpind[tmpind<=0] <- NA
  tmpall <- all[myorder[,i],out.ind[i]]
  out[i, ] <- tmpall[tmpind]
  
  }

  }else{
  myorder <- order(all[,fix.ind])
  
  out <- numeric( length(fixed.values))
  
  tmpind <- sum.I(fixed.values, ">=", all[myorder,fix.ind]) +addind
  tmpind[tmpind==0] <- NA
  tmpall <- all[myorder,out.ind]
  out <- tmpall[tmpind]
  
  }

  out
}



one.boot.plot_disc <-
  function(x){
    
    if( x$model.fit$link == "time-to-event"){
      mysurv <- with(x$derived.data, eval(x$formula[[2]]))
      event <- mysurv[,2]
    }else{
      event  <- x$derived.data[[as.character(x$formula[[2]])]]
    }
    
    
    myboot.sample <- x$functions$boot.sample( event = event, 
                                              trt = x$derived.data[[x$treatment.name]], 
                                              rho = x$model.fit$cohort.attributes)
    
    rho.b <- myboot.sample[1:7]
    ind   <- myboot.sample[-c(1:7)]
   # browser()
    #event.b  <- x$derived.data[[as.character(x$formula[[2]])]][ind]
   # trt.b  <- x$derived.data[[x$treatment.name]][ind]
    marker.b  <- x$derived.data$marker[ind] 
    
    if(x$model.fit$link == "risks_provided") 
    {
      provided_risk <- cbind(x$derived.data$fittedrisk.t0[ind], 
                             x$derived.data$fittedrisk.t1[ind])
    } else provided_risk = NULL
    
    
    
    tmp.trtsel <- trtsel.boot(x$formula, x$treatment.name, 
                              data = x$derived.data[ind, ],
                              d=x$model.fit$thresh, 
                              rho = rho.b, 
                              study.design = x$model.fit$study.design,
                              link  =  x$model.fit$link, 
                              disc.marker.neg = x$model.fit$disc.marker.neg, 
                              provided_risk = provided_risk, 
                              prediction.time = x$prediction.time)
    ####
    unique.fitted.vals <- unique(cbind(marker.b, tmp.trtsel$derived.data[,c("fittedrisk.t0", "fittedrisk.t1", "trt.effect")]))
    
    
    mval <- sort(unique(marker.b))
    out <- with( unique.fitted.vals,  
                 c( event.trt0.mkr0 = fittedrisk.t0[marker.b==mval[1]], 
                    event.trt1.mkr0 = fittedrisk.t1[marker.b==mval[1]], 
                    event.trt0.mkr1 = fittedrisk.t0[marker.b==mval[2]],
                    event.trt1.mkr1 = fittedrisk.t1[marker.b==mval[2]], 
                    trteff.mkr0 = trt.effect[marker.b==mval[1]], 
                    trteff.mkr1 = trt.effect[marker.b==mval[2]]))
    return(out)
  }



one.boot.plot.compare <-
  function(data1, data2, formulas,  event.names, treatment.names, ci,
           fixeddeltas.y1, fixeddeltas.y2,
           rho = rho, study.design, obp.boot.sample, obp.get.F, fix.ind, out.ind, link,
           provided_risk = NULL, 
           prediction.times){
    if(substr(ci, 1,1)=="h") addind = 0 
    else addind = 1
    
    
    if( link  == "time-to-event"){
      
      mysurv <- with(data1, eval(event.names[[1]]))
      event1 <- mysurv[,2]
      mysurv <- with(data2, eval(event.names[[2]]))
      event2 <- mysurv[,2]
      
    }else{

      event1 <- data1[[event.names[[1]]]]
      event2 <- data2[[event.names[[2]]]]
    }
    myboot.sample <- obp.boot.sample( event1, data1[[treatment.names[1]]], rho)
    
    rho.b <- myboot.sample[1:7]
    ind   <- myboot.sample[-c(1:7)]
    
 
    event.b = event1[ind]
    trt.b <- data1[[treatment.names[1]]][ind]
    ### marker1
    
    fixed.values <- fixeddeltas.y1
   
    if(link == "risks_provided"){
      
      obsrisk.t0.b <- provided_risk[ind,1]
      obsrisk.t1.b <- provided_risk[ind,2]
      
    }else if(link == "time-to-event"){
      
      coxfit <- do.call(coxph, list(formulas[[1]], data1[ind,]))
      
      
      obsrisk.t0.b  <- get.risk.t_coxph(coxfit, treatment.names[1], data1[ind,], prediction.time = prediction.times[1], t = 0)
      obsrisk.t1.b  <- get.risk.t_coxph(coxfit, treatment.names[1], data1[ind,], prediction.time = prediction.times[1], t = 1)
      #we still need to incorporate the nelson aalen baseline haz to get absolute risk at t = 'prediction.time'
      coef <- summary(coxfit)$coefficients
      
      
      
    }else{
      coef <- unname(get.coef(formulas[[1]], treatment.names[1], data1[ind,],  study.design, rho.b, link)[,1])
      linkinvfun <- binomial(link = link)$linkinv
      obsrisk.t0.b  <-  get.risk.t(coef, formulas[[1]], treatment.names[1], data1[ind,], linkinvfun, t = 0)
      obsrisk.t1.b  <-  get.risk.t(coef, formulas[[1]], treatment.names[1], data1[ind,], linkinvfun, t = 1)
      #obsrisk.t0.b  <- c(ifelse(coef[3]>0, 0,1),        get.risk.t0(coef,  marker.b, linkinvfun))
      #obsrisk.t1.b  <- c(ifelse(sum(coef[3:4])>0, 0,1), get.risk.t1(coef,  marker.b, linkinvfun))
      
    }
    
    obsdelta.b <- obsrisk.t0.b- obsrisk.t1.b
    F.D <- obp.get.F( obsdelta.b, event.b, trt.b, rho.b)*100
    
    #obsdelta.b <- c(-1, obsrisk.t0.b[-1] - obsrisk.t1.b[-1])
    #F.D <- c(0, obp.get.F( obsdelta.b[-1], event.b, trt.b, rho.b))*100
    
    all  <- cbind( F.D, obsdelta.b)
    myorder <- order(all[,fix.ind])
    out <- numeric( length(fixed.values))
    
    tmpind <- sum.I(fixed.values, ">=", all[myorder,fix.ind])
    tmpind[tmpind==0] <- NA
    tmpall <- all[myorder,out.ind]
    out <- tmpall[tmpind]
    
    marker.vals.delta.Y1 <- out
    
    ##
    
    ### Y2
    fixed.values <- fixeddeltas.y2
   
    if(link == "risks_provided"){
      obsrisk.t0.b <- provided_risk[ind,3]
      obsrisk.t1.b <- provided_risk[ind,4]
    }else if(link == "time-to-event"){
      
      coxfit <- do.call(coxph, list(formulas[[2]], data2[ind,]))
      
      
      obsrisk.t0.b  <- get.risk.t_coxph(coxfit, treatment.names[2], data2[ind,], prediction.time = prediction.times[2], t = 0)
      obsrisk.t1.b  <- get.risk.t_coxph(coxfit, treatment.names[2], data2[ind,], prediction.time = prediction.times[2], t = 1)
      #we still need to incorporate the nelson aalen baseline haz to get absolute risk at t = 'prediction.time'
      coef <- summary(coxfit)$coefficients
      
      
      
    }else{
      coef <- unname(get.coef(formulas[[2]], treatment.names[2], data2[ind,],study.design, rho.b, link)[,1])
      linkinvfun <- binomial(link = link)$linkinv
      obsrisk.t0.b  <-  get.risk.t(coef, formulas[[2]], treatment.names[2], data2[ind,], linkinvfun, t = 0)
      obsrisk.t1.b  <-  get.risk.t(coef, formulas[[2]], treatment.names[2], data2[ind,], linkinvfun, t = 1)
      #obsrisk.t0.b  <- c(ifelse(coef[3]>0, 0,1),        get.risk.t0(coef,  marker2.b, linkinvfun))
      #obsrisk.t1.b  <- c(ifelse(sum(coef[3:4])>0, 0,1), get.risk.t1(coef,  marker2.b, linkinvfun))
      
    }
    obsdelta.b <- obsrisk.t0.b - obsrisk.t1.b
    F.D <- obp.get.F( obsdelta.b, event.b, trt.b, rho.b)*100
    
    #obsdelta.b <- c(-1, obsrisk.t0.b[-1] - obsrisk.t1.b[-1])
    #F.D <- c(0, obp.get.F( obsdelta.b[-1], event.b, trt.b, rho.b))*100
    
    all  <- cbind( F.D, obsdelta.b)
    myorder <- order(all[,fix.ind])
    out <- numeric( length(fixed.values))
    
    tmpind <- sum.I(fixed.values, ">=", all[myorder,fix.ind]) + addind
    tmpind[tmpind==0] <- NA
    tmpall <- all[myorder,out.ind]
    out <- tmpall[tmpind]
    
    marker.vals.delta.Y2 <- out
    
    ##
    
    return( rbind( marker.vals.delta.Y1, marker.vals.delta.Y2))
  }



