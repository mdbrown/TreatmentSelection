one.boot.compare <-
function(data1, data2, formulas,  event.names, treatment.names,  rho, study.design, obe.boot.sample, obe.get.summary.measures, link, d, disc.rec.no.trt1, disc.rec.no.trt2,  prediction.times, bbc){


  
  if( link  == "time-to-event"){
    event.name1 = formulas[[1]][[2]]
    event.name2 = formulas[[2]][[2]]
    
    mysurv <- with(data1, eval(event.name1))
    event1 <- mysurv[,2]
    mysurv <- with(data2, eval(event.name2))
    event2 <- mysurv[,2]
    
  }else{
    event.name1 = as.character(formulas[[1]][[2]])
    event.name2 = as.character(formulas[[2]][[2]])
    
    event1 <- data1[[event.name1]]
    event2 <- data2[[event.name2]]
  }
  

  myboot.sample <- obe.boot.sample( event1, data1[[treatment.names[1]]], rho)

  rho.b <- myboot.sample[1:7]
  ind   <- myboot.sample[-c(1:7)]

  #first model 
  if(link == "risks_provided")
  {
     x1.b <- trtsel.boot( formula = formulas[[1]], treatment.name = treatment.names[1], data = data1[ind,],  
                          d = d, study.design = study.design, rho = rho.b, link = link, disc.rec.no.trt =disc.rec.no.trt1, 
                          provided_risk = cbind(data1$fittedrisk.t0, data1$fittedrisk.t1)[ind,], 
                          prediction.time = prediction.times[[1]])
     coefs1 <- rep(0,4)
  }else{
     x1.b <- trtsel.boot( formula = formulas[[1]], treatment.name = treatment.names[1], data = data1[ind,],
                          d = d, study.design = study.design, rho = rho.b, link = link, disc.rec.no.trt =disc.rec.no.trt1, 
                          prediction.time = prediction.times[[1]])
     coefs1 <- x1.b$model$coefficients[,1]
     coefs1 <- c(coefs1, 0,0,0,0)[1:4]
  }
  

  if(is.null(data1[["rec.no.trt"]])){
    x1.b$derived.data$rec.no.trt <- 1- x1.b$derived.data$rec.trt
    
  }
  
  if(link == "time-to-event") x1.b$derived.data$prediction.time = prediction.times[[1]]
  sm1.b <- obe.get.summary.measures(x1.b$derived.data, event.names[[1]], treatment.names[1],  rho.b)

  

  # bias corrected estimate 
  if(bbc){
    
   
  if(link == "time-to-event"){
    
    coxfit <- do.call(coxph, list(formulas[[1]], data1[ind,]))
    
    
    obsrisk.t0.f <- get.risk.t_coxph(coxfit, treatment.names[[1]], data1, prediction.times[[1]], t = 0)
    obsrisk.t1.f  <- get.risk.t_coxph(coxfit, treatment.names[[1]], data1, prediction.times[[1]], t = 1)
    #we still need to incorporate the nelson aalen baseline haz to get absolute risk at t = 'prediction.time'
    
  }else{
    
    tmpcoef <- unname(get.coef(formulas[[1]],treatment.names[[1]], data1[ind,], 
                            study.design, 
                            rho.b, 
                            link = link)[,1])
    
    linkinvfun <- binomial(link = link)$linkinv
    obsrisk.t0.f  <-  get.risk.t(tmpcoef, formulas[[1]], treatment.names[[1]], data = data1, linkinvfun, t = 0)
    obsrisk.t1.f  <-  get.risk.t(tmpcoef, formulas[[1]], treatment.names[[1]], data = data1, linkinvfun, t = 1)
    wi = 0
    
    
  }
  provided_risk.f <- cbind(obsrisk.t0.f, obsrisk.t1.f)
  
  
  #need to fit the model fit using bootstrap data to the original data
  x.f <- trtsel.boot( formula = formulas[[1]],
                      treatment.name = treatment.names[[1]], 
                      data = data1, 
                      d = d, 
                      study.design = study.design, 
                      rho = rho, 
                      link = "risks_provided", 
                      disc.rec.no.trt = disc.rec.no.trt1, 
                      provided_risk = provided_risk.f, 
                      prediction.time = prediction.times[[1]])
  
  
  x.f$derived.data$prediction.time <- prediction.times[[1]]
  sm1.f <- obe.get.summary.measures(x.f$derived.data, 
                                   event.name = event.names[[1]],
                                   treatment.name = treatment.names[[1]], 
                                   rho)
  
}else{
  sm1.f <- NULL
}

  
  
  
  #second model 
  if(link == "risks_provided")
  {
    x2.b <- trtsel.boot( formula = formulas[[2]], treatment.name = treatment.names[2], data = data2[ind,],  
                         d = d, study.design = study.design, rho = rho.b, link = link, disc.rec.no.trt =disc.rec.no.trt2, 
                         provided_risk = cbind(data2$fittedrisk.t0, data2$fittedrisk.t1)[ind,], 
                         prediction.time = prediction.times[[2]])
    coefs2 <- rep(0, 4)
  }else{
    x2.b <- trtsel.boot( formula = formulas[[2]], treatment.name = treatment.names[2], data = data2[ind,], 
                         d = d, study.design = study.design, rho = rho.b, link = link, disc.rec.no.trt =disc.rec.no.trt2, 
                         prediction.time = prediction.times[[2]])
    coefs2 <- x2.b$model$coefficients[,1]
    coefs1 <- c(coefs1, 0,0,0,0)[1:4]
  }
  
  if(is.null(data1[["rec.no.trt"]])){
    x2.b$derived.data$rec.no.trt <- 1- x2.b$derived.data$rec.trt
    
  }
  
  if(link == "time-to-event") x2.b$derived.data$prediction.time = prediction.times[[1]]
  sm2.b <- obe.get.summary.measures(x2.b$derived.data, event.names[[2]], treatment.names[2], rho.b)

  if(bbc){
    if(link == "time-to-event"){
      
      coxfit <- do.call(coxph, list(formulas[[2]], data2[ind,]))
      
      
      obsrisk.t0.f <- get.risk.t_coxph(coxfit, treatment.names[[2]], data2, prediction.times[[2]], t = 0)
      obsrisk.t1.f  <- get.risk.t_coxph(coxfit, treatment.names[[2]], data2, prediction.times[[2]], t = 1)
      #we still need to incorporate the nelson aalen baseline haz to get absolute risk at t = 'prediction.time'
      
    }else{
      
      tmpcoef <- unname(get.coef(formulas[[2]],treatment.names[[2]], data2[ind,], 
                                 study.design, 
                                 rho.b, 
                                 link = link)[,1])
      
      linkinvfun <- binomial(link = link)$linkinv
      obsrisk.t0.f  <-  get.risk.t(tmpcoef, formulas[[2]], treatment.names[[2]], data = data2, linkinvfun, t = 0)
      obsrisk.t1.f  <-  get.risk.t(tmpcoef, formulas[[2]], treatment.names[[2]], data = data2, linkinvfun, t = 1)
      wi = 0
      
      
    }
    provided_risk.f <- cbind(obsrisk.t0.f, obsrisk.t1.f)
    
    
    #need to fit the model fit using bootstrap data to the original data
    x.f <- trtsel.boot( formula = formulas[[2]],
                        treatment.name = treatment.names[[2]], 
                        data = data2, 
                        d = d, 
                        study.design = study.design, 
                        rho = rho, 
                        link = "risks_provided", 
                        disc.rec.no.trt = disc.rec.no.trt2, 
                        provided_risk = provided_risk.f, 
                        prediction.time = prediction.times[[2]])
    
    
    x.f$derived.data$prediction.time <- prediction.times[[2]]
    sm2.f <- obe.get.summary.measures(x.f$derived.data, 
                                      event.name = event.names[[2]],
                                      treatment.name = treatment.names[[2]], 
                                      rho)
    
  }else{
    sm2.f <- NULL
  }
  
  
  
  if(is.null(coefs1)) coefs1 <- rep(0, 4)
  if(is.null(coefs2)) coefs2 <- rep(0, 4)
  
  c(unlist(coefs1)[1:4], unlist(coefs2)[1:4], 
    unlist(sm1.b), unlist(sm2.b), 
    unlist(sm1.f), unlist(sm2.f))



}
