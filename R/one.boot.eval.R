one.boot.eval <-
function(data, formula, treatment.name, rho, study.design, obe.boot.sample, obe.get.summary.measures, link, d, disc.marker.neg = NULL, provided_risk = NULL, prediction.time = NULL, bbc){


  if( link == "time-to-event"){
    event.name = formula[[2]]
    mysurv <- with(data, eval(event.name))
    event <- mysurv[,2]
    stime <- mysurv[,1]
   
    data$prediction.time <- prediction.time
  }else{
    event.name <- as.character(formula[[2]])
    event <- data[[event.name]]
  }
  
  
  #  browser()
  sample <- obe.boot.sample( event = event, trt = data[[treatment.name]], rho = rho)
  rho.b <- sample[1:7]
  ind   <- sample[-c(1:7)]

 
  x.b <- trtsel.boot( formula = formula,
                      treatment.name = treatment.name, 
                      data = data[ind,], 
                      d = d, 
                      study.design = study.design, 
                      rho = rho.b, 
                      link = link, 
                      disc.marker.neg = disc.marker.neg, 
                      provided_risk = provided_risk[ind,], 
                      prediction.time = prediction.time)
  if(bbc){ 
    
    if(link == "time-to-event"){
      
      coxfit <- do.call(coxph, list(formula, data[ind,]))
      
      
      obsrisk.t0.f <- get.risk.t_coxph(coxfit, treatment.name, data, prediction.time, t = 0)
      obsrisk.t1.f  <- get.risk.t_coxph(coxfit, treatment.name, data, prediction.time, t = 1)
      #we still need to incorporate the nelson aalen baseline haz to get absolute risk at t = 'prediction.time'
 
    }else{
      
      coef <- unname(get.coef(formula,treatment.name, data[ind,], 
                              study.design, 
                              rho.b, 
                              link = link)[,1])
      
      linkinvfun <- binomial(link = link)$linkinv
      obsrisk.t0.f  <-  get.risk.t(coef, formula, treatment.name, data = data, linkinvfun, t = 0)
      obsrisk.t1.f  <-  get.risk.t(coef, formula, treatment.name, data = data, linkinvfun, t = 1)
      wi = 0
      
  
    }
    provided_risk.f <- cbind(obsrisk.t0.f, obsrisk.t1.f)
    
    
    #need to fit the model fit using bootstrap data to the original data
    x.f <- trtsel.boot( formula = formula,
                        treatment.name = treatment.name, 
                        data = data, 
                        d = d, 
                        study.design = study.design, 
                        rho = rho.b, 
                        link = "risks_provided", 
                        disc.marker.neg = disc.marker.neg, 
                        provided_risk = provided_risk.f, 
                        prediction.time = prediction.time)
    
    
    x.f$derived.data$prediction.time <- prediction.time
    sm.f <- obe.get.summary.measures(x.f$derived.data, 
                                     event.name = event.name,
                                     treatment.name = treatment.name, 
                                     rho.b)
    
  }else{
    sm.f <- NULL
  }
  
  if(is.null(data[["marker.neg"]])){
    x.b$derived.data$marker.neg <- 1- x.b$derived.data$marker.pos

  }
  
  coefs <- x.b$model$coefficients[,1]
  coefs <- c(coefs, c(0,0,0,0))
  coefs <- coefs[1:4]
  #sm = 'summary measures'
  
  x.b$derived.data$prediction.time <- prediction.time
  sm.b <- obe.get.summary.measures(x.b$derived.data, 
                                   event.name = event.name,
                                   treatment.name = treatment.name, 
                                   rho.b)
  
  
  
  

#  pdhat  <- sm.b$p.neg
#  neg    <- x.b$derived.data[ind,6] #marker neg or pos
#  marker.b <- data$marker[ind]
# thresh.b <- ifelse(pdhat > 0, max(marker.b[neg==1]), NA)


  #c(a3.b = a3.b, a1.b = a1.b, unlist(sm.b))
  if(is.null(coefs)) coefs <- rep(0, 4)
   c(unlist(coefs), unlist(sm.b), unlist(sm.f))#, thresh.b)

}
