trtsel.boot <-
function(formula, treatment.name, data, 
         d=0, 
         rho = NULL, study.design, link, 
         disc.rec.no.trt = NULL, provided_risk = NULL,
         prediction.time){
  
  
  # derived.data
  if(link == "risks_provided"){
    fittedrisk.t0 <- provided_risk[,1]
    fittedrisk.t1 <- provided_risk[,2]
    linkinvfun <-NULL
    
  }else if(link == "time-to-event"){
    
    coxfit <- do.call(coxph, list(formula,data))
    
    fittedrisk.t0 <- get.risk.t_coxph(coxfit, treatment.name, data, prediction.time, t = 0)
    fittedrisk.t1 <- get.risk.t_coxph(coxfit, treatment.name, data, prediction.time, t = 1)
    #we still need to incorporate the nelson aalen baseline haz to get absolute risk at t = 'prediction.time'
    coef <- summary(coxfit)$coefficients
    
  }else{
    # model.fit
    coef <- get.coef( formula, treatment.name, data, study.design, rho, link) #glm object
    
    linkinvfun <- binomial(link = link)$linkinv
    fittedrisk.t0 <- get.risk.t(coef[,1], formula, treatment.name, data, linkinvfun, t = 0)
    fittedrisk.t1 <- get.risk.t(coef[,1], formula, treatment.name, data, linkinvfun, t = 1)
  
  }
  
  model.fit <- list( "coefficients" = coef, 
                     "cohort.attributes" = rho,
                     "study.design" = study.design, 
                     "link" = link)
  trt <- data[[treatment.name]]
  trt.effect <- fittedrisk.t0 - fittedrisk.t1
  
  
  event.name = as.character(formula[[2]])
  marker.names = all.vars(formula)[!is.element( all.vars(formula), c(event.name, treatment.name)) ]
  if(length(marker.names)==1){ 
    marker = data[[marker.names]] 
  }else{
    marker = NULL
  }
  
  
  if(is.null(disc.rec.no.trt)){
    rec.no.trt <- ifelse( trt.effect < d, 1, 0) # indicator of being marker negative
  }else{
    rec.no.trt <- as.numeric(marker==disc.rec.no.trt)
  }



  derived.data <- data.frame( data[,c(all.vars(formula))] ,
                              fittedrisk.t0 = fittedrisk.t0, 
                              fittedrisk.t1 = fittedrisk.t1,
                              trt.effect = trt.effect)

  if(link == "time-to-event"){
    
    tmp <- with(data, eval(formula[[2]]))
    
    wi = numeric(nrow(data))
    
    wi[trt==1] <- get.censoring.weights(ti = prediction.time, stime = tmp[trt==1,1], status = tmp[trt==1,2] )
    wi[trt==0] <- get.censoring.weights(ti = prediction.time, stime = tmp[trt==0,1], status = tmp[trt==0,2] )
    
    derived.data$censoring.weights  <- wi
  }
  
  
  if(is.null(data$rec.no.trt)){
    rec.trt <- 1-rec.no.trt # indicator of being marker negative
    derived.data$rec.trt <- rec.trt 
  
  }else{
    derived.data$rec.no.trt <- rec.no.trt
    
 }
  


  out <- list(derived.data=derived.data, 
              model.fit = model.fit)

  
  out

}
