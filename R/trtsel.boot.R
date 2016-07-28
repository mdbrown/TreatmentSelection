trtsel.boot <-
function(formula, treatment.name, data, 
         d=0, 
         rho = NULL, study.design, link, 
         disc.marker.neg = NULL, provided_risk = NULL,
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

  trt.effect <- fittedrisk.t0 - fittedrisk.t1
  
  if(is.null(disc.marker.neg)){
    marker.neg <- ifelse( trt.effect < d, 1, 0) # indicator of being marker negative
  }else{
    marker.neg <- as.numeric(data$marker==disc.marker.neg)
  }

  event.name = as.character(formula[[2]])
  marker.names = all.vars(formula)[!is.element( all.vars(formula), c(event.name, treatment.name)) ]
  if(length(marker.names)==1){ 
    marker = data[[marker.names]] 
  }else{
    marker = NULL
  }
  

  derived.data <- data.frame( data[,c(all.vars(formula))] ,
                              fittedrisk.t0 = fittedrisk.t0, 
                              fittedrisk.t1 = fittedrisk.t1,
                              trt.effect = trt.effect)

  if(link == "time-to-event"){
    
    tmp <- with(data, eval(formula[[2]]))
    wi = get.censoring.weights(ti = prediction.time, stime = tmp[,1], status = tmp[,2] )
    derived.data$censoring.weights  <- wi
  }
  
  derived.data$marker <- marker
  if(is.null(data$marker.neg)){
    marker.pos <- 1-marker.neg # indicator of being marker negative
    derived.data$marker.pos <- marker.pos = marker.pos
  
  }else{
    derived.data$marker.neg <- marker.neg
    
 }
  


  out <- list(derived.data=derived.data, 
              model.fit = model.fit)

  
  out

}
