trtsel.boot <-
function(formula, treatment.name, data, 
         d=0, 
         rho = NULL, study.design, link, disc.marker.neg = NULL, provided_risk = NULL){
  
  # model.fit

  coef <- get.coef( formula, treatment.name, data, study.design, rho, link) #glm object
  
  model.fit <- list( "coefficients" = coef, 
                     "cohort.attributes" = rho,
                     "study.design" = study.design, 
                     "link" = link)

  # derived.data
  if(link == "risks_provided"){
    fittedrisk.t0 <- provided_risk[,1]
    fittedrisk.t1 <- provided_risk[,2]
    linkinvfun <-NULL
    
  }else{
    linkinvfun <- binomial(link = link)$linkinv
    fittedrisk.t0 <- get.risk.t0(coef[,1], formula, treatment.name, data, linkinvfun)
    fittedrisk.t1 <- get.risk.t1(coef[,1], formula, treatment.name, data, linkinvfun)
  
  }

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
  
  event = data[[event.name]]
  trt = data[[treatment.name]]
  
  derived.data <- data.frame( data[,c(all.vars(formula))] ,
                              fittedrisk.t0 = fittedrisk.t0, 
                              fittedrisk.t1 = fittedrisk.t1,
                              trt.effect = trt.effect)

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
