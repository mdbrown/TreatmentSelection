trtsel.boot <-
function(event, trt, marker, d=0, rho = NULL, study.design, link, disc.marker.neg = NULL){
  
  # model.fit
  
  coef <- get.coef( event, trt, marker, study.design, rho, link) #glm object
  
  model.fit <- list( "coefficients" = coef,  "cohort.attributes" = rho, "study.design" = study.design, "link" = link)
  linkinvfun <- binomial(link = link)$linkinv
  
  # derived.data

  fittedrisk.t0 <- get.risk.t0(coef, marker, linkinvfun)
  fittedrisk.t1 <- get.risk.t1(coef, marker, linkinvfun)

  trt.effect <- fittedrisk.t0 - fittedrisk.t1
  
  if(is.null(disc.marker.neg)){
    marker.neg <- ifelse( trt.effect < d, 1, 0) # indicator of being marker negative
  }else{
    marker.neg <- as.numeric(marker==disc.marker.neg)
    
  }

  derived.data <- data.frame( event = event, trt=trt, fittedrisk.t0 = fittedrisk.t0, 
                                          fittedrisk.t1 = fittedrisk.t1,
                                              trt.effect = trt.effect,
                                             marker.neg = marker.neg)



  out <- list(derived.data=derived.data, model.fit = model.fit)

  
  out

}
