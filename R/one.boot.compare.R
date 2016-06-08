one.boot.compare <-
function(data1, data2, formulas,  event.names, treatment.names,  rho, study.design, obe.boot.sample, obe.get.summary.measures, link, d, disc.marker.neg = NULL){


  myboot.sample <- obe.boot.sample( data1[[event.names[1]]], data1[[treatment.names[1]]], rho)

  rho.b <- myboot.sample[1:7]
  ind   <- myboot.sample[-c(1:7)]

  if(link == "risks_provided")
  {
     x1.b <- trtsel.boot( formula = formulas[[1]], treatment.name = treatment.names[1], data = data1[ind,],  
                          d = d, study.design = study.design, rho = rho.b, link = link, disc.marker.neg =disc.marker.neg, 
                          provided_risk = cbind(data1$fittedrisk.t0, data1$fittedrisk.t1)[ind,])
     coefs1 <- rep(0,4)
  }else{
     x1.b <- trtsel.boot( formula = formulas[[1]], treatment.name = treatment.names[1], data = data1[ind,],
                          d = d, study.design = study.design, rho = rho.b, link = link, disc.marker.neg =disc.marker.neg)
     coefs1 <- x1.b$model$coefficients[,1]
  }
  
  if(is.null(data1[["marker.neg"]])){
    x1.b$derived.data$marker.neg <- 1- x1.b$derived.data$marker.neg
    names(x1.b$derived.data)[6] <- "marker.pos"
    
  }
  
  sm1.b <- obe.get.summary.measures(x1.b$derived.data, event.names[1], treatment.names[1],  rho.b)

  if(link == "risks_provided")
  {
    x2.b <- trtsel.boot( formula = formulas[[2]], treatment.name = treatment.names[2], data = data2[ind,],  
                         d = d, study.design = study.design, rho = rho.b, link = link, disc.marker.neg =disc.marker.neg, 
                         provided_risk = cbind(data2$fittedrisk.t0, data2$fittedrisk.t1)[ind,])
    coefs2 <- rep(0, 4)
  }else{
    x2.b <- trtsel.boot( formula = formulas[[2]], treatment.name = treatment.names[2], data = data2[ind,], 
                         d = d, study.design = study.design, rho = rho.b, link = link, disc.marker.neg =disc.marker.neg)
    coefs2 <- x2.b$model$coefficients[,1]
  }
  
  if(is.null(data2[["marker.neg"]])){
    x2.b$derived.data$marker.neg <- 1- x2.b$derived.data$marker.neg
    names(x2.b$derived.data)[6] <- "marker.pos"
    
  }
  
  sm2.b <- obe.get.summary.measures(x2.b$derived.data, event.names[2], treatment.names[2], rho.b)

  c(unlist(coefs1), unlist(coefs2), unlist(sm1.b), unlist(sm2.b))



}
