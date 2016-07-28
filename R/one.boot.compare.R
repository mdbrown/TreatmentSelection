one.boot.compare <-
function(data1, data2, formulas,  event.names, treatment.names,  rho, study.design, obe.boot.sample, obe.get.summary.measures, link, d, disc.marker.neg1, disc.marker.neg2,  prediction.times){


  
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

  if(link == "risks_provided")
  {
     x1.b <- trtsel.boot( formula = formulas[[1]], treatment.name = treatment.names[1], data = data1[ind,],  
                          d = d, study.design = study.design, rho = rho.b, link = link, disc.marker.neg =disc.marker.neg1, 
                          provided_risk = cbind(data1$fittedrisk.t0, data1$fittedrisk.t1)[ind,], 
                          prediction.time = prediction.times[[1]])
     coefs1 <- rep(0,4)
  }else{
     x1.b <- trtsel.boot( formula = formulas[[1]], treatment.name = treatment.names[1], data = data1[ind,],
                          d = d, study.design = study.design, rho = rho.b, link = link, disc.marker.neg =disc.marker.neg1, 
                          prediction.time = prediction.times[[1]])
     coefs1 <- x1.b$model$coefficients[,1]
     coefs1 <- c(coefs1, 0,0,0,0)[1:4]
  }
  
  if(is.null(data1[["marker.neg"]])){
    x1.b$derived.data$marker.neg <- 1- x1.b$derived.data$marker.neg
    names(x1.b$derived.data)[6] <- "marker.pos"
    
  }
  
  if(link == "time-to-event") x1.b$derived.data$prediction.time = prediction.times[[1]]
  sm1.b <- obe.get.summary.measures(x1.b$derived.data, event.names[[1]], treatment.names[1],  rho.b)

  if(link == "risks_provided")
  {
    x2.b <- trtsel.boot( formula = formulas[[2]], treatment.name = treatment.names[2], data = data2[ind,],  
                         d = d, study.design = study.design, rho = rho.b, link = link, disc.marker.neg =disc.marker.neg2, 
                         provided_risk = cbind(data2$fittedrisk.t0, data2$fittedrisk.t1)[ind,], 
                         prediction.time = prediction.times[[2]])
    coefs2 <- rep(0, 4)
  }else{
    x2.b <- trtsel.boot( formula = formulas[[2]], treatment.name = treatment.names[2], data = data2[ind,], 
                         d = d, study.design = study.design, rho = rho.b, link = link, disc.marker.neg =disc.marker.neg2, 
                         prediction.time = prediction.times[[2]])
    coefs2 <- x2.b$model$coefficients[,1]
    coefs1 <- c(coefs1, 0,0,0,0)[1:4]
  }
  
  if(is.null(data2[["marker.neg"]])){
    x2.b$derived.data$marker.neg <- 1- x2.b$derived.data$marker.neg
    names(x2.b$derived.data)[6] <- "marker.pos"
    
  }
  if(link == "time-to-event") x2.b$derived.data$prediction.time = prediction.times[[1]]
  sm2.b <- obe.get.summary.measures(x2.b$derived.data, event.names[[2]], treatment.names[2], rho.b)

  c(unlist(coefs1), unlist(coefs2), unlist(sm1.b), unlist(sm2.b))



}
