get.summary.measures.cohort <-
  function(data, event.name, treatment.name, rho, d=0){  
  
    event = data[[event.name]]
    trt = data[[treatment.name]]
    trt.effect <- data$trt.effect
    
    if(is.null(data[["marker.pos"]])){
      neg <- data$marker.neg
      pos <- 1 - neg
    }else{
      pos <- data$marker.pos
      neg <- 1 - pos
    }
    #proportion marker negative
    p.marker.neg <- mean(neg)
    p.marker.pos <- mean(pos)
    #Average Benefit (B) of no treatment among marker negatives...
    
    #empirical estimate
    B.neg.emp <- ifelse(length(event[trt==0 & neg==1]) > 0 & length(event[trt==1 & neg==1]) > 0, 
                        mean(event[trt==1 & neg==1]) - mean(event[trt==0 & neg==1]) ,
                        0)
    
    #model based estimate
    B.neg.mod <- ifelse(sum(neg) >0, -mean(trt.effect[neg==1]), 0)
    
    #Average Benefit (B) of treatment among marker positives...
    
    #empirical estimate
    B.pos.emp <- ifelse(length(event[trt==0 & pos==1]) > 0 & length(event[trt==1 & pos==1]) > 0, 
                        mean(event[trt==0 & pos==1]) - mean(event[trt==1 & pos==1]), 
                        0)
    
    #model based estimate
    B.pos.mod <- ifelse(sum(pos)>0, mean(trt.effect[pos==1]), 0)
    
    #Theta
    
    if(is.null(data[["marker.pos"]])){
      Theta.emp <- B.neg.emp*p.marker.neg
      Theta.mod <- B.neg.mod*p.marker.neg
      
    }else{
      Theta.emp <- B.pos.emp*p.marker.pos
      Theta.mod <- B.pos.mod*p.marker.pos
    }
    
    p0.hat <- mean(event[trt==0])
    p1.hat <- mean(event[trt==1])
    Var.Delta <- mean((trt.effect - (p0.hat - p1.hat))^2)
    
    delta.F <- get.F.cohort( trt.effect, event, trt, rho = NULL)
    
    ooo <- order(trt.effect)
    
    s.delta.F <- delta.F[ooo]
    
    TG <- sum( diff(c(0, s.delta.F))*abs( sort(trt.effect) - (p0.hat - p1.hat))) 
    
    #event rates
    ER.trt0.emp = mean(event[trt==0])
    ER.trt0.mod = mean(data$fittedrisk.t0)
    ER.trt1.emp = mean(event[trt==1])
    ER.trt1.mod = mean(data$fittedrisk.t1)
    
    if(is.null(data[["marker.pos"]])){
      #default is trt all
      ER.mkrbased.emp = ER.trt1.emp - Theta.emp 
      ER.mkrbased.mod = ER.trt1.mod - Theta.mod
    }else{
      ER.mkrbased.emp = ER.trt0.emp - Theta.emp 
      ER.mkrbased.mod = ER.trt0.mod - Theta.mod    
    }
    
    
    list(     p.neg = p.marker.neg,
              p.pos = p.marker.pos,
              B.neg.emp = B.neg.emp,
              B.neg.mod = B.neg.mod, 
              B.pos.emp = B.pos.emp, 
              B.pos.mod = B.pos.mod, 
              Theta.emp = Theta.emp, 
              Theta.mod = Theta.mod, 
              Var.Delta = Var.Delta, 
              TG        = TG, 
              ER.trt0.emp = ER.trt0.emp, 
              ER.trt0.mod = ER.trt0.mod, 
              ER.trt1.emp = ER.trt1.emp, 
              ER.trt1.mod = ER.trt1.mod, 
              ER.mkrbased.emp = ER.mkrbased.emp,
              ER.mkrbased.mod = ER.mkrbased.mod
    ) 
    
    
    
  }

####################

get.summary.measures.cohort.survival <-
  function(data, event.name, treatment.name, rho, d=0){  
   
    
     t0 <- data$prediction.time[1]
    #event = data[[event.name]]
    trt = data[[treatment.name]]
    trt.effect <- data$trt.effect
    wi <- data$censoring.weights 
    
  
    tmp <- with(data, eval(event.name))
    stime = tmp[,1]
    status = tmp[,2] 
    
    
    if(is.null(data[["marker.pos"]])){
      neg <- data$marker.neg
      pos <- 1 - neg
    }else{
      pos <- data$marker.pos
      neg <- 1 - pos
    }
    #proportion marker negative
    p.marker.neg <- mean(neg)
    p.marker.pos <- mean(pos)
    #Average Benefit (B) of no treatment among marker negatives...
    
    #empirical estimate
    num <- (wi*I(stime < t0)*I(neg)); 
    den <- (wi*I(neg))

   
    if(sum(den[trt==0])==0 | sum(den[trt==1]) ==0) {
      B.neg.emp <- 0
      ER.neg.emp <- 0 
      #print("Bneg.emp is set to zero")
    }else{
      B.neg.emp <- sum(num[trt==1])/sum(den[trt==1]) - sum(num[trt==0])/sum(den[trt==0])
      ER.neg.emp <- sum(num[trt==0])/sum(den[trt==0])
    }

    
    #model based estimate
    B.neg.mod <- ifelse(sum(neg) > 0, -mean(trt.effect[neg==1]), 0)
    ER.neg.mod <- ifelse(sum(neg) >0, mean(data$fittedrisk.t0[neg==1]), 0)
    
    
    #Average Benefit (B) of treatment among marker positives...
    

    
    #empirical estimate
    num <- (wi*I(stime < t0)*pos); 
    den <- (wi*pos)
    if(sum(den[trt==0])==0 | sum(den[trt==1]) ==0) {
      B.pos.emp <- 0
      ER.pos.emp <- 0 
      # print("Bpos.emp is set to zero")
    }else{
      B.pos.emp <- sum(num[trt==0])/sum(den[trt==0]) - sum(num[trt==1])/sum(den[trt==1])
      ER.pos.emp <-  sum(num[trt==1])/sum(den[trt==1])
    }

    #model based estimate
    B.pos.mod <- ifelse(sum(pos)>0, mean(trt.effect[pos==1]), 0)
    ER.pos.mod <- ifelse(sum(pos) >0, mean(data$fittedrisk.t1[pos==1]), 0)
    
    
    #Theta
    
    if(is.null(data[["marker.pos"]])){
      Theta.emp <- B.neg.emp*p.marker.neg
      Theta.mod <- B.neg.mod*p.marker.neg
      
    }else{
      Theta.emp <- B.pos.emp*p.marker.pos
      Theta.mod <- B.pos.mod*p.marker.pos
    }
    
    
    #mod
    p0.hat <- sum(wi*I(stime < t0)*(1-trt))/sum(wi*(1-trt));#mean(risk.no.trt) #mean(event[trt==0])
    p1.hat <- sum(wi*I(stime < t0)*trt)/sum(wi*trt); #mean(risk.trt)
    
    Var.Delta <- mean((trt.effect - (p0.hat - p1.hat))^2)
    event = 0 
    delta.F <- get.F.cohort( trt.effect, event, trt, rho = NULL)
    
    ooo <- order(trt.effect)
    
    s.delta.F <- delta.F[ooo]
    
    TG <- sum( diff(c(0, s.delta.F))*abs( sort(trt.effect) - (p0.hat - p1.hat))) 

    ER.trt0.emp = p0.hat
    ER.trt0.mod = mean(data$fittedrisk.t0)
    ER.trt1.emp = p1.hat
    ER.trt1.mod = mean(data$fittedrisk.t1)
    

      ER.mkrbased.emp = ER.pos.emp*p.marker.pos + ER.neg.emp*p.marker.neg 
      ER.mkrbased.mod = ER.pos.mod*p.marker.pos + ER.neg.mod*p.marker.neg

    
    list(     p.neg = p.marker.neg,
              p.pos = p.marker.pos,
              B.neg.emp = B.neg.emp,
              B.neg.mod = B.neg.mod, 
              B.pos.emp = B.pos.emp, 
              B.pos.mod = B.pos.mod, 
              Theta.emp = Theta.emp, 
              Theta.mod = Theta.mod, 
              Var.Delta = Var.Delta, 
              TG        = TG, 
              ER.trt0.emp = ER.trt0.emp, 
              ER.trt0.mod = ER.trt0.mod, 
              ER.trt1.emp = ER.trt1.emp, 
              ER.trt1.mod = ER.trt1.mod, 
              ER.mkrbased.emp = ER.mkrbased.emp,
              ER.mkrbased.mod = ER.mkrbased.mod
    ) 
    
    
    
  }



####################


get.summary.measures.stratified.case.control <-
  function(data,  event.name, treatment.name, rho, d=0){  
    
    event = data[[event.name]]
    trt = data[[treatment.name]]
    trt.effect <- data$trt.effect
    
    if(is.null(data[["marker.pos"]])){
      neg <- data$marker.neg
      pos <- 1 - neg
    }else{
      pos <- data$marker.pos
      neg <- 1 - pos
    }
    
    
    # get probabilites for all strata based on rho1 and rho2
    Pr.D1.trt1 <- rho[5]
    Pr.D0.trt1 <- rho[4]
    Pr.D1.trt0 <- rho[3]
    Pr.D0.trt0 <- rho[2]
    
    Pr.D1.givT0 <- rho[3]/(rho[2]+rho[3])
    Pr.D1.givT1 <- rho[5]/(rho[4]+rho[5])
    #proportion marker negative
    p.marker.neg <- mean(neg[event==1 & trt==1])*Pr.D1.trt1 + 
      mean(neg[event==0 & trt==1])*Pr.D0.trt1 +
      mean(neg[event==1 & trt==0])*Pr.D1.trt0 +
      mean(neg[event==0 & trt==0])*Pr.D0.trt0
    
    p.marker.pos <- mean(pos[event==1 & trt==1])*Pr.D1.trt1 + 
      mean(pos[event==0 & trt==1])*Pr.D0.trt1 +
      mean(pos[event==1 & trt==0])*Pr.D1.trt0 +
      mean(pos[event==0 & trt==0])*Pr.D0.trt0
    
    #Average Benefit (B) of no treatment among marker negatives...
    
    #empirical estimate
    #cat(paste("\n", mean(event[trt==0 & neg ==1])))
    #if(!is.finite(mean(event[trt==0 & neg ==1]))) browser()
    B.neg.emp.trt0 <- ifelse(sum(trt==0 & neg==1)>0, expit( logit(mean(event[trt==0 & neg ==1])) + logit(Pr.D1.givT0) - logit(mean(event[trt==0])) ), 0)
    B.neg.emp.trt1 <- ifelse(sum(trt==1 & neg==1)>0, expit( logit(mean(event[trt==1 & neg ==1])) + logit(Pr.D1.givT1) - logit(mean(event[trt==1])) ), 0)
    
    B.neg.emp <- B.neg.emp.trt1 - B.neg.emp.trt0
    
    #model based estimate
    n.11 <- sum((event==1)*(trt==1))
    n.01 <- sum((event==0)*(trt==1))
    n.10 <- sum((event==1)*(trt==0))
    n.00 <- sum((event==0)*(trt==0))
    
    #numerator
    B.neg.mod.num <- (1/n.11)*Pr.D1.trt1*ifelse(sum(neg==1 & event==1 & trt==1)>0, sum(-trt.effect[neg==1 & event==1 & trt==1]), 0) + 
      (1/n.01)*Pr.D0.trt1*ifelse(sum(neg==1 & event==0 & trt==1)>0, sum(-trt.effect[neg==1 & event==0 & trt==1]), 0) + 
      (1/n.10)*Pr.D1.trt0*ifelse(sum(neg==1 & event==1 & trt==0)>0, sum(-trt.effect[neg==1 & event==1 & trt==0]), 0) + 
      (1/n.00)*Pr.D0.trt0*ifelse(sum(neg==1 & event==0 & trt==0)>0, sum(-trt.effect[neg==1 & event==0 & trt==0]), 0) 
    
    #denominator
    B.neg.mod.den <- (1/n.11)*Pr.D1.trt1*sum(neg[event==1 & trt==1]) + 
      (1/n.01)*Pr.D0.trt1*sum(neg[event==0 & trt==1]) + 
      (1/n.10)*Pr.D1.trt0*sum(neg[event==1 & trt==0]) + 
      (1/n.00)*Pr.D0.trt0*sum(neg[event==0 & trt==0])    
    
    B.neg.mod <- ifelse(B.neg.mod.den>0, B.neg.mod.num/B.neg.mod.den, 0)
    
    #Average Benefit (B) of treatment among marker positives...
    #empirical estimate
    
    B.pos.emp.trt0 <- ifelse(sum(trt==0 & pos==1)>0, expit( logit(mean(event[trt==0 & pos ==1])) + logit(Pr.D1.givT0) - logit(mean(event[trt==0])) ), 0)
    B.pos.emp.trt1 <- ifelse(sum(trt==1 & pos==1)>0, expit( logit(mean(event[trt==1 & pos ==1])) + logit(Pr.D1.givT1) - logit(mean(event[trt==1])) ), 0)
    
    B.pos.emp <- B.pos.emp.trt0 - B.pos.emp.trt1
    
    #model based estimate
    n.11 <- sum((event==1)*(trt==1))
    n.01 <- sum((event==0)*(trt==1))
    n.10 <- sum((event==1)*(trt==0))
    n.00 <- sum((event==0)*(trt==0))
    
    #numerator
    B.pos.mod.num <- (1/n.11)*Pr.D1.trt1*ifelse(sum(pos==1 & event==1 & trt==1)>0, sum(trt.effect[pos==1 & event==1 & trt==1]), 0) + 
      (1/n.01)*Pr.D0.trt1*ifelse(sum(pos==1 & event==0 & trt==1)>0, sum(trt.effect[pos==1 & event==0 & trt==1]), 0) + 
      (1/n.10)*Pr.D1.trt0*ifelse(sum(pos==1 & event==1 & trt==0)>0, sum(trt.effect[pos==1 & event==1 & trt==0]), 0) + 
      (1/n.00)*Pr.D0.trt0*ifelse(sum(pos==1 & event==0 & trt==0)>0, sum(trt.effect[pos==1 & event==0 & trt==0]), 0) 
    
    #denominator
    B.pos.mod.den <- (1/n.11)*Pr.D1.trt1*sum(pos[event==1 & trt==1]) + 
      (1/n.01)*Pr.D0.trt1*sum(pos[event==0 & trt==1]) + 
      (1/n.10)*Pr.D1.trt0*sum(pos[event==1 & trt==0]) + 
      (1/n.00)*Pr.D0.trt0*sum(pos[event==0 & trt==0])    
    
    B.pos.mod <- ifelse(B.pos.mod.den>0, B.pos.mod.num/B.pos.mod.den, 0)
    
    
    #Theta
    
    
    if(is.null(data[["marker.pos"]])){
      Theta.emp <- B.neg.emp*p.marker.neg
      Theta.mod <- B.neg.mod*p.marker.neg
      
    }else{
      Theta.emp <- B.pos.emp*p.marker.pos
      Theta.mod <- B.pos.mod*p.marker.pos
    }
    
    
    # Variance of Trt Effects
    
    # calculate sample weights 
    n.cc <- length(event)
    Pr.S <- n.cc/rho[1] # Pr(S=1)
    
    #wi <- rep(0, length(event))
    
    #wi[event==1 & trt==1] <- Pr.D1.trt1/(mean(event==1 & trt==1)*Pr.S)
    #wi[event==1 & trt==0] <- Pr.D1.trt0/(mean(event==1 & trt==0)*Pr.S)              
    #wi[event==0 & trt==1] <- Pr.D0.trt1/(mean(event==0 & trt==1)*Pr.S)
    #wi[event==0 & trt==0] <- Pr.D0.trt0/(mean(event==0 & trt==0)*Pr.S)
    
    #if(sum(wi > 25) > 0 ) { warning("Warning: there exist sampling weights > 25, Variance Estimate may be unstable")}
    
    
    
    #sum((wi*(trt.effect-d)^2))/sum(wi)
    
    
    # mean(trt.effect[event==1 & trt==1]^2)*Pr.D1.trt1 + 
    # mean(trt.effect[event==0 & trt==1]^2)*Pr.D0.trt1 +
    # mean(trt.effect[event==1 & trt==0]^2)*Pr.D1.trt0 +
    # mean(trt.effect[event==0 & trt==0]^2)*Pr.D0.trt0 -
    # (mean(trt.effect[event==1 & trt==1])*Pr.D1.trt1 + 
    # mean(trt.effect[event==0 & trt==1])*Pr.D0.trt1 +
    # mean(trt.effect[event==1 & trt==0])*Pr.D1.trt0 +
    # mean(trt.effect[event==0 & trt==0])*Pr.D0.trt0)^2 #sum((wi*(trt.effect-d)^2))/sum(wi)
    
    #Total Gain (TG)
    
    delta.F <- get.F.stratified.case.control( trt.effect, event, trt, rho = rho)
    
    ooo <- order(trt.effect)
    
    s.delta.F <- delta.F[ooo]
    
    TG <- sum( diff(c(0, s.delta.F))*abs( sort(trt.effect) - (Pr.D1.givT0 - Pr.D1.givT1) ) )
    
    #event rates
    ER.trt0.emp = ifelse(sum(trt==0)>0, expit( logit(mean(event[trt==0])) + logit(Pr.D1.givT0) - logit(mean(event[trt==0])) ), 0)
    ER.trt0.mod = mean(data$fittedrisk.t0[event==1 & trt==1])*Pr.D1.trt1 + 
      mean(data$fittedrisk.t0[event==0 & trt==1])*Pr.D0.trt1 +
      mean(data$fittedrisk.t0[event==1 & trt==0])*Pr.D1.trt0 +
      mean(data$fittedrisk.t0[event==0 & trt==0])*Pr.D0.trt0
    
    ER.trt1.emp = ifelse(sum(trt==1)>0, expit( logit(mean(event[trt==1])) + logit(Pr.D1.givT1) - logit(mean(event[trt==1])) ), 0)
    ER.trt1.mod = mean(data$fittedrisk.t1[event==1 & trt==1])*Pr.D1.trt1 + 
      mean(data$fittedrisk.t1[event==0 & trt==1])*Pr.D0.trt1 +
      mean(data$fittedrisk.t1[event==1 & trt==0])*Pr.D1.trt0 +
      mean(data$fittedrisk.t1[event==0 & trt==0])*Pr.D0.trt0
    
    if(is.null(data[["marker.pos"]])){
      #default is trt all
      ER.mkrbased.emp = ER.trt1.emp - Theta.emp 
      ER.mkrbased.mod = ER.trt1.mod - Theta.mod
    }else{
      ER.mkrbased.emp = ER.trt0.emp - Theta.emp 
      ER.mkrbased.mod = ER.trt0.mod - Theta.mod    
    }
    
    
    Var.Delta =   mean(trt.effect[event==1 & trt==1]^2)*Pr.D1.trt1 + 
      mean(trt.effect[event==0 & trt==1]^2)*Pr.D0.trt1 +
      mean(trt.effect[event==1 & trt==0]^2)*Pr.D1.trt0 +
      mean(trt.effect[event==0 & trt==0]^2)*Pr.D0.trt0 -(ER.trt0.emp - ER.trt1.emp)^2
    
    
    list(     p.neg = p.marker.neg,
              p.pos = p.marker.pos,
              B.neg.emp = B.neg.emp,
              B.neg.mod = B.neg.mod, 
              B.pos.emp = B.pos.emp, 
              B.pos.mod = B.pos.mod, 
              Theta.emp = Theta.emp, 
              Theta.mod = Theta.mod, 
              Var.Delta = Var.Delta, 
              TG        = TG, 
              ER.trt0.emp = ER.trt0.emp, 
              ER.trt0.mod = ER.trt0.mod, 
              ER.trt1.emp = ER.trt1.emp, 
              ER.trt1.mod = ER.trt1.mod, 
              ER.mkrbased.emp = ER.mkrbased.emp,
              ER.mkrbased.mod = ER.mkrbased.mod
    ) 
    
    
  }

#########################

get.summary.measures.case.control <-
  function(data,  event.name, treatment.name, rho, d=0){  
    #rho <- rho[1]
    event = data[[event.name]]
    trt = data[[treatment.name]]
    trt.effect <- data$trt.effect
    
    if(is.null(data[["marker.pos"]])){
      neg <- data$marker.neg
      pos <- 1 - neg
    }else{
      pos <- data$marker.pos
      neg <- 1 - pos
    }
    #proportion marker negative
    p.marker.neg <- mean(neg[event==1])*rho[3] + mean(neg[event==0])*(1-rho[3])
    p.marker.pos<- mean(pos[event==1])*rho[3] + mean(pos[event==0])*(1-rho[3])
    
    #Average Benefit (B) of no treatment among marker negatives...
    
    #empirical estimate
    
    B.neg.emp.trt0 <-  ifelse( sum(event[trt==0 & neg==1])>0, expit( logit(mean(event[trt==0 & neg==1])) + logit(rho[3]) - logit(mean(event)) ), 0)
    B.neg.emp.trt1 <-  ifelse( sum(event[trt==1 & neg==1])>0, expit( logit(mean(event[trt==1 & neg==1])) + logit(rho[3]) - logit(mean(event)) ), 0) 
    
    B.neg.emp <- B.neg.emp.trt1 - B.neg.emp.trt0
    
    #model based estimate
    n.r   <- sum(event)
    n.nor <- sum(1-event)
    #numerator
    B.neg.mod.num <-     (1/n.r)*(rho[3])*ifelse(sum(neg==1 & event==1)>0, sum(-trt.effect[neg==1 & event==1]), 0) + 
      (1/n.nor)*(1-rho[3])*ifelse(sum(neg==1 & event==0)>0, sum(-trt.effect[neg==1 & event==0]), 0)
    #denominator
    B.neg.mod.den <-     (1/n.r)*(rho[3])*sum(neg[event==1]) + 
      (1/n.nor)*(1-rho[3])*sum(neg[event==0])
    
    B.neg.mod <- ifelse(B.neg.mod.den>0, B.neg.mod.num/B.neg.mod.den, 0)
    
    #Average Benefit (B) of treatment among marker positives...
    
    #empirical estimate
    B.pos.emp.trt0 <-  ifelse( sum(event[trt==0 & pos==1])>0, expit( logit(mean(event[trt==0 & pos==1])) + logit(rho[3]) - logit(mean(event)) ), 0)
    B.pos.emp.trt1 <-  ifelse( sum(event[trt==1 & pos==1])>0, expit( logit(mean(event[trt==1 & pos==1])) + logit(rho[3]) - logit(mean(event)) ), 0) 
    
    B.pos.emp <- B.pos.emp.trt0 - B.pos.emp.trt1
    
    #model based estimate
    #numerator
    B.pos.mod.num <-     (1/n.r)*(rho[3])*ifelse(sum(pos==1 & event==1)>0, sum(trt.effect[pos==1 & event==1]), 0) + 
      (1/n.nor)*(1-rho[3])*ifelse(sum(pos==1 & event==0)>0, sum(trt.effect[pos==1 & event==0]), 0)
    #denominator
    B.pos.mod.den <-     (1/n.r)*(rho[3])*sum(pos[event==1]) +
      (1/n.nor)*(1-rho[3])*sum(pos[event==0])
    
    B.pos.mod <- ifelse(B.pos.mod.den>0, B.pos.mod.num/B.pos.mod.den, 0)
    
    #Theta
    
    if(is.null(data[["marker.pos"]])){
      Theta.emp <- B.neg.emp*p.marker.neg
      Theta.mod <- B.neg.mod*p.marker.neg
      
    }else{
      Theta.emp <- B.pos.emp*p.marker.pos
      Theta.mod <- B.pos.mod*p.marker.pos
    }
    
    # Variance of Trt Effects
    
    # calculate sample weights 
    n.cc <- length(event)
    Pr.S <- n.cc/rho[1] # Pr(S=1)
    #wi <- rep(0, length(event))
    #wi[event==1] <- rho[1]/(mean(event ==1)*Pr.S)
    #wi[event==0] <- (1-rho[1])/(mean(event==0)*Pr.S)  
    
    #if(sum(wi > 25) > 0 ) { warning("Warning: there exist sampling weights > 25, Variance Estimate may be unstable")}
    
    
    #  Var.Delta = mean((trt.effect[event==1])^2)*rho[3] + mean((trt.effect[event==0])^2)*(1-rho[3]) 
    
    # mean(trt.effect[event==1]^2)*rho[3] + mean(trt.effect[event==0]^2)*(1-rho[3]) - 
    #(mean(trt.effect[event==1])*rho[3]   + mean(trt.effect[event==0])*(1-rho[3]))^2  #sum((wi*(trt.effect- d )^2))/sum(wi)
    
    #Total Gain (TG)
    
    
    delta.F <- get.F.case.control( trt.effect, event, trt, rho = rho)
    
    ooo <- order(trt.effect)
    
    s.delta.F <- delta.F[ooo]
    p0 <- ((mean(1-trt[event==1]))*rho[3])/(1-rho[2])
    p1 <- ((mean(trt[event==1]))*rho[3])/(rho[2])
    TG <- sum( diff(c(0, s.delta.F))*abs( sort(trt.effect) - (p0 - p1) ))
    
    #event rates
    ER.trt0.emp = ifelse( sum(trt==0)>0, expit( logit(mean(event[trt==0])) + logit(rho[3]) - logit(mean(event)) ), 0)
    ER.trt0.mod = mean(data$fittedrisk.t0[event==1])*rho[3] + mean(data$fittedrisk.t0[event==0])*(1-rho[3])
    
    ER.trt1.emp = ifelse( sum(trt==1)>0, expit( logit(mean(event[trt==1])) + logit(rho[3]) - logit(mean(event)) ), 0)
    ER.trt1.mod = mean(data$fittedrisk.t1[event==1])*rho[3] + mean(data$fittedrisk.t1[event==0])*(1-rho[3])
    
    if(is.null(data[["marker.pos"]])){
      #default is trt all
      ER.mkrbased.emp = ER.trt1.emp - Theta.emp 
      ER.mkrbased.mod = ER.trt1.mod - Theta.mod
    }else{
      ER.mkrbased.emp = ER.trt0.emp - Theta.emp 
      ER.mkrbased.mod = ER.trt0.mod - Theta.mod    
    }
    
    
    
    Var.Delta = mean((trt.effect[event==1])^2)*rho[3] + mean((trt.effect[event==0])^2)*(1-rho[3]) - (ER.trt0.emp - ER.trt1.emp)^2
    
    
    list(     p.neg = p.marker.neg,
              p.pos = p.marker.pos,
              B.neg.emp = B.neg.emp,
              B.neg.mod = B.neg.mod, 
              B.pos.emp = B.pos.emp, 
              B.pos.mod = B.pos.mod, 
              Theta.emp = Theta.emp, 
              Theta.mod = Theta.mod, 
              Var.Delta = Var.Delta, 
              TG        = TG, 
              ER.trt0.emp = ER.trt0.emp, 
              ER.trt0.mod = ER.trt0.mod, 
              ER.trt1.emp = ER.trt1.emp, 
              ER.trt1.mod = ER.trt1.mod, 
              ER.mkrbased.emp = ER.mkrbased.emp,
              ER.mkrbased.mod = ER.mkrbased.mod
    ) 
    
    
    
    
  }

#########################