one.boot.plot <-
function(event, trt, marker, ci, rho = rho, study.design, obp.boot.sample, obp.get.F, fixed.values, fix.ind, out.ind, link, provided_risk = NULL){

  myboot.sample <- obp.boot.sample( event, trt, rho)
  #this makes it work for step function
  if(substr(ci, 1,1)=="h") addind = 0 
  else addind = 1
   

  rho.b <- myboot.sample[1:7]
  ind   <- myboot.sample[-c(1:7)]

  event.b  <- event[ind]
  trt.b  <- trt[ind]
  marker.b  <- marker[ind] 
  

  coef <- unname(get.coef(event.b, trt.b, marker.b, study.design, rho.b, link = link)[,1])

  linkinvfun <- binomial(link = link)$linkinv
  
  if(link == "risks_provided"){
    obsrisk.t0.b <- provided_risk[ind,1]
    obsrisk.t1.b <- provided_risk[ind,2]
  }else{
  obsrisk.t0.b  <-  get.risk.t0(coef,  marker.b, linkinvfun)
  obsrisk.t1.b  <-  get.risk.t1(coef,  marker.b, linkinvfun)
  }
  
  obsdelta.b <-obsrisk.t0.b - obsrisk.t1.b#

  F.Y <- obp.get.F( marker.b,        event.b, trt.b, rho.b)*100#
  F.D <- obp.get.F( obsdelta.b, event.b, trt.b, rho.b)*100#  

  #all 
  all  <- cbind( F.Y, obsrisk.t0.b, obsrisk.t1.b, F.D, obsdelta.b)
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
