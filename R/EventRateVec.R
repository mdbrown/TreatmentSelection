EventRateVec <- function(risk.t0, risk.t1, f.d, rho = NULL){
  #calculate event rate for v percentile of delta 
  # calculate this for all study designs, which we can tell based on the length of rho 
  browser()
  if(all(rho==-9999) | is.null(rho)){
    order_fd <- order(f.d)
    
    cs_riskT0 <- cumsum(risk.t0[order_fd])
    cs_riskT1 <- cumsum(risk.t1[order_fd]) #'reverse' cumsum
      
    plot(sort(f.d), cs_riskT0/seq_along(f.d), ylim = c(0,1))
    points(sort(f.d), cs_riskT1/seq_along(f.d), col = "red" )
    
    plot(f.d, risk.t0, ylim = c(0,1))
    points(f.d, risk.t1, col = "red" )
    
    out <- (cs_riskT0 + cs_riskT1)/length(f.d)
    
  

  }else if(all(rho[5:7]==-9999)){ #case cohort
    
    
  }else{ #stratified case cohort
  
    
  }
  
    out[rank(f.d)]
}