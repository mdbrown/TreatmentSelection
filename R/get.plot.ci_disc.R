get.plot.ci_disc <-
function( marker, trt, event, rho = rho, plot.type, ci, bootstraps, obp.boot.sample, obp.get.F,  alpha){
  ##first we need to look at what is fixed and what needs to vary to get the ci's we want
  ## I am just storing some index variables that make me able to calculate the 
  ## proper ci's later.  

  #the data will be set up as 
  # F.marker, risk_t0, risk_t1 (all sorted by F.marker), F.event, obs delta (sorted by F.event)

  one.boot.plot_disc <-
    function(event, trt, marker, rho = rho,  obp.boot.sample, obp.get.F){
      
      myboot.sample <- obp.boot.sample( event, trt, rho)
      
      rho.b <- myboot.sample[1:7]
      ind   <- myboot.sample[-c(1:7)]
      
      event.b  <- event[ind]
      trt.b  <- trt[ind]
      marker.b  <- marker[ind] 
      Fy = obp.get.F(marker.b, event.b, trt.b, rho.b, return.fun = TRUE)
      mval <- sort(unique(marker))
      c(   event.trt0.mkr0 = mean(event.b[trt.b==0 & marker.b ==mval[1]]), 
           event.trt1.mkr0 = mean(event.b[trt.b==1 & marker.b ==mval[1]]), 
           event.trt0.mkr1 = mean(event.b[trt.b==0 & marker.b ==mval[2]]),
           event.trt1.mkr1 = mean(event.b[trt.b==1 & marker.b ==mval[2]]), 
           mean.trt0.mkr0 = Fy(mval[1]), 
           mean.trt1.mkr0 = Fy(mval[1]), 
           mean.trt0.mkr1 = Fy(mval[2]),
           mean.trt1.mkr1 = Fy(mval[2]), 
           trteff.mkr0 =mean(event.b[trt.b==0 & marker.b ==mval[1]]) - mean(event.b[trt.b==1 & marker.b ==mval[1]]), 
           trteff.mkr1 =mean(event.b[trt.b==0 & marker.b ==mval[2]]) - mean(event.b[trt.b==1 & marker.b ==mval[2]])
           )

    }

  
  # now bootstrap

  boot.data <- replicate(bootstraps, one.boot.plot_disc( event, trt, marker, rho,obp.boot.sample, obp.get.F))
  
  if(substr(ci, 1, 1) =="h"){
    
    if(substr(plot.type, 1, 3) =="ris") myconf.ints <- apply(boot.data[5:8,], 1, quantile, probs = c(alpha/2, 1-alpha/2))
    if(substr(plot.type, 1, 3) =="tre") myconf.ints <- apply(boot.data[c(5,7),], 1, quantile, probs = c(alpha/2, 1-alpha/2))
    if(substr(plot.type, 1, 3) =="cdf") myconf.ints <- apply(boot.data[c(9:10),], 1, quantile, probs = c(alpha/2, 1-alpha/2))
    
    }else{

    if(substr(plot.type, 1, 3) =="ris") myconf.ints <- apply(boot.data[1:4,], 1, quantile, probs = c(alpha/2, 1-alpha/2))
    if(substr(plot.type, 1, 3) =="tre") myconf.ints <- apply(boot.data[c(9:10),], 1, quantile, probs = c(alpha/2, 1-alpha/2))
    if(substr(plot.type, 1, 3) =="cdf") myconf.ints <- apply(boot.data[c(5,7),], 1, quantile, probs = c(alpha/2, 1-alpha/2))
    
  }
  
  myconf.ints
}
