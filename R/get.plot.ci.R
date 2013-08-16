get.plot.ci <-
function( marker, trt, event, study.design, rho = rho, plot.type, ci, bootstraps, fixed.values, obp.boot.sample, obp.get.F, link, alpha){
  ##first we need to look at what is fixed and what needs to vary to get the ci's we want
  ## I am just storing some index variables that make me able to calculate the 
  ## proper ci's later.  
  fix.ind <- NULL 
  out.ind <- NULL 

  #the data will be set up as 
  # F.marker, risk_t0, risk_t1 (all sorted by F.marker), F.event, obs delta (sorted by F.event)

     myplot <- plot.type  #which study.design of plot
     myci   <- ci    #orientation of confidence intervals

     n = length(fixed.values)

     if(substr(myplot, 1, 4) =="risk"){ 
     #predcurve
        if(substr(myci, 1, 4) =="hori") {
           ## predcurve plot with horizontal ci bands
           fix.ind = 2:3    #fix risk_trt
           out.ind = c(1,1) #output F.marker  
        }else if(substr(myci, 1, 4) =="vert"){
           ## predcurve plot with vertical ci bands
           fix.ind = c(1,1) #fix F.marker
           out.ind = 2:3      #output risk_trt
        }
        n <- length(fixed.values)
     } else if(substr(myplot, 1, 4) =="trea"){
     #trteffect curve
     
        if(substr(myci, 1, 4) =="hori") {
           ## predcurve plot with horizontal ci bands
           fix.ind = 5    # fix delta
           out.ind = 4    # vary F.marker
        }else if(substr(myci, 1, 4) =="vert"){
           ## predcurve plot with vertical ci bands
           fix.ind = 4    #fix F.event
           out.ind = 5    #vary delta
        }
     }else if(substr(myplot, 1, 4) =="cdf"){
     #CDF delta curve
        if(substr(myci, 1, 4) =="hori") {
           ## predcurve plot with horizontal ci bands
           fix.ind = 4    
           out.ind = 5  
        }else if(substr(myci, 1, 4) =="vert"){
           ## predcurve plot with vertical ci bands
           fix.ind = 5
           out.ind = 4
        }
     }

  


  
  # now bootstrap

  boot.data <- replicate(bootstraps, one.boot.plot( event, trt, marker, rho, study.design, obp.boot.sample, obp.get.F, fixed.values, fix.ind, out.ind, link))
  if(substr(myplot, 1,3)=="ris"){ boot.data[is.na(boot.data)] <- 0 }

  myconf.ints <- matrix(ncol  = n, nrow = 2*length(fix.ind)) 

  if(length(fix.ind) > 1){
  for( i in 1:length(fix.ind)){
     j = i*2
     #n is the number of fixed values, we have to handle n = 1 differently than n > 1
     if(n > 1){
     myconf.ints[(j-1):(j),] <- apply( boot.data[i,,], 1, quantile,probs = c(alpha/2, 1-alpha/2), na.rm = TRUE) 
     }else{
     myconf.ints[(j-1):(j),] <- quantile( boot.data[i,,],probs = c(alpha/2, 1-alpha/2), na.rm = TRUE) 

     }
  }
  }else{
     #n is the number of fixed values, we have to handle n = 1 differently than n > 1
     if(n > 1){
        myconf.ints[1:2,] <- apply( boot.data, 1, quantile,probs = c(alpha/2, 1-alpha/2), na.rm = TRUE) 
     }else{
        myconf.ints[1:2,] <- quantile( boot.data, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE) 

     }

  }
  myconf.ints
}
