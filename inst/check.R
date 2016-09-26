
SIM.data.singleMarker <- 
  function(nn,
           mu = 0, 
           Sigma = 1, 
           beta = log(3),
           beta2 = log(.5), 
           beta3 = log(2), 
           lam0 = .1, 
           cens.lam = 0, 
           time.max = 5)
  {
    
    Y <- rnorm(nn, mu, Sigma)
    trt <- rbinom(nn, size = 1, prob = .5)
    
    mu.i <- Y*beta + trt*beta2 + Y*trt*beta3
    
    #true survival time
    r.ti <- log(-log(runif(nn)))
    ti <-  -mu.i + r.ti
    ti <- exp(ti)/lam0
    
    
    
    #time.max is the followup time. 
    ci = rep(time.max, nn)
    
    
    if(cens.lam > 0){
      ci = rexp(nn, rate = cens.lam)     
    }
    
    
    ci = pmin(ci, time.max)
    
    
    #observed marker is the min of ti and ci        
    xi <- pmin(ti, ci)
    # failure indicator
    di <- ifelse( ti == xi, 1, 0)
    
    #xi is the min of ti and ci
    #di is the indicator for failure, 1 for failure, 0 for censored
    #Y is the marker values
    
    result <- as.data.frame(cbind(xi, di, Y, trt)) 
    names(result) = c( "xi", "di", "Y", "trt")
    return(result)
  }


surv_tsdata <- SIM.data.singleMarker(1000)

ts_surv <- trtsel(Surv(time = xi, event = di)~Y*trt,
                  treatment.name = "trt", 
                  prediction.time = 1, 
                  data = surv_tsdata)

plot(ts_surv, bootstraps = 50)
calibrate(ts_surv, plot.type = "risk.t1")

TreatmentSelection::evaluate(ts_surv, bootstraps = 50)

TreatmentSelection::evaluate(ts_surv, bootstraps = 50,bias.correct = FALSE)
