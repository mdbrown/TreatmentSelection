boot.sample.case.control <-
  function( event, trt, rho ){
    # f is the fraction of cases we sampled 
    #sample conditional on event
    f <- rho[4]
    N <- rho[1] #rho = c(Pr(event=1), cohort sample size, Pr(event==1|trt=0), Pr(event=1|trt=1), nmatch, No of trt==1)
    k <- round(sum(event==0)/sum(event==1)) #number of controls per case
    # N.t <- round(rho[2]*rho[1])
    
    N.d1.star <- rbinom(1, size = N, prob = (rho[3]))
    
    n.d1 <- f*N.d1.star #how many cases to sample
    
    n.d0 <- k*n.d1 #how many controls to sample. k is number of controls/cases we want
    
    
    myall   <- 1:length(trt) 
    
    Resample <- TRUE
    tryN = 0
    while(Resample){
      ind.d0 <- sample(myall[event==0], size = n.d0, replace = TRUE)
      ind.d1 <- sample(myall[event==1], size = n.d1, replace = TRUE)
      
      ind <- c(ind.d0, ind.d1)
      Resample <- any(table(event[ind], trt[ind]) == 0) | length(unique(trt[ind]))==1 | length(unique(event[ind]))==1
      if(Resample){ warning("Bootstrap sample failed to sample individuals in each trt/event strata. Had to resample") ; tryN = tryN +1}
      if(tryN >10) stop("Had to resample too many bootstrap replicates due to too few observations in trt/event strata")
    }
    
    # event.b <- event[ind]
    # trt.b <- trt[ind]
    
    # Pr.D1.cond.trt1.b <- mean(event.b[trt.b==1])*sum(trt.b==1)/N.t
    # Pr.D1.cond.trt0.b <- mean(event.b[trt.b==0])*sum(trt.b==0)/(N-N.t)
    
    
    rho.b <- c( N, rho[2], (N.d1.star)/N, f, -9999, -9999, -9999)  
    
    return(c(rho.b, ind))
  }

boot.sample.cohort <-
  function( event, trt, rho){
    
    #boot1 <- sample(1:sum(trt)  ,replace=TRUE)
    
    #sample conditional on trt
    #trt.b <- c(trt[trt==0][boot0], trt[trt==1][boot1])
    #Y.b <- c(Y[trt==0][boot0], Y[trt==1][boot1])
    #event.b <- c(event[trt==0][boot0], event[trt==1][boot1])
    
    # trt.b <- c(trt[boot0])
    # Y.b <- c(Y[boot0])
    # event.b <- c(event[boot0])
    
    # Y2.b <- c(Y2[boot0])
    Resample <- TRUE
    
    tryN = 0
    
    while(Resample){
      
      ind <- sample(length(trt),replace=TRUE)
      
      Resample <- any(table(event[ind], trt[ind]) == 0) | length(unique(trt[ind]))==1 | length(unique(event[ind]))==1
      
      
      if(Resample){ warning("Bootstrap sample failed to sample individuals in each trt/event strata. Had to resample") ; tryN = tryN +1}
      if(tryN >100) stop("Had to resample too many bootstrap replicates due to too few observations in trt/event strata")
    }
    
    return(c(rep(-9999, 7), ind))
  
  }

boot.sample.stratified.case.control <-
  function( event, trt,rho){
    
    n = length(trt)
    N.t0.d1 <- round(rho[1]*rho[3]) # how many cases with trt=0 in cohort
    N.t1.d1 <- round(rho[1]*rho[5]) # how many cases with trt=1 in cohort
    N.t1    <- round(rho[1]*(rho[4]+rho[5])) # how many with trt=1 in cohort
    N       <- rho[1]   
    f <- rho[6:7]
    
    N.t0 <- N - N.t1 #N is cohort sample size, so this is number of trt=0 in the cohort
    k.0 <- round(sum(event == 0 & trt == 0)/sum(event == 1 & trt == 0)) # number of controls per case in untreated arm
    k.1 <- round(sum(event == 0 & trt == 1)/sum(event == 1 & trt == 1)) #number of controls per case in treated arm
    
    #sample conditional on event and trt strata
    
    Nstar.trt1 <- rbinom(1, size = N, prob = N.t1/N) #sample number of treatments
    Nstar.trt0 <- N - Nstar.trt1 #number of controls for this boot
    
    Nstar.trt1.event1 <- rbinom(1, size = Nstar.trt1, prob = N.t1.d1/N.t1)  # sample number of controls in this treatment arm
    Nstar.trt0.event1 <- rbinom(1, size = Nstar.trt0, prob = N.t0.d1/N.t0)  # same 
    
    n.t0.d1 <- f[1]*Nstar.trt0.event1 # rewriting to homogenize notation
    n.t1.d1 <- f[2]*Nstar.trt1.event1
    
    n.t0.d0 <- k.0*n.t0.d1 #number of controls to sample in each trt arm
    n.t1.d0 <- k.1*n.t1.d1
    
    
    Resample <- TRUE
    tryN = 0
    while(Resample){
      
      myall   <- 1:n 
      boot.t0.d0 <- sample( myall[trt==0 & event==0], size = n.t0.d0,  replace = TRUE)
      boot.t1.d0 <- sample( myall[trt==1 & event==0], size = n.t1.d0,  replace = TRUE)
      boot.t0.d1 <- sample( myall[trt==0 & event==1], size = n.t0.d1,  replace = TRUE)
      boot.t1.d1 <- sample( myall[trt==1 & event==1], size = n.t1.d1,  replace = TRUE)
      ind <- c(boot.t0.d0, boot.t1.d0, boot.t0.d1, boot.t1.d1)
      
      Resample <- any(table(event[ind], trt[ind]) == 0) | length(unique(trt[ind]))==1 | length(unique(event[ind]))==1
      
      if(Resample){ warning("Bootstrap sample failed to sample individuals in each trt/event strata. Had to resample") ; tryN = tryN +1}
      if(tryN >10) stop("Had to resample too many bootstrap replicates due to too few observations in trt/event strata")
    }
    
    
    
    
    rho.b = c( N, (1 - Nstar.trt0.event1/Nstar.trt0)*Nstar.trt0/N,
               Nstar.trt0.event1/N, 
               (1 - Nstar.trt1.event1/Nstar.trt1)*Nstar.trt1/N,
               Nstar.trt1.event1/N, f)
    #c((Nstar.trt0.event1)/(Nstar.trt0), (Nstar.trt1.event1)/(Nstar.trt1), Nstar.trt0.event1, Nstar.trt1.event1, Nstar.trt1, N, k)
    
    return(c(rho.b, ind))
    
  }

