
get.censoring.weights <- function(ti, stime, status){
  
  ix.ctrl <- stime >= ti
  ix.cens <- stime < ti & status == 0 
  new.times.case.ctrl <- stime
  
  
  # this saves some time. survival curve doesn't need to be estimated past ti.
  # ix <- stime > (ti + .1) # the magic number is necessary, otherwise the KM estimate at dc$ti is off.
  #  stime[ix] <- (ti + .1)
  
  cc  <- survfit(Surv(stime, status == 0) ~ 1,
                 se.fit = FALSE, type = 'kaplan-meier')
  new.times.case.ctrl[ix.ctrl] <- ti
  
  new.times.case.ctrl.sorted.incr <- sort(new.times.case.ctrl, decreasing = FALSE, method = 'shell')
  recover.original.order <- rank(new.times.case.ctrl)
  
  cens.weights <- summary(cc, times = new.times.case.ctrl.sorted.incr)$surv[recover.original.order]
  cens.weights <- 1/cens.weights
  cens.weights[ix.cens] <- 0 
  return(cens.weights)
}
