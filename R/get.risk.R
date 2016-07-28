

get.risk.t <-
  function(coef, formula, treatment.name, data, linkinvfun, t) {
    
    data.t0 <- data
    data.t0[[treatment.name]] <-  t 
    data.t0 <- model.matrix(formula, data = data.t0)
    #this is the case for survival outcome (no intercept)
    if(ncol(data.t0) == length(coef) + 1) data.t0 = data.t0[,-1]
    linkinvfun(data.t0%*%coef)
  }

##time to event outcomes 

get.risk.t_coxph <-
  function(coxfit, treatment.name, data, prediction.time, t) {

    data.t0 <- data
    data.t0[[treatment.name]] <-  t
    sfit.a0 <- survfit(coxfit, newdata = data.t0, se.fit = FALSE, conf.type = "none")
    risk.a0 <- c( 1-summary(sfit.a0, times = prediction.time)$surv)
    risk.a0
     }

