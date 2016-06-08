get.risk.t0 <-
function(coef, formula, treatment.name, data, linkinvfun) {

  data.t0 <- data
  data.t0[[treatment.name]] <-  0 
  data.t0 <- model.matrix(formula, data = data.t0)
  linkinvfun(data.t0%*%coef)
}


get.risk.t1 <-
  function(coef, formula, treatment.name, data, linkinvfun) {
    
    data.t0 <- data
    data.t0[[treatment.name]] <-  1 
    data.t0 <- model.matrix(formula, data = data.t0)
    linkinvfun(data.t0%*%coef)
  }
