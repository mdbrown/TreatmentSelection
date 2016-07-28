#' a simple function to estimate summary measures for a  rule to select treatment.  
#' 
#' Provides point estimates for summary measures used to evaluate a rule used to select treatment. 
#' 
#' 
#' @param event  vector for adverse event. Can be binary (1 is bad, 0 is good) or continuous (large numbers are worse). If failure time variable 'time' is set, event is used as the adverse event status indicator. 
#' @param time the failure time for survival outcomes.
#' @param trt binary trt status 1 for "treated" and 0 for "un-treated."
#' @param trt.rule 
#' @param trt.effect estimated treatment effects. 
#' @param default.trt the default treatment rule, either "trt all" or "trt none". 
#' @param prediction.time a landmark prediction time used only when the 'time' variable is set. 
#' 
#' @export
trtsel_measures <- function(event, time = NULL, trt, trt.rule, trt.effect , default.trt = c("trt all", "trt none"), 
                            prediction.time = NULL){
  
  stopifnot(is.numeric(trt))
  stopifnot(is.numeric(event))
  stopifnot(is.numeric(trt.rule))
  if(!is.numeric(trt) | !all(is.element(unique(trt), c(0,1)))) stop( "trt must be a numeric vector with elements 1 or 0") 
  
  default.trt = match.arg(default.trt)
  
  neg <- trt.rule
  pos <- 1 - neg
  
  if(missing(trt.effect)){ 
    warning("Estimates of trt.effect are not provided. Only empirical estimates will be calculated.")
    noModelBased <- TRUE
    trt.effect <- rep(NA, length(trt))
  }else{
    noModelBased <-FALSE
  }
  

  #binary marker 
  if(is.null(stime)){ 
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
  
  if(default.trt == "trt all"){
    Theta.emp <- B.neg.emp*p.marker.neg
    Theta.mod <- B.neg.mod*p.marker.neg
    
  }else{
    Theta.emp <- B.pos.emp*p.marker.pos
    Theta.mod <- B.pos.mod*p.marker.pos
  }
  

  if(noModelBased){
    Var.Delta <- NA
    TG <- NA
  }else{
    p0.hat <- mean(event[trt==0])
    p1.hat <- mean(event[trt==1])
    Var.Delta <- mean((trt.effect - (p0.hat - p1.hat))^2)
    
  delta.F <- get.F.cohort( trt.effect, event, trt, rho = NULL)
  
  ooo <- order(trt.effect)
  
  s.delta.F <- delta.F[ooo]
  
  TG <- sum( diff(c(0, s.delta.F))*abs( sort(trt.effect) - (p0.hat - p1.hat))) 
  }
  #event rates
  ER.trt0.emp = mean(event[trt==0])
  ER.trt1.emp = mean(event[trt==1])
  
  if(default.trt == "trt all"){
    #default is trt all
    ER.mkrbased.emp = ER.trt1.emp - Theta.emp 
  }else{
    ER.mkrbased.emp = ER.trt0.emp - Theta.emp    
  }
  }else{
    ##time-to-event marker 
    if(!all(is.element(unique(event), c(0,1)))) stop( "when survival time is provided, event must be a numeric vector with elements 1 or 0") 
    
    if(is.null(prediction.time)) stop("prediction.time must be set for time-to-event outcomes.")
    stopifnot(is.numeric(prediction.time))
    
    t0 <- prediction.time 
    #event = data[[event.name]]
    wi <- get.censoring.weights(ti = prediction.time, status = event, stime = stime)
    
    status = event
    stime = time
  
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
    
    if(default.trt == "trt all"){
      Theta.emp <- B.neg.emp*p.marker.neg
      Theta.mod <- B.neg.mod*p.marker.neg
      
    }else{
      Theta.emp <- B.pos.emp*p.marker.pos
      Theta.mod <- B.pos.mod*p.marker.pos
    }
    
    
    #mod
    p0.hat <- sum(wi*I(stime < t0)*(1-trt))/sum(wi*(1-trt));#mean(risk.no.trt) #mean(event[trt==0])
    p1.hat <- sum(wi*I(stime < t0)*trt)/sum(wi*trt); #mean(risk.trt)
    
    if(noModelBased){
      Var.Delta <- NA
      TG <- NA
    }else{
    Var.Delta <- mean((trt.effect - (p0.hat - p1.hat))^2)
    event = 0 
    delta.F <- get.F.cohort( trt.effect, event, trt, rho = NULL)
    
    ooo <- order(trt.effect)
    
    s.delta.F <- delta.F[ooo]
    
    TG <- sum( diff(c(0, s.delta.F))*abs( sort(trt.effect) - (p0.hat - p1.hat))) 
    }
    
    ER.trt0.emp = p0.hat
    ER.trt0.mod = mean(data$fittedrisk.t0)
    ER.trt1.emp = p1.hat
    ER.trt1.mod = mean(data$fittedrisk.t1)
    
    
    ER.mkrbased.emp = ER.pos.emp*p.marker.pos + ER.neg.emp*p.marker.neg 
    ER.mkrbased.mod = ER.pos.mod*p.marker.pos + ER.neg.mod*p.marker.neg
    
    
  }
  
  
  out <- list(     p.neg = p.marker.neg,
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
            ER.trt1.emp = ER.trt1.emp, 
            ER.mkrbased.emp = ER.mkrbased.emp
  ) 
  class(out) = "trtsel_summary_measures"
  out
}



#' 
#' print output from 'estimate_measures'
#' 
#' S3 method for class ''trtsel''
#' 
#' 
#' @param x object of class "trtsel_summary_measures", output from the estimate_measures function
#' @param \dots ignored
print.trtsel_summary_measures <-
  function(x, ...){
    
    #if we fail to reject the null hypothesis, display a warning
 
    cat("\n")
    
    cat(paste("  Summary Measure Estimates\n", sep=""))
    cat(" -----------------------------------------------------------\n")
    

    cat("  Decrease in event rate under marker-based treatment (Theta)\n")
    cat("    Empirical:   ")
    cat(paste(" ", round(x$Theta.emp,  3), sep = ""))
    cat("\n")
    cat("    Model Based: ")
    cat(paste(" ", round(x$Theta.mod,  3), sep = ""))
    cat("\n\n")
    
    cat("  Proportion marker negative: ")
    cat(paste("   ", round(x$p.neg,      3),  sep = ""))
    cat("\n")
    cat("  Proportion marker positive: ")
    cat(paste("   ", round(x$p.pos,      3),  sep = ""))
    cat("\n\n")
    
    cat("  Average benefit of no treatment among marker-negatives (B.neg)\n")
    cat("    Empirical:   ")
    cat(paste(" ", round(x$B.neg.emp,  3),  sep = ""))
    cat("\n")
    cat("    Model Based: ")
    cat(paste(" ", round(x$B.neg.mod,  3),  sep = ""))
    cat("\n\n")
    
    
    cat("  Average benefit of treatment among marker-positives (B.pos)\n")
    cat("    Empirical:   ")
    cat(paste(" ", round(x$B.pos.emp,  3),  sep = ""))
    cat("\n")
    cat("    Model Based: ")
    cat(paste(" ", round(x$B.pos.mod,  3), sep = ""))
    cat("\n\n")
    
    
    
    if(is.null(x$discrete.marker)){
      cat("  Variance in estimated treatment effect: ")
      cat(paste("  ", round(x$Var.Delta,  3),  sep = ""))
      
      
      cat("\n")
      cat("  Total Gain: ")
      cat(paste("    ", round(x$TG,  3),  sep = ""))
      

    }
    
    cat("\n\n  Event Rates:\n")
    cat(" --------------------\n")  
    cat("             Treat all       Treat None    Marker-based Treatment") 
    cat("\n")
    cat(" Empirical: ")
    cat(paste(" ", sprintf("   %.3f      ", round(x$ER.trt1.emp,  3)),  
              sprintf("     %.3f     ",round(x$ER.trt0.emp,  3)), 
              sprintf("     %.3f    ", round(x$ER.mkrbased.emp,  3)), sep=""))
    cat("\n          ")

     
    
    
  }

