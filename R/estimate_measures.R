#' a simple function to estimate summary measures for a  treatment selection biomarker. 
#' 
#' Provides point estimates for summary measures used to evaluate a treatment selection biomarker.  
#' 
#' 
#' @param event Name of indicator vector for adverse event found in data.frame
#' "data". Coded 1 for occurrence of event and 0 otherwise.
#' @param trt Name of treatment status in data.frame "data". Coded 1 for
#' "treated" and 0 for "un-treated."
#' @param marker.neg 
#' @param trt.effect
#' 
#' @export
estimate_measures <- function(event, trt, marker.neg, trt.effect , default.trt = c("trt all", "trt none")){
  
  default.trt = match.arg(default.trt)
  
  neg <- marker.neg
  pos <- 1 - neg
  
  if(missing(trt.effect)){ 
    warning("Estimates of trt.effect are not provided. Only empirical estimates will be calculated.")
    noModelBased <- TRUE
    trt.effect <- rep(NA, length(trt))
  }else{
    noModelBased <-FALSE
  }
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

