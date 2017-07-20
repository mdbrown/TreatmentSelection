#' print a trtsel object
#' 
#' S3 method for class ''trtsel''
#' 
#' @param x object of class "trtsel", which can be created using function
#' "trtsel." See ?trtsel for more information.
#' @param \dots ignored
#' @method print trtsel
#' @export 
print.trtsel <-
function(x, ...){

  cat(paste("Study design:", x$model.fit$study.design, "\n\n"))
  if( substr(x$model.fit$study.design,1,4)  == "nest" ){
  ca = x$model.fit$cohort.attributes
  cat("   Cohort attributes: \n")
  cat(paste("      Cohort sample size =", ca[1], "\n"))
  cat(paste("      Pr(trt == 1) in cohort =", ca[2], "\n"))
  cat(paste("      Pr(event == 1) in cohort =", ca[3], "\n"))
  cat(paste("      fraction of cases sample from cohort =", ca[4], "\n\n"))  

  }
  if( substr(x$model.fit$study.design,1,4)  == "stra" ){
    ca = x$model.fit$cohort.attributes
    cat("   Cohort attributes: \n")
    cat(paste("      Cohort sample size =", ca[1], "\n"))
    cat(paste("      Pr(trt == 0 & event == 0) in cohort =", ca[2], "\n"))
    cat(paste("      Pr(trt == 0 & event == 1) in cohort =", ca[3], "\n"))
    cat(paste("      Pr(trt == 1 & event == 0) in cohort =", ca[4], "\n"))
    cat(paste("      Pr(trt == 1 & event == 1) in cohort =", ca[5], "\n"))
    
    cat(paste("      fraction of cases with trt == 0 sampled from cohort=", ca[6], "\n"))  
    cat(paste("      fraction of cases with trt == 1 sampled from cohort=", ca[7], "\n\n"))
  }
  
  if(ncol(x$derived.data)==6){
    cat("Fitted risks provided. No model fitting implemented.")
    
  }else{
  cat("Model Fit:")
 
 print(x$model.fit$family)
  
   
  cat(" Coefficients: \n")
  print(x$model.fit$coefficients)
  }
  cat("\n\n")
  cat("Derived Data: (first ten rows)\n\n")
  print(head(x$derived.data, n=10))

  cat("\n")


}


#' 
#' print a trtsel object
#' 
#' S3 method for class ''trtsel''
#' 
#' 
#' @param x object of class "eval.trtsel", output from the eval.trtsel function
#' @param \dots ignored
#' 
#' @method print eval.trtsel
#' @export
print.eval.trtsel <-
  function(x, ...){
    
    #if we fail to reject the null hypothesis, display a warning
    alpha <- x$test.Null$alpha
    
    cat("\n")
    
    cat(paste(" Summary Measure Estimates (with ", round(100*(1-alpha)), "% confidence intervals) \n", sep=""))
    cat(" -----------------------------------------------------------\n")
    
    if(is.null(x$conf.intervals)) {
      cat("   No confidence intervals calculated...returning NA for lower and upper bounds"); cat("\n\n")
      x$conf.intervals <- matrix( nrow = 2, ncol = 16)
    }
    
    cat("  Decrease in rate of outcomes under model-based treatment rule (Theta)\n")
    cat("    Empirical:   ")
    cat(paste(" ", round(x$estimates$Theta.emp,  3), " (",
              round(unname(x$conf.intervals[1,7]), 3), ",",
              round(unname(x$conf.intervals[2,7]), 3), ") ", sep = ""))
    cat("\n")
    cat("    Model Based: ")
    cat(paste(" ", round(x$estimates$Theta.mod,  3), " (",
              round(unname(x$conf.intervals[1,8]), 3), ",",
              round(unname(x$conf.intervals[2,8]), 3), ") ", sep = ""))
    cat("\n\n")
    
    cat("  Proportion recommended no treatment:\n")
    cat(paste("   ", round(x$estimates$p.rec.no.trt,      3), " (",
              round(unname(x$conf.intervals[1,1]), 3), ",",
              round(unname(x$conf.intervals[2,1]), 3), ") ", sep = ""))
    cat("\n")
    cat("  Proportion recommended treatment:\n")
    cat(paste("   ", round(x$estimates$p.rec.trt,      3), " (",
              round(unname(x$conf.intervals[1,2]), 3), ",",
              round(unname(x$conf.intervals[2,2]), 3), ") ", sep = ""))
    cat("\n\n")
    
    cat("  Average benefit of no treatment among those recommended no trt (B.neg)\n")
    cat("    Empirical:   ")
    cat(paste(" ", round(x$estimates$B.neg.emp,  3), " (",
              round(unname(x$conf.intervals[1,3]), 3), ",",
              round(unname(x$conf.intervals[2,3]), 3), ") ", sep = ""))
    cat("\n")
    cat("    Model Based: ")
    cat(paste(" ", round(x$estimates$B.neg.mod,  3), " (",
              round(unname(x$conf.intervals[1,4]), 3), ",",
              round(unname(x$conf.intervals[2,4]), 3), ") ", sep = ""))
    cat("\n\n")
    
    
    cat("  Average benefit of treatment among those recommended treatment (B.pos)\n")
    cat("    Empirical:   ")
    cat(paste(" ", round(x$estimates$B.pos.emp,  3), " (",
              round(unname(x$conf.intervals[1,5]), 3), ",",
              round(unname(x$conf.intervals[2,5]), 3), ") ", sep = ""))
    cat("\n")
    cat("    Model Based: ")
    cat(paste(" ", round(x$estimates$B.pos.mod,  3), " (",
              round(unname(x$conf.intervals[1,6]), 3), ",",
              round(unname(x$conf.intervals[2,6]), 3), ") ", sep = ""))
    cat("\n\n\n")
    
    
    
    if(is.null(x$discrete.marker)){
      cat("  Variance in estimated treatment effect: \n  ")
      cat(paste("  ", round(x$estimates$Var.Delta,  3), " (",
                round(unname(x$conf.intervals[1,9]), 3), ",",
                round(unname(x$conf.intervals[2,9]), 3), ") ", sep = ""))
      
      
      cat("\n")
      cat("  Total Gain: \n")
      cat(paste("    ", round(x$estimates$TG,  3), " (",
                round(unname(x$conf.intervals[1,10]), 3), ",",
                round(unname(x$conf.intervals[2,10]), 3), ") ", sep = ""))
      
      cat("\n\n")
      cat("  Marker positivity threshold:  "); cat(round( x$estimates$Marker.Thresh, 3))
      cat("\n\n")
      
    }
    
    cat("  Event Rates:\n")
    cat(" --------------------\n")  
    cat("             Treat all       Treat none    Marker-based treatment rule") 
    cat("\n")
    cat(" Empirical: ")
    cat(paste(" ", sprintf("   %.3f      ", round(x$estimates$ER.trt1.emp,  3)),  
              sprintf("     %.3f     ",round(x$estimates$ER.trt0.emp,  3)), 
              sprintf("     %.3f    ", round(x$estimates$ER.mkrbased.emp,  3)), sep=""))
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f)  ", round(unname(x$c[1,13]), 3), round(unname(x$c[2,13]), 3)), 
              sprintf(" (%.3f,%.3f)  ",     round(unname(x$c[1,11]), 3), round(unname(x$c[2,11]), 3)), 
              sprintf(" (%.3f,%.3f) ",      round(unname(x$c[1,15]), 3),round(unname(x$c[2,15]), 3)), sep = ""))
    
    cat("\n Model Based:   ")
    cat(paste("", sprintf("%.3f      ", round(x$estimates$ER.trt1.mod,  3)),  
              sprintf("     %.3f     ",round(x$estimates$ER.trt0.mod,  3)), 
              sprintf("     %.3f    ", round(x$estimates$ER.mkrbased.mod,  3)), sep=""))
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f)  ", round(unname(x$c[1,14]), 3), round(unname(x$c[2,14]), 3)), 
              sprintf(" (%.3f,%.3f)  ",     round(unname(x$c[1,12]), 3), round(unname(x$c[2,12]), 3)), 
              sprintf(" (%.3f,%.3f) ",      round(unname(x$c[1,16]), 3),round(unname(x$c[2,16]), 3)), sep = ""))
    
    
    
  }


#' 
#' print a trtsel object
#' 
#' S3 method for class ''trtsel''
#' 
#' 
#' @param x object of class "compare.trtsel", output from the compare.trtsel
#' function
#' @param \dots ignored
#' @method print compare.trtsel
#' @export 
print.compare.trtsel <-
  function(x, ...){
    
    confperc <- round(100*(1-x$alpha))
    
    cat("                      Summary Measure Estimates \n")
    cat(paste("                    (with ", confperc ,"% confidence intervals) \n"))
    
    cat(paste("\n               ", x$model.names[1], "    |    ", x$model.names[2], " |   difference    (p-value)\n") )
    cat(" ------------------------------------------------------------------------\n\n")
    
    
    
    
    cat("Decrease in rate of outcomes under model-based treatment rules (Theta)\n")
    bootN <- x$bootstraps  
    
    if(bootN==0){ 
      x$p.values = rep(NA, 10)
      cat("   No confidence intervals calculated...returning NA for lower and upper bounds"); cat("\n\n")
      x$ci.model1 <- matrix( nrow = 2, ncol = 10)
      x$ci.model2 <- matrix( nrow = 2, ncol = 10)
      x$ci.diff <- matrix( nrow = 2, ncol = 1)
    }
    
    tmp.pval <- x$p.values[7]
    if(is.element(tmp.pval, 0) & bootN>0) tmp.pval <- paste("<", 1/bootN)
    
    
    
    cat(" Empirical: ")
    cat(paste(" ", sprintf("   %.3f      |", round(x$estimates.model1$Theta.emp,  3)),  
              sprintf("     %.3f     |",round(x$estimates.model2$Theta.emp,  3)), 
              sprintf("     %.3f    ", round(x$estimates.diff$Theta.emp,  3)), 
              "     (", tmp.pval,")", sep=""))
    
    
    
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f) |", round(unname(x$ci.model1[1,7]), 3), round(unname(x$ci.model1[2,6]), 3)), 
              sprintf(" (%.3f,%.3f) |", round(unname(x$ci.model2[1,7]), 3), round(unname(x$ci.model2[2,6]), 3)), 
              sprintf(" (%.3f,%.3f) ", round(unname(x$ci.diff[1,7]), 3),round(unname(x$ci.diff[2,6]	), 3)), sep = ""))
    tmp.pval <- x$p.values[8]
    if(is.element(tmp.pval, 0)) tmp.pval <- paste("<", 1/bootN)
    
    cat("\n")
    cat(" Model Based:")  
    cat(paste(" ", sprintf("  %.3f      |", round(x$estimates.model1$Theta.mod,  3)),  
              sprintf("     %.3f      |",round(x$estimates.model2$Theta.mod,  3)), 
              sprintf("     %.3f    ", round(x$estimates.diff$Theta.mod,  3)), 
              "     (", tmp.pval,")", sep=""))
    
    
    
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model1[1,8]), 3), round(unname(x$ci.model1[2,7]), 3)), 
              sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model2[1,8]), 3), round(unname(x$ci.model2[2,7]), 3)), 
              sprintf(" (%.3f,%.3f) ", round(unname(x$ci.diff[1,8]), 3),round(unname(x$ci.diff[2,7]), 3)), sep = ""))
    
    cat("\n")
    cat("\n")
    
    cat("Proportion recommended no treatment:\n")
    tmp.pval <- x$p.values[1]
    if(is.element(tmp.pval, 0)) tmp.pval <- paste("<", 1/bootN)
    
    cat("            ")  
    cat(paste(" ", sprintf("   %.3f      |", round(x$estimates.model1$p.rec.no.trt,  3)),  
              sprintf("     %.3f      |",round(x$estimates.model2$p.rec.no.trt,  3)), 
              sprintf("     %.3f    ", round(x$estimates.diff$p.rec.no.trt,  3)), 
              "     (", tmp.pval,")", sep=""))
    
    
    
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model1[1,1]), 3), round(unname(x$ci.model1[2,1]), 3)), 
              sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model2[1,1]), 3), round(unname(x$ci.model2[2,1]), 3)), 
              sprintf(" (%.3f,%.3f) ", round(unname(x$ci.diff[1,1]), 3),round(unname(x$ci.diff[2,1]), 3)), sep = ""))
    cat("\n")
    
    
    cat("Proportion recommended treatment:\n")
    tmp.pval <- x$p.values[2]
    if(is.element(tmp.pval, 0)) tmp.pval <- paste("<", 1/bootN)
    
    cat("            ")  
    cat(paste(" ", sprintf("   %.3f      |", round(x$estimates.model1$p.rec.trt,  3)),  
              sprintf("     %.3f      |",round(x$estimates.model2$p.rec.trt,  3)), 
              sprintf("     %.3f    ", round(x$estimates.diff$p.rec.trt,  3)), 
              "     (", tmp.pval,")", sep=""))
    
    
    
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model1[1,2]), 3), round(unname(x$ci.model1[2,2]), 3)), 
              sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model2[1,2]), 3), round(unname(x$ci.model2[2,2]), 3)), 
              sprintf(" (%.3f,%.3f) ", round(unname(x$ci.diff[1,2]), 3),round(unname(x$ci.diff[2,2]), 3)), sep = ""))
    
    cat("\n")
    cat("\n")
    
    tmp.pval <- x$p.values[3]
    if(is.element(tmp.pval, 0)) tmp.pval <- paste("<", 1/bootN)
    
    cat("Average benefit of no treatment among those recommended no treatment (B.neg)\n")
    cat(" Empirical: ")
    cat(paste(" ", sprintf("   %.3f      |", round(x$estimates.model1$B.neg.emp,  3)),  
              sprintf("     %.3f     |",round(x$estimates.model2$B.neg.emp,  3)), 
              sprintf("     %.3f    ", round(x$estimates.diff$B.neg.emp,  3)), 
              "     (", tmp.pval,")", sep=""))
    
    
    
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f) |", round(unname(x$ci.model1[1,3]), 3), round(unname(x$ci.model1[2,3]), 3)), 
              sprintf(" (%.3f,%.3f) |", round(unname(x$ci.model2[1,3]), 3), round(unname(x$ci.model2[2,3]), 3)), 
              sprintf(" (%.3f,%.3f) ", round(unname(x$ci.diff[1,3]), 3),round(unname(x$ci.diff[2,3]), 3)), sep = ""))
    tmp.pval <- x$p.values[4]
    if(is.element(tmp.pval, 0)) tmp.pval <- paste("<", 1/bootN)
    
    cat("\n")
    cat(" Model Based:")  
    cat(paste(" ", sprintf("  %.3f      |", round(x$estimates.model1$B.neg.mod,  3)),  
              sprintf("     %.3f      |",round(x$estimates.model2$B.neg.mod,  3)), 
              sprintf("     %.3f    ", round(x$estimates.diff$B.neg.mod,  3)), 
              "     (", tmp.pval,")", sep=""))
    
    
    
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model1[1,4]), 3), round(unname(x$ci.model1[2,4]), 3)), 
              sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model2[1,4]), 3), round(unname(x$ci.model2[2,4]), 3)), 
              sprintf(" (%.3f,%.3f) ", round(unname(x$ci.diff[1,4]), 3),round(unname(x$ci.diff[2,4]), 3)), sep = ""))
    
    cat("\n")
    cat("\n")
    
    tmp.pval <- x$p.values[5]
    if(is.element(tmp.pval, 0)) tmp.pval <- paste("<", 1/bootN)
    
    
    cat("Average benefit of treatment among those recommended treatment (B.pos)\n")
    cat(" Empirical: ")
    cat(paste(" ", sprintf("   %.3f      |", round(x$estimates.model1$B.pos.emp,  3)),  
              sprintf("     %.3f     |",round(x$estimates.model2$B.pos.emp,  3)), 
              sprintf("     %.3f    ", round(x$estimates.diff$B.pos.emp,  3)), 
              "     (", tmp.pval,")", sep=""))
    
    
    
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f) |", round(unname(x$ci.model1[1,5]), 3), round(unname(x$ci.model1[2,5]), 3)), 
              sprintf(" (%.3f,%.3f) |", round(unname(x$ci.model2[1,5]), 3), round(unname(x$ci.model2[2,5]), 3)), 
              sprintf(" (%.3f,%.3f) ", round(unname(x$ci.diff[1,5]), 3),round(unname(x$ci.diff[2,5]), 3)), sep = ""))
    tmp.pval <- x$p.values[6]
    if(is.element(tmp.pval, 0)) tmp.pval <- paste("<", 1/bootN)
    
    cat("\n")
    cat(" Model Based:")  
    cat(paste(" ", sprintf("  %.3f      |", round(x$estimates.model1$B.pos.mod,  3)),  
              sprintf("     %.3f      |",round(x$estimates.model2$B.pos.mod,  3)), 
              sprintf("     %.3f    ", round(x$estimates.diff$B.pos.mod,  3)), 
              "     (",tmp.pval,")", sep=""))
    
    
    
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model1[1,6]), 3), round(unname(x$ci.model1[2,6]), 3)), 
              sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model2[1,6]), 3), round(unname(x$ci.model2[2,6]), 3)), 
              sprintf(" (%.3f,%.3f) ", round(unname(x$ci.diff[1,6]), 3),round(unname(x$ci.diff[2,6]), 3)), sep = ""))
    
    cat("\n\n\n")
    tmp.pval <- x$p.values[9]
    if(is.element(tmp.pval, 0)) tmp.pval <- paste("<", 1/bootN)
    
    cat("Variance in estimated treatment effect : \n  ")
    cat("          ")  
    cat(paste(" ", sprintf("   %.3f      |", round(x$estimates.model1$Var.Delta,  3)),  
              sprintf("     %.3f      |",round(x$estimates.model2$Var.Delta,  3)), 
              sprintf("     %.3f    ", round(x$estimates.diff$Var.Delta,  3)), 
              "     (", tmp.pval,")", sep=""))  
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model1[1,9]), 3), round(unname(x$ci.model1[2,9]), 3)), 
              sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model2[1,9]), 3), round(unname(x$ci.model2[2,9]), 3)), 
              sprintf(" (%.3f,%.3f) ", round(unname(x$ci.diff[1,9]), 3),round(unname(x$ci.diff[2,9]), 3)), sep = ""))
    
    cat("\n\n")
    
    tmp.pval <- x$p.values[10]
    if(is.element(tmp.pval, 0)) tmp.pval <- paste("<", 1/bootN)
    
    cat("Total Gain: \n")
    cat("          ")  
    cat(paste(" ", sprintf("     %.3f      |", round(x$estimates.model1$TG,  3)),  
              sprintf("     %.3f      |",round(x$estimates.model2$TG,  3)), 
              sprintf("     %.3f    ", round(x$estimates.diff$TG,  3)), 
              "     (", tmp.pval,")", sep=""))  
    cat("\n          ")
    cat(paste(" ",sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model1[1,10]), 3), round(unname(x$ci.model1[2,10]), 3)), 
              sprintf(" (%.3f,%.3f)  |", round(unname(x$ci.model2[1,10]), 3), round(unname(x$ci.model2[2,10]), 3)), 
              sprintf(" (%.3f,%.3f) ", round(unname(x$ci.diff[1,10]), 3),round(unname(x$ci.diff[2,10]), 3)), sep = ""))
    
    cat("\n\n")  
  }


#' 
#' print a trtsel object
#' 
#' S3 method for class ''trtsel''
#' 
#' 
#' @param x object of class "calibrate.trtsel", output from the
#' calibrate.trtsel function
#' @param \dots ignored
#' @method print calibrate.trtsel
#' @export
print.calibrate.trtsel <-
  function( x, ... ) {
    cal <- x
    HL <- cal$HL.TestStat
    p.value <- cal$p.value
    Df <- cal$Df
    
    cat("\n")
    cat("  Hosmer - Lemeshow test for model calibration\n")
    cat(" ----------------------------------------------\n\n")
    
    cat(paste("   Number of Groups:", Df+2, "\n\n"))
    cat("   No Treatment (trt = 0):\n")
    
    cat("    Test Statistic = "); cat(round(HL[1], 3)); cat(",   DF = "); cat(Df); cat(",   p value = "); cat(p.value[1]); cat("\n\n")
    
    cat("   Treated (trt = 1):\n")
    
    cat("    Test Statistic = "); cat(round(HL[2], 3)); cat(",   DF = "); cat(Df); cat(",   p value = "); cat(p.value[2]); cat("\n\n\n")
    
  }


