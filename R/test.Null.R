test.Null <-
function( trtsel, alpha){
 
 # 6/6/12 changed from test based on the bootstrap to the likelihood ratio test. 
 # below is the old code

 #n.boot <- ncol(boot.data)
 #potential.pvals <- (1:n.boot)/n.boot

 #a3.b <- boot.data[4,]


 #if(!bounded){ 
   
   # first check to see if the max and min cover 0,
   # if this is true the pvalue is less than our lowest possible detectable value

 #  if(!cover(min(a3.b), max(a3.b), 0) ){ 
     
 #    p.val<- paste("<", 1/n.boot)
 #    reject <- TRUE

 #  }else{

 #    reject.all <- unname( mapply( cover, 
 #                                  quantile(a3.b, potential.pvals/2, , study.design = 1, na.rm = TRUE),
 #                                  quantile(a3.b, 1 - potential.pvals/2, study.design = 1, na.rm = TRUE), 
 #                                  rep(0, n.boot))  )
 #    tmp <- which(reject.all==FALSE)[1] 
 #    p.val <- potential.pvals[ifelse(tmp==1, 1, tmp - 1)]
 #    reject <- p.val <= alpha

 #   }

 #  }
  #we only do this test if we have a single marker: 

 if(length(trtsel$model.fit$marker.names)==1){
  if(trtsel$model.fit$outcome != "time-to-event"){
    if(is.null(trtsel$model.fit$coefficients)) p.val <- NA
    else p.val <- trtsel$model.fit$coefficients[4,4]
    reject <- p.val <= alpha
    z.value <- trtsel$model.fit$coefficients[4,3] 
    
  }else{
  if(is.null(trtsel$model.fit$coefficients)) p.val <- NA
  else p.val <- trtsel$model.fit$coefficients[3,5]
  reject <- p.val <= alpha
  z.value <- trtsel$model.fit$coefficients[3,4] 
  }
   
 }else{
   p.val <- NA
   reject <- NA
   z.value <- NA
 }
  list( reject = reject, p.value = p.val, z.statistic = z.value, alpha = alpha)

}


test.Null.warning <-
  function(x, a1a3.pval){
    
    if(!x & is.null(a1a3.pval)){
      cat("\n")
      cat("  ##############################################\n")
      cat("  ###                WARNING!                ###\n")
      cat("  ##############################################\n\n")   
      
      
      cat("  ### Not enough evidence to reject the      ###\n")
      cat("  ### hypothesis test of:                    ###\n") 
      cat("  ###                                        ###\n")        
      cat("  ### H_0 : No marker-by-treatment           ###\n")
      cat("  ###              interaction               ###\n")
      cat("  ###                                        ###\n") 
      cat("  ### Inference for Theta may be unreliable! ###\n\n")
      cat("  ##############################################\n\n") 
      
    }else if(!x & !is.null(a1a3.pval)){
      
      cat("\n")
      cat("  ##############################################\n")
      cat("  ###                WARNING!                ###\n")
      cat("  ##############################################\n\n")   
      
      
      cat("  ### Not enough evidence to reject both     ###\n")
      cat("  ### hypothesis tests of:                   ###\n") 
      cat("  ### H_0 :                                  ###\n")        
      cat("  ### 1. No marker-by-treatment interaction  ###\n")
      cat("  ###                                        ###\n")  
      cat("  ### 2. The marker positivity threshold     ###\n") 
      cat("  ###    is outside marker bounds            ###\n")  
      cat("  ###                                        ###\n")  
      cat("  ### Inference for Theta may be unreliable! ###\n\n")
      cat("  ##############################################\n\n") 
    }
  }

