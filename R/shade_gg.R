shade_gg <-
function(p, bounds, fixed, type, lty = 1, bands){
  
   if(any(dim(bounds)==0) & substr(type,1,1)=="h"){
     
     return(p + coord_flip())
   }else if(any(dim(bounds)==0)  & substr(type, 1, 1)=="v"){
     
     return(p)
   }
   
   numbounds <- ncol(bounds)
   if(is.null(numbounds)) {
      numbounds <- 1
      bounds <- matrix(bounds, ncol = 1)
   } 

   mydat <- data.frame(t(bounds), fixed)
   names(mydat) = c("low", "high", "fixed")
  
  
   if(type == "v"){
     #vertical ci
   if(bands == FALSE){
     p <- p + geom_errorbar(data = mydat, aes(ymin = low, ymax = high, x = fixed), linetype = lty, color = "gray50", size = 1, width = 5)
   }else{
     p <- p + geom_ribbon(data = mydat, alpha =.2, aes(x = fixed, ymin = low, ymax = high))

   }
   }else if(type =="h"){
   if(bands == FALSE){

      p <- p + geom_errorbar(data = mydat, aes(ymin = low, ymax = high, x = fixed), linetype = lty, color = "gray50", size = 1, width = 5) + coord_flip()
     
   }else{
      p <- p + geom_ribbon(data = mydat, alpha =.2, aes(x = fixed, ymin = low, ymax = high)) +coord_flip() #+ scale_x_reverse()
      
   }
   }
   return(p)
}
