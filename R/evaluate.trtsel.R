evaluate <- function(x, ...) UseMethod("evaluate")

evaluate.trtsel <-
function(x, bootstraps = 1000, alpha = .05){

  if(!is.trtsel(x)) stop("x must be an object of class 'trtsel' created by using the function 'trtsel' see ?trtsel for more help")
 
  if(alpha<0 | alpha > 1) stop("Error: alpha should be between 0 and 1")
  if(bootstraps ==0 ) print("bootstrap confidence intervals will not be calculated")
  if(bootstraps == 1) warning("Number of bootstraps must be greater than 1, bootstrap confidence intervals will not be computed") 

  data<-x$derived.data
  study.design<-x$model.fit$study.design
  rho<-x$model.fit$cohort.attributes
  boot.sample <- x$functions$boot.sample
  get.summary.measures <- x$functions$get.summary.measures
  marker.bounds <- x$model.fit$marker.bounds
  
  #we only test the 
  test.Null.val <- test.Null(x, alpha = alpha)
  event.name = as.character(x$formula[[2]])
  treatment.name = x$treatment.name 
  
  link <- x$model.fit$link
  
  if(link == "risks_provided") provided_risk <- cbind(x$derived.data$fittedrisk.t0, x$derived.data$fittedrisk.t1)
  else provided_risk = NULL
  
  data$prediction.time = x$prediction.time

  if(bootstraps > 1){
  #get bootstrap data

  boot.data <- replicate(bootstraps, one.boot.eval(data = data, 
                                                   formula = x$formula, 
                                                   treatment.name = x$treatment.name, 
                                                   rho = rho, 
                                                   d = x$model.fit$thresh, 
                                                   study.design = study.design, 
                                                   obe.boot.sample = boot.sample, 
                                                   obe.get.summary.measures = get.summary.measures, 
                                                   link = link, 
                                                   disc.marker.neg = x$model.fit$disc.marker.neg, 
                                                   provided_risk = provided_risk, 
                                                   prediction.time = x$prediction.time))
  
 #appease check
  quantile <- NULL
  conf.intervals <- apply(boot.data[-c(1:4),], 1, quantile, probs = c(alpha/2, 1-alpha/2), type = 1, na.rm = TRUE) 
  row.names(conf.intervals) <- c("lower", "upper")
  }else{
    conf.intervals = NULL
  }
  
  ## 2. Estimate summary measures
  if(x$model.fit$link == "time-to-event") event.name = x$formula[[2]]
  summary.measures <- data.frame(get.summary.measures(data, event.name, treatment.name,  rho))
  
  #marker threshold st delta(mthresh) = 0
  if(any(data$marker.neg==0) & any(data$marker.neg==1) &is.null(x$model.fit$disc.marker.neg)& link != "risks_provided" ){

    #only calculate marker threshold if there is a single marker 
     if(!is.null(data[["marker"]]) & is.numeric(data$marker)){
       
    summary.measures$Marker.Thresh <-ifelse( with(data, trt.effect[which.min(marker)]) < 0 , 
                                             max(data$marker[data$marker.neg == 1]), 
                                             min(data$marker[data$marker.neg == 1]))
     }

  }else if(any(data$marker.pos==1) & any(data$marker.pos==0) &is.null(x$model.fit$disc.marker.neg)& link != "risks_provided"){
    
    #only calculate marker threshold if there is a single marker 
    if(!is.null(data[["marker"]]) & is.numeric(data$marker)){
    summary.measures$Marker.Thresh <-ifelse( with(data, trt.effect[which.min(marker)]) < 0 , 
                                             max(data$marker[data$marker.pos == 0]), 
                                             min(data$marker[data$marker.pos == 0]))
    
    }
  
  }
  
  
  if(is.null(summary.measures$Marker.Thresh)) summary.measures$Marker.Thresh <- NA

  result <- list(test.Null          = test.Null.val, 
                 estimates = summary.measures, 
                 conf.intervals = conf.intervals)
  if(!is.null(x$model.fit$disc.marker.neg)) result$discrete.marker = TRUE
  

  class(result) <- "eval.trtsel"

  return(result) 

}
