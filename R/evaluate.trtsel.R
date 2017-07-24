#' a method for evaluating a trtsel object
#' @param x object
#' @param \dots not used  
#' @seealso \code{\link{evaluate.trtsel}}
#' @export 
evaluate <- function(x, ...) UseMethod("evaluate")

#' evaluate the performance of one or more biomarkers for their ability to guide patient treatment recommendations.
#' 
#'   Evaluates the ability of one or more biomarkers for their ability to guide patient treatment recommendations.  
#'   (Bias-corrected) summary measures of marker performance are estimated and confidence intervals are provided.  
#'   This function accepts as input an object of class "trtsel", created using the function "trtsel". 
#' 
#'@aliases evaluate.trtsel 
#' 
#'@param x An object of class "trtsel", created by using the function "trtsel."
#'@param bootstraps Number of bootstrap replicates for creating confidence intervals for each performance measure. The default value is 1000. Set bootstraps = 0 if no confidence intervals are desired.
#'@param bias.correct logical indicator of whether to bias-correct measures for over-optimism using bootstrap-bias correction. When the same data is used to fit and evaluate the model, performance measures are over-optimistic. Setting this equal to TRUE uses a bootstrap method to bias-correct performance measures. See details for more information.  
#'@param alpha (1-alpha)*100\% confidence intervals are calculated. Default value is alpha = 0.05 which yields 95\% CI's.  The same alpha is used for the two-sided type-I error for the test of H0: No decrease in event rate under marker-based treatment.
#'@param ... ignored.
#'@return  A list with the following components (see Janes et al. (2013) for a description of the summary measures and estimators): 
#'\item{test.Null }{ List of results of a test of the null hypothesis H0: No decrease in event rate under marker-based treatment. Contains "reject" (logical; was H0 rejected), p.value, z.statistic, and alpha }
#'\item{estimates }{data.frame of dimension 1x9 of summary measure estimates. Includes:
#'    p.rec.trt : proportion recommended treatment;
#'  p.rec.notrt : proportion recommended no treatment;
#'  B.neg.emp, B.neg.mod: Average benefit of no treatment among those recommended no trt, empirical and model-based estimates;
#'  B.pos.emp, B.neg.mod: Average benefit of treatment among those recommended trt, empirical and model-based estimates;
#'  Theta.emp, Theta.mod: Decrease in rate of outcomes under marker-based treatment, empirical and model-based estimates; 
#'  Var.Delta: variance in estimated treatment effect; 
#'  TG: Total gain.  
#'  ER.trt0.emp/mod, ER.trt1.emp/mod, ER.mkrbased.emp/mod: Event rates under trt = 0, trt = 1 or for marker-based treatment. 
#'  Marker.Thresh: Marker positivity threshold-- defined as the maximum marker value such that estimated treatment effect < "thresh" (=treatment effect used to define the treatment rule, this is set when creating the trtsel object). If all observations are marker negative (or all are positive), Marker.Thresh has value NA}
#' \item{conf.intervals}{ data.frame of dimension 2x9 with bootstrap-based confidence intervals for each summary measures. If bootstraps = 0 or boot = FALSE, this component is NULL. }
#'@note The evaluate function used this way both fit and evaluates a risk model using the same data, which is known to bias performance measure estimates to be overly optimistic. To correct for this bias, the evaluate function by default sets bias.correct=TRUE. This implements bootstrap bias correction via the ``refined bootstrap" method described in Efron and Tibshirani 1994. In short, we sample BB bootstrap datasets. For each, obtain a new treatment selection rule based on the re-fit model and calculate the difference in estimated performance of this rule using the bootstrap vs. original data. The average of these differences estimates the bias. We shift naive performance estimates and confidence intervals down by the estimated bias.
#'@seealso \code{\link{trtsel}} for creating trtsel objects, \code{\link{plot.trtsel}} for plotting risk curves and more, \code{\link{calibrate.trtsel}} for assessing model calibration, and \code{\link{compare.trtsel}} to compare two trtsel object. 
#'@examples 
#'data(tsdata)
#'
#'###########################
#'## Create trtsel objects 
#'###########################
#'
#'trtsel.Y1 <- trtsel(event ~ Y1*trt, 
#'                    treatment.name = "trt", 
#'                    data = tsdata, 
#'                    study.design = "RCT",
#'                    family = binomial(link = "logit"), 
#'                    default.trt = "trt all")
#'trtsel.Y1
#'
#'#################################
#'## Evaluate marker performance
#'#################################
#'
#'# Marker Y1
#'estimates.Y1 <- evaluate(trtsel.Y1, bootstraps = 50)
#'estimates.Y1
#'
#'@method evaluate trtsel
#'@import stats
#'@export  
evaluate.trtsel <-
function(x, ...,  bias.correct = TRUE, bootstraps = 1000, alpha = .05){
 
  if(!is.trtsel(x)) stop("x must be an object of class 'trtsel' created by using the function 'trtsel' see ?trtsel for more help")
 
  if(alpha<0 | alpha > 1) stop("Error: alpha should be between 0 and 1")
  if(bootstraps ==0 ) print("bootstrap confidence intervals will not be calculated")
  if(bootstraps == 1) warning("Number of bootstraps must be greater than 1, bootstrap confidence intervals will not be computed") 
  stopifnot(is.logical(bias.correct))
  if(x$model.fit$family$family == "risks_provided"){
    bias.correct = FALSE
  }
  if(missing(bias.correct)){
    if(x$model.fit$family$family == "risks_provided"){
      bias.correct = FALSE
    }else if (bootstraps > 1){
      bias.correct  =TRUE
      message("Bootstrap bias-correction will be implemented to correct for over-optimism bias in estimation.")
    }else{
      bias.correct = FALSE
    }
    
  }
  
  if(bias.correct & x$model.fit$family$family  == "risks_provided") {
    message("risks are already provided from a fitted model, bias-correction is not available. \n Reported measures will not be bias-corrected.")
    bias.correct = FALSE
  } 
  data<-x$derived.data
  study.design<-x$model.fit$study.design
  rho<-x$model.fit$cohort.attributes
  boot.sample <- x$functions$boot.sample
  get.summary.measures <- x$functions$get.summary.measures
  
  #we only test the 
  test.Null.val <- NA #test.Null(x, alpha = alpha)
  event.name = as.character(x$formula[[2]])
  treatment.name = x$treatment.name 
  
  family <- x$model.fit$family
  
  if(x$model.fit$family$family == "risks_provided") provided_risk <- cbind(x$derived.data$fittedrisk.t0, x$derived.data$fittedrisk.t1)
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
                                                   family= family, 
                                                   disc.rec.no.trt = x$model.fit$disc.rec.no.trt, 
                                                   provided_risk = provided_risk, 
                                                   prediction.time = x$prediction.time,
                                                   bbc = bias.correct))
  
 #appease check
  quantile <- NULL
  conf.intervals <- apply(boot.data[-c(1:4),], 1, quantile, probs = c(alpha/2, 1-alpha/2), type = 1, na.rm = TRUE) 
  row.names(conf.intervals) <- c("lower", "upper")
  
  if(bias.correct){
    bias  <- apply(boot.data[c(5:36), ], 1, mean, na.rm  = TRUE)
    bias <- bias[1:16] - bias[17:32]
  
    conf.intervals <- conf.intervals[,1:16] - rbind(bias, bias) 
  }else{
    bias = rep(0, dim(conf.intervals)[[2]]/2)
  }

  }else{
    conf.intervals = NULL
  }
  
  ## 2. Estimate summary measures
  if(x$model.fit$family$family == "time-to-event") event.name = x$formula[[2]]
  
  
  summary.measures <- data.frame(get.summary.measures(data, event.name, treatment.name,  rho))
  summary.measures <- summary.measures - bias
  
  #marker threshold st delta(mthresh) = 0
  if(any(data$rec.no.trt==0) & any(data$rec.no.trt==1) &is.null(x$model.fit$disc.rec.no.trt)& family$family != "risks_provided" ){
    
    #only calculate marker threshold if there is a single marker 
    if(length(x$model.fit$marker.names)==1){
      marker = data[[x$model.fit$marker.names]]
    summary.measures$Marker.Thresh <-ifelse(  data$trt.effect[which.min(marker)] < 0 , 
                                             max(marker[data$rec.no.trt == 1]), 
                                             min(marker[data$rec.no.trt == 1]))
     }

  }else if(any(data$rec.trt==1) & any(data$rec.trt==0) &is.null(x$model.fit$disc.rec.no.trt)& family$family != "risks_provided"){
   
    #only calculate marker threshold if there is a single marker 
    if(length(x$model.fit$marker.names)==1){
      marker = data[[x$model.fit$marker.names]]
    summary.measures$Marker.Thresh <-ifelse( data$trt.effect[which.min(marker)] < 0, 
                                             max(marker[data$rec.trt == 0]), 
                                             min(marker[data$rec.trt == 0]))
    
    }
  
  }
  
  
  if(is.null(summary.measures$Marker.Thresh)) summary.measures$Marker.Thresh <- NA

  result <- list(#test.Null          = test.Null.val, 
                 estimates = summary.measures, 
                 conf.intervals = conf.intervals,
                 bias = bias, 
                 bias.correct = bias.correct)
  if(!is.null(x$model.fit$disc.rec.no.trt)) result$discrete.marker = TRUE
  

  class(result) <- "eval.trtsel"

  return(result) 

}
