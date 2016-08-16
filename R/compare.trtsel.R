#' method to compare trtsel objects
#' @param x object
#' @param \dots not used  
#' @export
compare <- function(x, ...) UseMethod("compare")

#' compare the performance of two treatment selection markers
#' 
#' Evaluates and compares the performance of two treatment selection markers
#' measured in the same data.  Summary measures of the performance of each
#' marker are estimated and confidence intervals are provided.  Differences in
#' measures of performance between markers are estimated along with confidence
#' intervals, and the results of tests comparing marker performance measures
#' are returned.  "Treatment effect curves" for each marker are overlaid on the
#' same plot.  An object of class 'trtsel' must first be created for each
#' marker using the function 'trtsel' by supplying a data.frame containing
#' marker, treatment, and event status information; treatment and event data
#' must be identical for the two markers.
#' 
#' 
#' @param x An object of class "trtsel" created by using function
#' "trtsel". This is created using data for the first marker.  Note: event and
#' treatment vectors provided to create this trtsel object must be identical to
#' those used to create the trtsel2 object.
#' @param trtsel2 An object of class "trtsel" created by using function
#' "trtsel". This is created using data for the second marker.
#' @param bootstraps Number of bootstrap replicates for creating confidence
#' intervals and bands. Default value is 500. Set bootstraps=0 if no confidence
#' intervals or bands are desired.
#' @param alpha (1-alpha)*100\% confidence intervals are calculated. Default
#' value is alpha = 0.05 which yields 95\% CI's.
#' @param plot Indication of whether a plot showing treatment effect curves for
#' the two markers should be created.  TRUE (default) or FALSE.
#' @param ci If plot = TRUE, indication of whether horizontal or vertical
#' confidence intervals be plotted.  Character string of either "horizontal"
#' (default) or "vertical." Only applies if plot = TRUE.
#' @param fixed.values A numeric vector indicating fixed values on the x- or
#' y-axes at which bootstrap confidence intervals are provided. If
#' "fixed.values" are provided, point-wise confidence intervals will be printed
#' (i.e. conf.bands will be taken as FALSE).  This option applies to the plot
#' only.
#' @param offset If confidence intervals are to be plotted for specified
#' fixed.values, offset is the amount of distance to offset confidence
#' intervals so that they do not overlap on the plot. The default value is
#' 0.01. Only applies if plot = TRUE.
#' 
#' @param conf.bands Indication of whether pointwise confidence intervals are
#' shown for the curve(s).  TRUE (default) or FALSE. If "fixed.values" are
#' input, this option is ignored and no confidence bands are produced. Only
#' applies if plot = TRUE.
#' @param conf.bandsN If conf.bands = TRUE, the number of points along the x-
#' or y-axis at which to calculate the pointwise confidence intervals. The
#' default is 100. Only applies if plot = TRUE.
#' @param model.names A vector of length 2 indicating the names for the two
#' markers in trtsel1, and trtsel2, respectively, for the plot legend. The default value is c("Model 1",
#' "Model 2").
#' @param xlab A label for the x-axis. Default values depend on plot.type. Only
#' applies if plot = TRUE.
#' @param ylab A label for the y-axis. Default values depend on plot.type. Only
#' applies if plot = TRUE.
#' @param xlim The limits for the x-axisof the plot, in the form c(x1,x2). Only
#' applies if plot = TRUE.
#' @param ylim The limits for the y-axis of the plot, in the form c(y1,y2).
#' Only applies if plot = TRUE.
#' @param main The main title for the plot. Only applies if plot = TRUE.
#' @param annotate.plot Only applies to comparison of two discrete markers.
#' Should the plot be annotated with the marker group percentages? default is
#' TRUE.
#' @return A list with components (see Janes et al. (2013) for a description of
#' the summary measures and estimators):
#' 
#' \item{estimates.marker1}{ Point estimates of the summary measures: p.neg,
#' p.pos, B.neg, B.pos, Theta, Var.Delta, TG and event rates for marker 1. }
#' \item{estimates.marker2}{ Point estimates for the same summary measures for
#' marker 2.} \item{estimates.diff}{ Estimated difference in summary measures
#' (marker 1 - marker 2).} \item{ci.marker1}{ 2x9 data.frame with confidence
#' intervals for marker 1 performance measures.} \item{ci.marker2}{ 2x9
#' data.frame with confidence intervals for marker 2 performance measures.}
#' \item{ci.diff}{ 2x9 data.frame with confidence intervals for differences in
#' performance measures.} \item{p.values}{1x9 data.frame with p-values for each
#' difference corresponding to a test of H0: marker 1 performance = marker 2
#' performance.} \item{bootstraps}{ bootstraps value provided.}
#' 
#' In addition, if plot = TRUE: \item{plot}{ ggplot object containing the
#' generated plot.} \item{plot.ci.marker1}{ A data.frame containing the bounds
#' of the bootstrap-based confidence intervals for marker1, along with the
#' fixed.values they are centered around, where applicable. }
#' \item{plot.ci.marker2}{ a data.frame containing the bounds of the
#' bootstrap-based confidence intervals for marker 2, along with the
#' fixed.values they are centered around, where applicable. }
#' @note Plot output is only produced when comparing a continuous (discrete)
#' marker to a continuous (discrete) marker because confidence bands are
#' calculated differently for continuous vs. discrete markers. See the note
#' under ?plot.trtsel, for a description of the differences between how the
#' bootstrap confidence bands are calculated.
#' @seealso \code{\link{trtsel}} for creating trtsel objects,
#' \code{\link{plot.trtsel}} for plotting risk curves and more,
#' \code{\link{evaluate.trtsel}} for evaluating marker performance, and
#' \code{\link{calibrate.trtsel}} for assessing model calibration..
#' @references
#' 
#' Janes, Holly; Brown, Marshall D; Pepe, Margaret; Huang, Ying; "An Approach
#' to Evaluating and Comparing Biomarkers for Patient Treatment Selection" The
#' International Journal of Biostatistics. Volume 0, Issue 0, ISSN (Online)
#' 1557-4679, ISSN (Print) 2194-573X, DOI: 10.1515/ijb-2012-0052, April 2014
#' @examples
#' 
#' 
#' data(tsdata)
#' 
#' ###########################
#' ## Create trtsel objects 
#' ###########################
#' 
#' trtsel.Y1 <- trtsel(event ~ Y1*trt, 
#'                    treatment.name = "trt", 
#'                    data = tsdata, 
#'                    study.design = "RCT",
#'                    link = "logit", 
#'                    default.trt = "trt all")
#'
#' trtsel.Y1
#' 
#' trtsel.Y2 <- trtsel(event ~ Y2*trt, 
#'                    treatment.name = "trt", 
#'                    data = tsdata, 
#'                    default.trt = "trt all")
#' trtsel.Y2
#'                           
#' 
#' ###############################
#' ## Compare marker performance
#' ###############################
#' 
#' 
#' # Plot treatment effect curves with pointwise confidence intervals
#' ## use more bootstraps in practice
#' compare(trtsel1 = trtsel.Y1, trtsel2 = trtsel.Y2,
#'                                 bootstraps = 10, plot = TRUE,      
#'                                 ci = "horizontal",  conf.bands = TRUE) 
#'                                 
#' 
#' @method compare trtsel
#' @export
compare.trtsel <-
function(trtsel1, trtsel2, bootstraps = 500, alpha = .05, plot = TRUE, 
                           ci   = "horizontal", fixed.values =  NULL, offset = .01,
                            conf.bands = TRUE, conf.bandsN =100, model.names = c("Model 1", "Model 2"), 
                           xlab = NULL, 
                           ylab = NULL, 
                           xlim = NULL, 
                           ylim = NULL, 
                           main = NULL, annotate.plot = TRUE){
  quantile <- NULL #appease check
  # assume paired data here, so each individual has a measurement on y1 and y2. Also I am assuming each data set is ordered the same way. 


  if(!is.trtsel(trtsel1)) stop("trtsel1 must be an object of class 'trtsel' created by using the function 'trtsel' see ?trtsel for more help")
  if(!is.trtsel(trtsel2)) stop("trtsel2 must be an object of class 'trtsel' created by using the function 'trtsel' see ?trtsel for more help")
  
  if(trtsel1$model.fit$outcome != trtsel2$model.fit$outcome) stop("This function can not compare trtsel objects with different outcome types: binary outcome to one with a time-to-event outcome.")
  
  
  if(alpha<0 | alpha > 1) stop("Error: alpha should be between 0 and 1")
  if(bootstraps ==0 ) cat("bootstrap confidence intervals will not be calculated\n")
  if(bootstraps == 1) {warning("Number of bootstraps must be greater than 1, bootstrap confidence intervals will not be computed"); bootstraps <- 0;}  
  
  
  study.design  <-trtsel1$model.fit$study.design
  rho   <-trtsel1$model.fit$cohort.attributes #because of paired data, rho should be the same for each trtsel object
  link <- trtsel1$model.fit$link
  
  data1 <- trtsel1$derived.data 
  data2 <- trtsel2$derived.data
  
  if(trtsel1$default.trt != trtsel2$default.trt) stop("default.trt is different between markers. Summary measure comparison would not be valid.")
  
  #cant compare a biomarker- provided trtsel object with one that was generated by fitted risks being input
  #if(ncol(data1) != ncol(data2)) stop("cannot compare a trtsel object that was created by providing fitted risk to another trtsel object that was not. Bootstrapping methods are not comparable.") 
  
  if(nrow(data1) != nrow(data2)) stop("trtsel objects must have the same number of observations for comparison")
  
  
  if( trtsel1$model.fit$outcome  == "time-to-event"){
    event.name1 = trtsel1$formula[[2]]
    event.name2 = trtsel1$formula[[2]]
    
    mysurv <- with(trtsel1$derived.data, eval(event.name1))
    event1 <- mysurv[,2]
    mysurv <- with(trtsel2$derived.data, eval(event.name2))
    event2 <- mysurv[,2]
    
  }else{
    event.name1 = as.character(trtsel1$formula[[2]])
    event.name2 = as.character(trtsel2$formula[[2]])

    event1 <-  trtsel1$derived.data[[event.name1]]
    event2 <-  trtsel2$derived.data[[event.name2]]
  }
  

  if(!all.equal(data1[[trtsel1$treatment.name]], 
                data2[[trtsel2$treatment.name]], check.attributes = FALSE, use.names = FALSE)) stop("trt labels must be identical to compare markers!")

  if(!all.equal(event1, event2, check.attributes = FALSE, use.names = FALSE)) stop("event labels must be identical to compare markers!")
  
  boot.sample <- trtsel1$functions$boot.sample
  get.summary.measures <- trtsel1$functions$get.summary.measures
  
  if(bootstraps > 1){
  #get bootstrap data
  boot.data <- replicate(bootstraps, one.boot.compare(data1 = data1, data2 = data2,
                                                      formulas = list(trtsel1$formula, trtsel2$formula), 
                                                      event.names = c(event.name1, event.name2), 
                                                      treatment.names = c(trtsel1$treatment.name, trtsel2$treatment.name), 
                                                      rho = rho, study.design = study.design, obe.boot.sample = boot.sample, 
                                                      obe.get.summary.measures = get.summary.measures, link = link, 
                                                      d = trtsel1$model.fit$thresh, 
                                                      disc.marker.neg1 = trtsel1$model.fit$disc.marker.neg, 
                                                      disc.marker.neg2 = trtsel2$model.fit$disc.marker.neg, 
                                                      prediction.times = c(trtsel1$prediction.time, trtsel2$prediction.time)))
  
  boot.data1 <- boot.data[c(1:4, 9:18),]
  boot.data2 <- boot.data[c(5:8, 25:34),]

  boot.data = boot.data1 - boot.data2

  ## Estimate summary measures
  if(link == "time-to-event") data1$prediction.time = trtsel1$prediction.time
  if(link == "time-to-event") data2$prediction.time = trtsel2$prediction.time
  
  sm.m1 <- get.summary.measures(data1, event.name1, trtsel1$treatment.name,  rho)
  sm.m2 <- get.summary.measures(data2, event.name2, trtsel2$treatment.name, rho)
  sm.diff <- as.data.frame(t(unlist(sm.m1) - unlist(sm.m2) ))
  
  ci.m1   <- apply(boot.data1[-c(1:4),], 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)
  ci.m2   <- apply(boot.data2[-c(1:4),], 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)

  ci.diff <- apply(boot.data[-c(1:4),], 1, quantile, probs = c(alpha/2, 1-alpha/2), na.rm = TRUE)


  ## Get p-values for differences in all performance measures (ie boot.data[5:13,])

  potential.pvals <- (1:bootstraps)/bootstraps
  p.vals <- rep(0, 10)  

  for( i in 1:10 ){

   tmp.ind <- c(5:14)[i]
   tmp.boot.data <- boot.data[tmp.ind,]
   tmp.boot.data <- tmp.boot.data[is.finite(tmp.boot.data)]

   if(!cover(min(tmp.boot.data), max(tmp.boot.data), 0) ){ 
     
     p.vals[i] <- 0


   }else{

     reject.all <- unname( mapply( cover, 
                                   quantile(tmp.boot.data, potential.pvals/2, type = 1, na.rm = TRUE),
                                   quantile(tmp.boot.data, 1 - potential.pvals/2, type = 1, na.rm = TRUE), 
                                   rep(0, bootstraps))  )
     reject.all <- c(reject.all, FALSE)
     tmp.reject <- which(reject.all==FALSE)[1] 
     p.vals[i] <- potential.pvals[ifelse(tmp.reject==1, 1, tmp.reject - 1)]
   }
    
  }

  p.vals <- data.frame(t(p.vals))
  names(p.vals) <- names(sm.diff)[1:10]
  row.names(p.vals) <- c("p.values")
  row.names(ci.m1) <- c("lower", "upper")
  row.names(ci.m2) <- c("lower", "upper")
  row.names(ci.diff) <- c("lower", "upper")
  result <- list(estimates.marker1   = data.frame(sm.m1),
                 estimates.marker2   = data.frame(sm.m2), 
                 estimates.diff = data.frame(sm.diff), 
                 ci.marker1   = data.frame(ci.m1), 
                 ci.marker2   = data.frame(ci.m2), 
                 ci.diff   = data.frame(ci.diff),
                 trtsel1 = trtsel1, 
                 trtsel2 = trtsel2, 
                 p.values = p.vals,  
                 bootstraps = bootstraps)

  }else{
  sm.m1 <- get.summary.measures(data1, rho)
  sm.m2 <- get.summary.measures(data2, rho)
  sm.diff <- as.data.frame(t(unlist(sm.m1) - unlist(sm.m2) ))

result <- list(estimates.marker1   = data.frame(sm.m1),
                 estimates.marker2   = data.frame(sm.m2), 
                 estimates.diff = data.frame(sm.diff), 
                 trtsel1 = trtsel1, 
                 trtsel2 = trtsel2, 
                 bootstraps = bootstraps)
  }

  #for plotting, we can only compare marker types that are the same...ie discrete to discrete, continuous to continuous
  same.marker.type = (is.null(trtsel1$model.fit$disc.marker.neg) == is.null(trtsel2$model.fit$disc.marker.neg))
  if(plot & !same.marker.type) {
    warning("Can not generate plots to compare a discrete marker to a continuous marker (bootstrap methods are not comparable). No plots will be produced!")
    plot = FALSE
  }
  

  if(plot & is.null(trtsel1$model.fit$disc.marker.neg)){ 
    if(!is.element(ci, c("vertical", "horizontal"))) stop("If plotting, ci must be one of `vertical` or `horizontal`")
    
  if(length(fixed.values>0)) conf.bands = FALSE 
  if(conf.bands){
    
   if(substr(ci, 1,1 )=="v"){
      fixed.values1 = seq(from = 0, to = 100, length.out = conf.bandsN)
      fixed.values2 = seq(from = 0, to = 100, length.out = conf.bandsN)
      offset = 0

   }else{
      fixed.values1 = seq(from = min(c(data1$trt.effect)), to = max(c(data1$trt.effect)), length.out = conf.bandsN)
      fixed.values2 = seq(from = min(c(data2$trt.effect)), to = max(c(data2$trt.effect)), length.out = conf.bandsN)
      offset = 0

   }

  }else{
   fixed.values1 <- fixed.values
   fixed.values2 <- fixed.values
  } 
  
  curves <-  myplotcompare.trtsel( x = result, bootstraps =bootstraps, alpha  = alpha, ci = ci, marker.names = model.names,
                           fixeddeltas.y1 =  fixed.values1, fixeddeltas.y2 = fixed.values2, 
                           xlab = xlab, 
                           ylab = ylab, 
                           xlim = xlim, 
                           ylim = ylim, 
                           main = main, offset = offset, conf.bands=conf.bands)
  result$plot <- curves$plot

  result$plot.ci.marker1 <- curves$trtsel1$conf.intervals
  result$plot.ci.marker2 <- curves$trtsel2$conf.intervals
  }else if(plot & !is.null(trtsel1$model.fit$disc.marker.neg)){
    
    curves <-  myplotcompare.trtsel_disc( x = result, bootstraps =bootstraps, alpha  = alpha, ci = ci, marker.names = model.names, 
                                     xlab = xlab, 
                                     ylab = ylab, 
                                     xlim = xlim, 
                                     ylim = ylim, 
                                     main = main, offset = offset, conf.bands=conf.bands, annotate.plot)
    result$plot <- curves$plot
    result$plot.ci <- curves$ci.bounds

    
    
  }
  
  
  
  result$trtsel1 <- NULL
  result$trtsel2 <- NULL
  class(result) <- "compare.trtsel"
  result$alpha = alpha
  result$model.names = model.names 
  result$formulas = list(trtsel1$formula, trtsel2$formula)
  return(result) 

}
