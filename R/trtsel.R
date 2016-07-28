#' create a trtsel object
#' 
#' Creates an object of class "trtsel" given a data.frame containing marker,
#' treatment, and adverse event status information.  The functions "plot",
#' "evaluate", and "calibrate" can then be used to plot risk and treatment
#' effect curves, estimate summary measures, and check model calibration. The
#' function "compare" can also be used to compare two treatment selection
#' markers.
#' 
#' 
#' @param formula a 'formula' object including outcome ~ markers and marker by treatment interactions for
#'  the treatment selection model to be evaluated. The outcome can be either binary or a
#'   'Surv' object for time-to-event outcomes. Binary variable should equal 1 for cases and 0 for controls.
#' @param treatment.name  Name of the treatment variable in data.frame "data". The treatment variable must be coded 1 for
#' "treated" and 0 for "un-treated."
#' @param data data.frame object used to fit and evaluate the model. 
#' @param fittedrisk.t0 Instead of providing a marker, fitted risks for T=0 and
#' T=1 may be provided. This should be the column name of the fitted risk for
#' T=0 that can be found in 'data'. If fitted risks are provided, a model will
#' not be fit, and all bootstrap confidence intervals will be conditional on
#' the model fit provided.
#' 
#' @param fittedrisk.t1 Name of for the fitted risks given treatment = 1.
#' @param thresh The treatment effect threshold used to define the treatment
#' rule: Do not treat if the marker-specific treatment effect is less than
#' "thresh". This is a numeric constant with a default value of 0.
#' @param study.design Character string indicating the study design used to
#' collect the data. The three options are "randomized cohort" (default),
#' "nested case-control", or "stratified nested case-control".  A "randomized
#' cohort" design is simply a randomized trial comparing T = 0 to T = 1 with
#' the marker measured at baseline.  A nested case-control or stratified nested
#' case-control study samples cases and controls for marker measurement,
#' perhaps stratified on treatment assignment, from a randomized trial
#' comparing T = 0 to T = 1.  See Janes et al. (2013) for a full description of
#' these designs.
#' 
#' If a "nested case-control" or "stratified nested case-control" design is
#' specified, "cohort.attributes"" must be provided (see below).
#' @param cohort.attributes If a "nested case-control" or "stratified nested
#' case-control" design is specified, "cohort.attributes" must be provided.
#' Order does matter.
#' 
#' For the "nested case-control" design, specify the following attributes of
#' the randomized trial "cohort" from which the case-control sample was
#' selected: \cr \cr cohort.attributes = c(cohort sample size, \cr proportion
#' treated in cohort (Pr(trt==1)), \cr adverse event prevalance in cohort
#' (Pr(event==1)), \cr fraction of cases sampled from cohort) \cr \cr
#' 
#' For the "stratitified nested case-control" design, specify the following
#' attributes of the randomized trial "cohort" from which the stratified
#' case-control sample was selected: \cr \cr cohort.attributes = c(cohort
#' sample size, \cr Pr(trt==0 & event==0) in cohort, \cr Pr(trt==0 & event==1)
#' in cohort, \cr Pr(trt==1 & event==0) in cohort, \cr fraction of cases with
#' trt == 0 sampled from cohort, \cr fraction of cases with trt == 1 sampled
#' from cohort )\cr \cr
#' @param link Link function used to fit the risk model. Options are
#' "logit"(default), "probit", "cauchit", "log" and "cloglog." Link functions
#' other than "logit" are available only when study.design = "randomized
#' cohort".
#' 
#' @param default.trt The default treatment assignment to compare with
#' marker-based treatment. Can either be set at "trt all" (default) or "trt
#' none". Use "trt all" if everyone is treated and the aim is to discover those
#' who would benefit from no treatment, but use "trt none" if the common
#' practice is to treat no-one and the goal is to discover those who would
#' benefit from treatment.
#' @param prediction.time a landmark prediction time used only when the outcome is a time-to-event Surv object 
#' 
#' @return
#' 
#' An object of class "trtsel," which is a list containing:
#' 
#' \item{model.fit }{ A list containing "coefficients" -- a 4 x 4 matrix with
#' columns for coefficient estimates, standard errors, t-statistics, and
#' two-sided p-values.  "cohort.attributes" -- the vector of cohort.attributes
#' provided "study.design" -- character string of study.design provided }
#' \item{derived.data }{ A data.frame with "event", "trt", "marker",
#' "fittedrisk.t0" (risk estimates given no treatment), "fittedrisk.t1" (risk
#' estimates given treatment), "trt.effect" (treatment effect estimates), and
#' "marker.neg" (indicator of trt.effect < thresh) columns.  }
#' \item{functions}{ For internal package use only }
#' @seealso \code{\link{plot.trtsel}} for plotting risk curves and more,
#' \code{\link{eval.trtsel}} for evaluating marker performance,
#' \code{\link{calibrate.trtsel}} for assessing model calibration, and
#' \code{\link{compare.trtsel}} to compare two trtsel object.
#' @references Janes, Holly; Brown, Marshall D; Pepe, Margaret; Huang, Ying;
#' "An Approach to Evaluating and Comparing Biomarkers for Patient Treatment
#' Selection" The International Journal of Biostatistics. Volume 0, Issue 0,
#' ISSN (Online) 1557-4679, ISSN (Print) 2194-573X, DOI: 10.1515/ijb-2012-0052,
#' April 2014
#' @examples
#' 
#' 
#' data(tsdata)
#' 
#' ###########################
#' ## Create trtsel objects 
#' ###########################
#' 
# trtsel.Y1 <- trtsel(event ~ Y1*trt, 
#                    treatment.name = "trt", 
#                    data = tsdata, 
#                    study.design = "randomized cohort",
#                    link = "logit", 
#                    default.trt = "trt all")
#
#' trtsel.Y1
#' 
# trtsel.Y2 <- trtsel(event ~ Y2*trt, 
#                    treatment.name = "trt", 
#                    data = tsdata, 
#                    study.design = "randomized cohort",
#                    link = "logit", 
#                    default.trt = "trt all")
#' trtsel.Y2
#' 
#' 
#' # calculate fitted risks using a logistic model 
#' #(one can use any model here, the point is that the fitted risks are provided )
#' mymod <- glm(event~trt*Y2, data= tsdata, family = binomial("logit"))
#' 
#' tsdata$fitted.t0 <- predict(mymod, newdata=data.frame(trt = 0, Y2 = tsdata$Y2), type = "response")
#' tsdata$fitted.t1 <- predict(mymod, newdata=data.frame(trt = 1, Y2 = tsdata$Y2), type = "response")
#' 
#' #all bootstrapping done using this object will be conditional on the model fit. 
#' 
#' myfitted.trtsel <- trtsel( event~trt, treatment.name = "trt", 
#'                            data = tsdata,
#'                            fittedrisk.t0 = "fitted.t0",
#'                            fittedrisk.t1 = "fitted.t1",
#'                            study.design = "randomized cohort", 
#'                            default.trt = "trt all")
#' 
#' 
#' 
#' @import survival 
#' @export trtsel
trtsel <-
function(formula, treatment.name, data, 
         fittedrisk.t0 = NULL, fittedrisk.t1 = NULL, thresh=0, 
         prediction.time = NULL, 
         study.design = c("randomized cohort", "nested case-control", "stratified nested case-control"), 
         cohort.attributes = NULL, 
         link = c("logit", "probit", "cauchit", "log", "cloglog"), 
         default.trt = c("trt all", "trt none") ){
  
  call <- match.call() 
  marker.bounds = NULL #depricated, we don't allow for bounded markers now 
  #check data frame 
  if(!is.data.frame(data)){stop('data must be a data.frame')}
  
  #checkc to see if all variables are present in data
  tmpnames <- c(treatment.name, fittedrisk.t0, fittedrisk.t1, all.vars(formula))
  if(!all(is.element(tmpnames, names(data)))) stop(paste("'", tmpnames[which(!is.element(tmpnames, names(data)))], "' cannot be found in data.frame provided", sep = ""))
  
  #only keep complete cases 
  mycomplete <- complete.cases(data[,tmpnames]); 
  
  #check for missing data and throw it out, print a warning
  if(nrow(data)!=sum(mycomplete)){
    warning(paste(nrow(data)-sum(mycomplete), "observation(s) were removed due to missing data \n New sample size is now:", sum(mycomplete)))
    data <- data[mycomplete,]
    
  }
  ## Error Checking
  
  event.name <- as.character(formula[[2]])
  #define outcome 
  if(event.name[[1]] == "Surv"){
    outcome = "time-to-event" 
    link = "time-to-event"
    if(is.null(prediction.time)) stop("'prediction.time' must be set for a time-to-event outcome.")
    #make sure prediction time is reasonable value? 
    #...
  }else{
    outcome = "binary"
    if(!is.numeric(data[[event.name]]) | !all(is.element(unique(data[[event.name]]), c(0,1)))) stop( "Binary outcome must be a numeric vector with elements 1 or 0")
    link = match.arg(link)
    if(!is.element(link, c("logit", "probit", "cauchit", "log", "cloglog"))) stop("link must be one of ''logit'', ''probit'', ''cauchit'', ''log'', ''cloglog''")
  }
  #check event
  


  trt <- data[[treatment.name]]
  if(!is.numeric(trt) | !all(is.element(unique(trt), c(0,1)))) stop( "trt must be a numeric vector with elements 1 or 0") 
  
  ## if there is only one marker in the model, we keep it around, otherwise set marker to null 
  
  marker.names = all.vars(formula)[!is.element( all.vars(formula), c(event.name, treatment.name)) ]
  if(length(marker.names)==1){ 
    
    marker = data[[marker.names]]
    if(!is.numeric(marker)) stop("For a model with a single marker, the marker must be numeric.")
  }else{
    marker = NULL
  }
  
  #if(!is.numeric(data[[marker.name]])) stop( "marker must be a numeric") 
  #if(length(marker) >1){ stop("only a single marker is allowed")}
  default.trt <- match.arg(default.trt)
  if(!is.element(default.trt, c("trt all", "trt none"))){ stop( "default.trt must be either 'trt all' or 'trt none'")}
  ## End Error Checking

  d <- thresh
  study.design = match.arg(study.design) 
  
#find out which bootstrapping functions to use based on type
  if( substr(study.design,1,4)  == "rand" ) { 
   
    boot.sample <- boot.sample.cohort 

    if(outcome == "binary"){
      get.summary.measures <- get.summary.measures.cohort
      get.F <- get.F.cohort
      }
    
    if(outcome == "time-to-event"){
      get.summary.measures <- get.summary.measures.cohort.survival
      get.F <- get.F.cohort.survival
    }

    if(length(cohort.attributes) >0) warning("study.design = ''randomized cohort'', but cohort.attributes assigned: cohort.attributes will be ignored"); cohort.attributes=NULL;
     #just passing null value here 
    

  }else if( substr(study.design, 1, 4) =="nest") { 
    if(outcome == "time-to-event") stop("study.design must be ''randomized cohort'' for time-to-event outcomes.")
    if(link!="logit") warning("when study.design is ''nested case-control'' only link=''logit'' is allowed, setting link = ''logit''"); link = "logit"
    boot.sample <- boot.sample.case.control 
    get.F <- get.F.case.control
    get.summary.measures <- get.summary.measures.case.control

    if(length(cohort.attributes) != 4){ 
    
    stop("cohort.attributes not specified correctly, when study.design=''nested case-control'': \n  
          cohort.attributes = c(cohort sample size, 
          proportion treated in cohort (Pr(trt==1)),
          adverse event prevalance in cohort (Pr(event==1)),
          fraction of cases sampled from cohort)\n")
    }

    if(length(trt) > cohort.attributes[1]) stop("Sub-cohort sample size larger than input cohort sample size, please check cohort.attributes[1]")

    if( any(cohort.attributes[-1]<0 ) | any(cohort.attributes[-1] > 1)) stop("Probabilities in cohort.attributes are not in (0,1), please check cohort.attributes")

   # if(length(cohort.attributes) == 3) cohort.attributes <- c(cohort.attributes, 1) # adding f = 1 by default
    #cohort attributes is c(N, Pr(trt = 1), Pr(event = 1) ) 

  }
  else if( substr(study.design, 1, 5) =="strat") { 
    if(outcome == "time-to-event") stop("study.design must be ''randomized cohort'' for time-to-event outcomes.")
    if(link!="logit") warning("when study.design is ''stratified nested case-control'' only link=''logit'' is allowed, setting link = ''logit''"); link = "logit"
    boot.sample <- boot.sample.stratified.case.control
    get.F <- get.F.stratified.case.control  
    get.summary.measures <- get.summary.measures.stratified.case.control
   # if(length(cohort.attributes) == 5) cohort.attributes <- c(cohort.attributes, 1, 1)
   if(length(cohort.attributes) != 6){ 
    
    stop("cohort.attributes not specified correctly, when study.design=''nested case-control'': \n  
          cohort.attributes = c(cohort sample size, 
                              Pr(trt==0 & event==0) in cohort, 
                              Pr(trt==0 & event==1) in cohort, 
                              Pr(trt==1 & event==0) in cohort, 
                              fraction of cases with trt == 0 sampled from cohort, 
                              fraction of cases with trt == 1 sampled from cohort )\n ")
    }
    
    ca <- cohort.attributes
    cohort.attributes = c(ca[1], ca[2], ca[3], ca[4], 1-(ca[2]+ca[3]+ca[4]), ca[5], ca[6])


    if(length(trt) > ca[1]) stop("Sub-cohort sample size larger than input cohort sample size, please check cohort.attributes[1]")
    if( any(cohort.attributes[-1]<0) | any(cohort.attributes[-1] > 1)) stop("Probabilities in cohort.attributes are not in (0,1), please check cohort.attributes. \n Is Pr(trt==0 & event==0) + Pr(trt==0 & event==1) + Pr(trt==1 & event==0) > 1?")

  }
  else { stop("study.design not specified correctly, must be one of ''randomized cohort'', ''nested case-control'', or ''stratified nested case-control''") }

  
  functions <- list("boot.sample" = boot.sample, "get.F" = get.F, "get.summary.measures" = get.summary.measures)

  rho = cohort.attributes



  if(!is.null(fittedrisk.t0)) {
    fitted_risk_t0 = data[[fittedrisk.t0]]
    if(!is.null(marker)) warning("fitted risks provided: marker data will be ignored")
    marker <- NULL
    link <- "risks_provided"
    if(is.null(fittedrisk.t1)) stop("must provide fitted risk for trt = 1 as well")
    if(any(fitted_risk_t0 > 1) | any(fitted_risk_t0 <0)) stop("fitted risks for trt = 0 are outside of bounds (0,1)")
    
  }
  
  if(!is.null(fittedrisk.t1)){
    
   fitted_risk_t1 = data[[fittedrisk.t1]]
   if(is.null(fitted_risk_t0)) stop("must provide fitted risk for trt = 0 as well")
   if(any(fitted_risk_t1 > 1) | any(fitted_risk_t1 <0)) stop("fitted risks for trt = 1 are outside of bounds (0,1)")
   
  }  

  # model.fit
  #returns null if risks are provided




  
  # derived.data

  #now that we allow for different link functions for "randomized cohorts", we need to get the fitted risks using the coefs we calculated

  if(link == "risks_provided"){
    linkinvfun <- NULL
  }else if(link == "time-to-event"){
   
    coxfit <- do.call(coxph, list(formula,data))
    
    fitted_risk_t0 <- get.risk.t_coxph(coxfit, treatment.name, data, prediction.time, t = 0)
    fitted_risk_t1 <- get.risk.t_coxph(coxfit, treatment.name, data, prediction.time, t = 1)
    #we still need to incorporate the nelson aalen baseline haz to get absolute risk at t = 'prediction.time'
    coef <- summary(coxfit)$coefficients
    }else{
      #binary outcome
      coef <- get.coef( formula = formula, 
                        treatment.name = treatment.name,
                        data = data,
                        study.design = study.design, 
                        rho = rho, 
                        link = link)
      
    linkinvfun <- binomial(link = link)$linkinv
    fitted_risk_t0 <- get.risk.t(coef[,1], formula, treatment.name, data, linkinvfun, t = 0)
    fitted_risk_t1 <- get.risk.t(coef[,1], formula, treatment.name, data, linkinvfun, t = 1)
    }
  
  if(isTRUE(all.equal(fitted_risk_t0, fitted_risk_t1))) warning("fitted risks are the same under each treatment arm so treatment effects are all zero, \n did you forget to include interactions in your risk model?")
  
  model.fit <- list( "coefficients" = coef, "cohort.attributes" = rho, 
                     "study.design" = study.design, 
                     "link" = link, "outcome" = outcome, 
                     "thresh" = d, 
                     "marker.names"  = marker.names)
  
  
  
  trt.effect <- fitted_risk_t0 - fitted_risk_t1
  if(length(unique(marker))==2){ 
    #find which value of the marker is marker negative

    Meantrteff.y1 <- mean(trt.effect[marker == unique(marker)[1]])
    Meantrteff.y2 <- mean(trt.effect[marker == unique(marker)[2]])
  
   # if(Meantrteff.y1 < d & Meantrteff.y2 <d){ stop()}
    if(Meantrteff.y1 > Meantrteff.y2){ 
      marker.neg <- as.numeric(marker==unique(marker)[2])
      model.fit$disc.marker.neg = unique(marker)[2]
      }else{
      marker.neg <- as.numeric(marker==unique(marker)[1])
      model.fit$disc.marker.neg = unique(marker)[1]
    }
  }else{
    marker.neg <- ifelse( trt.effect < d, 1, 0) # indicator of being marker negative
  }

  ## if we dont use marker; we use fitted risks
  derived.data <- data.frame( data[,c(all.vars(formula))] , 
              fittedrisk.t0 = fitted_risk_t0, 
              fittedrisk.t1 = fitted_risk_t1,
              trt.effect = trt.effect)
  
  derived.data$marker = marker 
  if(default.trt =="trt all"){
      derived.data$marker.neg <- marker.neg
      
    }else{
      marker.pos <- 1-marker.neg # indicator of being marker negative
      derived.data$marker.pos <- marker.pos = marker.pos
    }
    
  #need to add sampling weights to time-to-event outcome 
  if(outcome == "time-to-event"){
  
    tmp <- with(data, eval(formula[[2]]))
    wi = get.censoring.weights(ti = prediction.time, stime = tmp[,1], status = tmp[,2] )
    derived.data$censoring.weights  <- wi
  }
   
  out <- list(derived.data=derived.data, 
              formula = formula, 
              treatment.name = treatment.name, 
              prediction.time = prediction.time, 
              model.fit = model.fit, 
              functions  = functions,
              default.trt = default.trt, 
              call = call)
  class(out) = "trtsel"
  
  out

}

