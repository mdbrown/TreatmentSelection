#' method for calibrating trtsel objects 
#' @seealso \code{\link{calibrate.trtsel}}
#' @param x object
#' @param \dots not used  
#' @export
calibrate <- function(x, ...){ UseMethod("calibrate")}

#' assess model calibration of a trtsel object
#' 
#' Assess calibration of fitted models for risk and treatment effect given
#' marker.  Plots are used to compare observed vs. fitted risks and treatment
#' effects and Hosmer-Lemeshow goodness-of-fit tests are reported.  An object
#' of class "trtsel" must first be created using the function "trtsel" by
#' supplying a data.frame containing marker, treatment, and event status
#' information.
#' 
#' @aliases calibrate calibrate.trtsel
#' @param x An object of class "trtsel", created by using the function
#' "trtsel."
#' @param groups Number of groups; observations are split into groups based on
#' quantiles of predicted risk or treatment effect, depending on plot.type. For
#' plot.type = "treatment effect", observations are split into groups based on
#' quantile of predicted treatment effect; for plot.type= "calibration",
#' "risk.t0", or "risk.t1", and for the Hosmer-Lemshow test statistic,
#' observations on each treatment are split into groups based on quantile of
#' predicted risk.  The default value is 10.
#' @param plot.type Which type of plot to produce. Options are "calibration"
#' (default), which plots average predicted vs. observed risks on a log scale;
#' "risk.t1" and "risk.t0" which overlays observed risks on fitted risk curves
#' for T = 1 and T = 0 subjects, respectively; and "treatment effect" which
#' overlays observed treatment effects on the fitted treatment effect curve.
#' @param trt.names A vector of length 2 indicating the names for the two
#' treatment options, T= 1 and T = 0, respectively, for the plot legend. This
#' option is only used when plot.type="calibration". The default value is
#' c("Treatment", "No Treatment").
#' @param line.color color for lines in calibration plots. 
#' @param point.color  color for points in calibration plots. 
#' @param xlab A label for the x-axis. Default values depend on plot.type. Only
#' applies if plot.type is specified.
#' @param ylab A label for the y-axis. Default values depend on plot.type.
#' Only applies if plot.type is specified.
#' @param xlim The limits for the x-axisof the plot, in the form
#' c(lower,upper). Only applies if plot.type is specified.
#' @param ylim The limits for the y-axis of the plot, in the form
#' c(lower,upper). Only applies if plot.type is specified.
#' @param main The main title for the plot.  Only applies if plot.type is
#' specified.
#' @return A list with the following components:
#' \item{HL.TestStat}{Hosmer-Lemeshow test statistic for assessing fit of the
#' risk models for the T = 0 and T = 1 groups.} \item{p.value}{P-values for the
#' Hosmer-Lemeshow tests in each treatment group.} \item{Df}{Degrees of freedom
#' for the chi-square distribution used to generate a p-value for the
#' Hosmer-Lemeshow chi-square test. This equals "groups" - 2. } \item{plot}{If
#' plot output was created, the ggplot plotting object.}
#' @seealso \code{\link{trtsel}} for creating trtsel objects,
#' \code{\link{plot.trtsel}} for plotting risk curves and more,
#' \code{\link{evaluate.trtsel}} for evaluating marker performance, and
#' \code{\link{compare.trtsel}} to compare two trtsel object.
#' @examples
#' 
#' data(tsdata)
#' 
#' ###########################
#' ## Create trtsel objects 
#' ###########################
#' trtsel.Y1 <- trtsel(event ~ Y1*trt, 
#'                    treatment.name = "trt", 
#'                    data = tsdata, 
#'                    study.design = "RCT",
#'                    link = "logit", 
#'                    default.trt = "trt all")
#'
#' trtsel.Y1
#' 

#' ##############################
#' ## Assess model calibration
#' ##############################
#' 
#'  
#' cali.Y1 <- calibrate(trtsel.Y1, plot.type = "calibration")
#' cali.Y1
#' 
#' # A "treatment effect" plot 
#' calibrate(trtsel.Y1, plot.type = "treatment effect")
#' 
#' 
#' @importFrom binom binom.confint
#' @method calibrate trtsel 
#' @export 
#' 
calibrate.trtsel <-
function( x, groups = 10, plot.type = "calibration", trt.names = c("Treatment", "No Treatment"), line.color = "black", point.color = "grey10", main = NULL, ylim = NULL, xlim = NULL, ylab = NULL, xlab=NULL){

  if(!is.trtsel(x)) stop("x must be an object of class 'trtsel' created by using the function 'trtsel' see ?trtsel for more help")
  if(!is.null(x$model.fit$disc.rec.no.trt)) stop("Calibration not supported for a discrete marker")
  if(!is.null(x$model.fit$disc.rec.no.trt)) stop("Calibration not supported for a discrete marker")

  event.name = as.character(x$formula[[2]])
  lower <- upper <- 
  if( x$model.fit$link == "time-to-event"){
    event.name = x$formula[[2]]
    mysurv <- with(x$derived.data, eval(event.name))
    event <- mysurv[,2]
    stime <- mysurv[,1]
  }else{
    event.name <- as.character(x$formula[[2]])
    event <- x$derived.data[[event.name]]
  }
  
  
  trt <- x$derived.data[[x$treatment.name]]
  n <- length(trt)
  if(!is.numeric(groups)) stop("groups must be an integer")
  
  if(groups < 2) stop("Must have more than 2 groups!")

  if(!is.element(plot.type, c("calibration", "risk.t0", "risk.t1", "treatment effect", NA, "none"))){ 

    stop("available plot.type options are \"calibration\", \"risk.t0\", \"risk.t1\", \"treatment effect\", \"none\" or NA") 
  }

  fittedrisk.t0 <- x$derived.data$fittedrisk.t0
  fittedrisk.t1 <- x$derived.data$fittedrisk.t1
  fitteddelta   <- x$derived.data$trt.effect

  fittedrisk.c.t0 <- x$derived.data$fittedrisk.t0[trt==0] #fitted risk conditional on trt
  fittedrisk.c.t1 <- x$derived.data$fittedrisk.t1[trt==1]



  D.t0 <- event[trt==0]
  D.t1 <- event[trt==1]

  
  trt.t0 <- trt[trt==0]
  n.t0 <- sum(trt==0)

  trt.t1 <- trt[trt==1]
  n.t1 <- sum(trt==1)

  rho  <- x$model.fit$cohort.attributes
  study.design <- x$model.fit$study.design

 ###
  #find out which functions to use based on type

  # calculate the empirical CDF...we use this to get the cut points for the HL statistic and for plotting the pred curves/trt effect curves. 

  if( substr(study.design, 1,4) == "rand" ) { 

    F.risk.t0 <- get.F.cohort( fittedrisk.c.t0, D.t0, trt.t0, rho, return.fun=TRUE)
    F.risk.t1 <- get.F.cohort( fittedrisk.c.t1, D.t1, trt.t1, rho, return.fun=TRUE)
    F.delta   <- get.F.cohort( fitteddelta    , event, trt, rho, return.fun=TRUE)


  }else if( substr(study.design, 1, 4) =="nest") { 

    F.risk.t0 <- get.F.case.control( fittedrisk.c.t0, D.t0, trt.t0, rho, return.fun=TRUE)
    F.risk.t1 <- get.F.case.control( fittedrisk.c.t1, D.t1, trt.t1, rho, return.fun=TRUE)
    F.delta   <- get.F.case.control( fitteddelta    , event, trt, rho, return.fun=TRUE)
    
  }else if( substr(study.design, 1,5) =="strat") { 

    #we split up by treatment here, so we cant calculate F in the same way for strat. cc. 
    # instead we calculate F(marker | trt = tmp.trt)
    get.F.tmp <- function(marker,event,trt,rho, tmp.trt){

      tmp.index <- ifelse(tmp.trt==0, 1, 2) 
      if(tmp.trt==0){
        Pr.D1.trt <- (rho[3])/(rho[3]+rho[2])
        Pr.D0.trt <- 1-Pr.D1.trt
      }else if(tmp.trt==1){
        Pr.D1.trt <- (rho[5])/(rho[5]+rho[4])
        Pr.D0.trt <- 1-Pr.D1.trt
      }
      marker.D1.trt <- marker[event==1 & trt==tmp.trt]
      marker.D0.trt <- marker[event==0 & trt==tmp.trt]
                           
      FY.D1.trt <- ecdf(marker.D1.trt) 
      FY.D0.trt <- ecdf(marker.D0.trt)    

      function(x) FY.D1.trt(x)*(Pr.D1.trt) + FY.D0.trt(x)*(Pr.D0.trt) 

    } 
    

    F.risk.t0 <- get.F.tmp( fittedrisk.c.t0, D.t0, trt.t0, rho, tmp.trt = 0)
    F.risk.t1 <- get.F.tmp( fittedrisk.c.t1, D.t1, trt.t1, rho, tmp.trt = 1)
    F.delta   <- get.F.stratified.case.control( fitteddelta, event, trt, rho, return.fun=TRUE)



  
  }else { stop("study.design not specified correctly") }




## Calculate observed and expected risk for the plots and the homer - lemeshow statistic  


 #find the cutoff points based on the quantiles of F.risk...for cohort this is just the quantiles of the observed risks, however for 
 # scc and cc, this is trickier because F.risk is a weighted average

breaks.t0 <- sort(fittedrisk.c.t0)[sum.I( seq(0, 1, 1/groups), "<",F.risk.t0(fittedrisk.c.t0))]
breaks.t1 <- sort(fittedrisk.c.t1)[sum.I( seq(0, 1, 1/groups), "<",F.risk.t1(fittedrisk.c.t1))]
breaks.delta <- sort(fitteddelta)[ sum.I( seq(0, 1, 1/groups), "<",F.delta(fitteddelta))]

 #check to make sure breaks are unique, if not, we need to reduce the number of groups that we are using

 if(!(length(unique(round(breaks.t0, 5)))==(groups+1) & length(unique(round(breaks.t1, 5))) == (groups+1) & length(unique(round(breaks.delta, 5)))==(groups+1))){ 
   
   stop("Error: Too many groups, cut points are not unique. Please reduce number of groups")
   
 }

#groups 
cut.t0      <- cut( fittedrisk.c.t0, breaks = breaks.t0, include.lowest = TRUE)
cut.t1      <- cut( fittedrisk.c.t1, breaks = breaks.t1, include.lowest = TRUE)
cut.delta   <- cut( fitteddelta, breaks = breaks.delta, include.lowest = TRUE)

#observed risk 
if( x$model.fit$link == "time-to-event"){
  
  sfit.t0 <- summary(survfit(Surv(stime[trt==0], event[trt==0]==1)~cut.t0, se.fit = TRUE ), extend = TRUE, times = x$prediction.time)
  sfit.t1 <- summary(survfit(Surv(stime[trt==1], event[trt==1]==1)~cut.t1, se.fit = TRUE ), extend = TRUE, times = x$prediction.time)
  obs.risk.t0 <- 1- sfit.t0$surv
  obs.risk.t1 <- 1- sfit.t1$surv

  sfit.t0.delta <- summary(survfit(Surv(stime[trt==0], event[trt==0]==1)~cut.delta[trt==0], se.fit = TRUE ), times = x$prediction.time)
  sfit.t1.delta <- summary(survfit(Surv(stime[trt==1], event[trt==1]==1)~cut.delta[trt==1], se.fit = TRUE ), times = x$prediction.time)
  obs.risk.t0.tmp <- 1- sfit.t0.delta$surv
  obs.risk.t1.tmp <- 1- sfit.t1.delta$surv
  

  
}else{
  obs.risk.t0 <- aggregate( D.t0, by = list(cut.t0), FUN = "mean")$x
  obs.risk.t1 <- aggregate( D.t1, by = list(cut.t1), FUN = "mean")$x
  obs.risk.t1.tmp <- aggregate( D.t1, by = list(cut.delta[trt==1]), FUN = "mean")$x  
  obs.risk.t0.tmp <- aggregate( D.t0, by = list(cut.delta[trt==0]), FUN = "mean")$x
  
}

 #expected risk 
 exp.risk.t0 <- aggregate( fittedrisk.c.t0, by = list(cut.t0), FUN = "mean")$x
 ng.t0       <- as.numeric(unlist(table(cut.t0)))

 if(any(ng.t0<5)) warning(paste("For Treatment = 0,", sum(ng.t0<5), "groups have less than 5 observations."))

 #trt = 1
 exp.risk.t1 <- aggregate( fittedrisk.c.t1, by = list(cut.t1), FUN = "mean")$x
 ng.t1       <- as.numeric(unlist(table(cut.t1))) 

 if(any(ng.t1<5)) warning(paste("For Treatment = 1,", sum(ng.t1<5), "groups have less than 5 observations."))
 
 #Delta
exp.delta   <- aggregate( fitteddelta, by = list(cut.delta), FUN = "mean")$x



 #make sure there are at least one observation from each treatment arm in each group

 if(!(length(obs.risk.t0.tmp)==length(obs.risk.t1.tmp))) stop("Failure to observe at least one individual per treatment arm in each group. Please reduce the number of groups")

 obs.delta       <- obs.risk.t0.tmp - obs.risk.t1.tmp
 
 ng.delta        <- as.numeric(unlist(table(cut.delta))) 

 if(any(ng.delta<5)) warning(paste("For observed treatment effects,", sum(ng.delta<5), "groups have less than 5 observations."))

 
 
 if(substr(study.design, 1, 4) == "nest"){
    obs.risk.t0 = expit(logit(obs.risk.t0) + logit(rho[3]) - logit(mean(event)))
    obs.risk.t1 = expit(logit(obs.risk.t1) + logit(rho[3]) - logit(mean(event)))
    obs.delta   = expit(logit(obs.risk.t1.tmp)+ logit(rho[3]) - logit(mean(event))) - 
                  expit(logit(obs.risk.t0.tmp)+ logit(rho[3]) - logit(mean(event)))
    
 }else if(substr(study.design, 1, 5) =="strat"){

   
   

    Pr.D1.givT0 <- rho[3]/(rho[2]+rho[3])
    Pr.D1.givT1 <- rho[5]/(rho[4]+rho[5])
    obs.risk.t0 = expit(logit(obs.risk.t0) + logit(Pr.D1.givT0) - logit(mean(event[trt==0])))
    obs.risk.t1 = expit(logit(obs.risk.t1) + logit(Pr.D1.givT1) - logit(mean(event[trt==1])))
    obs.delta = expit(logit(obs.risk.t1.tmp) + logit(Pr.D1.givT1) - logit(mean(event[trt==1]))) - expit(logit(obs.risk.t0.tmp) + logit(Pr.D1.givT0) - logit(mean(event[trt==0])))                               
                                            
 }
##calculate Hosmer - Lemeshow test stastistitic a



if(study.design=="randomized cohort"){

  hl.t0 <- sum( ng.t0 * ( obs.risk.t0 - exp.risk.t0)^2 / (exp.risk.t0*(1-exp.risk.t0))) 
  hl.t1 <- sum( ng.t1 * ( obs.risk.t1 - exp.risk.t1)^2 / (exp.risk.t1*(1-exp.risk.t1)))

}else{
  if(x$model.fit$link == "risks_provided") stop("cannot calculate Hosmer Lemeshow statistic when fitted risks are provided and study design is not cohort")
  marker <- x$derived.data$marker
  risk.naive.all <- fitted(glm(x$formula, family = binomial(link = x$model.fit$link)))
  
  risk.naive.t0.all <- risk.naive.all[trt==0]
  risk.naive.t1.all <- risk.naive.all[trt==1]
  
  #now sum across groups 
  exp.risk.naive.t0 <- aggregate( risk.naive.t0.all, by = list(cut.t0), FUN = "mean")$x
  exp.risk.naive.t1 <- aggregate( risk.naive.t1.all, by = list(cut.t1), FUN = "mean")$x
  
 
  hl.t0 <- sum( ng.t0 * ( obs.risk.t0 - exp.risk.t0)^2 / ( (exp.risk.t0^2*(1-exp.risk.t0)^2)/(exp.risk.naive.t0*(1-exp.risk.naive.t0)) ) )
  hl.t1 <- sum( ng.t1 * ( obs.risk.t1 - exp.risk.t1)^2 / ( (exp.risk.t1^2*(1-exp.risk.t1)^2)/(exp.risk.naive.t1*(1-exp.risk.naive.t1)) ) )

  

}

Df <- groups - 2 

pval.t0    <- 1 - pchisq( hl.t0, Df)
pval.t1    <- 1 - pchisq( hl.t1, Df)
#pval.delta <- 1 - pchisq( hl.delta, g-2)


##plot
if(is.element(plot.type, c("calibration", "risk.t0", "risk.t1", "treatment effect"))){
#set boundaries 
min.risk <- min(c(fittedrisk.c.t0, fittedrisk.c.t1))
max.risk <- max(c(fittedrisk.c.t0, fittedrisk.c.t1))
    cen <- mean(c(min.risk, max.risk))
    ran <- max.risk - min.risk
    ran <- ran*1.1
    mylim <- c(cen-ran/2, cen+ran/2)
   }
#
## to appease check
  observedRisk <- expectedRisk <- F.risk <- risk <- y <- NULL; 
  
  
if(is.element(plot.type, "calibration")){
  

   if(!is.null(xlim)){ 
      if(any(xlim <0)) stop("Parameters of xlim must be > 0 due to log scaling") 
   } 
   if(!is.null(ylim)){ 
      if(any(ylim <0)) stop("Parameters of ylim must be > 0 due to log scaling") 
   } 

   if(is.null(xlab)) xlab <- "observed risk"
   if(is.null(ylab)) ylab <- "average predicted risk"
  

   if(is.null(main)) main <- "Calibration plot"



 
 data <- data.frame("observedRisk" = c(obs.risk.t0, obs.risk.t1),
                    "expectedRisk" = c(exp.risk.t0, exp.risk.t1), 
                    "trt" = rep(c(0,1), c(length(obs.risk.t0), length(obs.risk.t1))) )
   data <- subset(data, observedRisk >0)
   data <- subset(data, expectedRisk >0)
 
 p <- ggplot(data = data, aes(x= observedRisk, y = expectedRisk, shape = factor(trt)))
 p <- p + coord_trans(x = "log", y = "log") +
   scale_shape_discrete("", labels = trt.names) + 
   ylab(ylab) + xlab(xlab) + ggtitle(main) +# theme( text = element_text(size=16)) +
   geom_line(aes(x = observedRisk, y = observedRisk), colour = "grey50", linetype = 2 ) + 
     geom_point( color = point.color)
   
    if(!is.null(xlim)){
    #  xaxis <- round(seq(from = xlim[1], to=xlim[2], length.out=5), 2)
      if(xlim[1]==0){
        warning("due to log scaling, the lower bound of xlim must be > 0, changing xlim[1] <- .01")
        xlim[1] <- .01
        
      }
      p <- p+ scale_x_continuous(limits = xlim)
    }
    if(!is.null(ylim)){
     # yaxis <- round(seq(from = ylim[1], to=ylim[2], length.out=5), 2)
      if(ylim[1]==0){
        warning("due to log scaling, the lower bound of ylim must be > 0, changing ylim[1] <- .01")
        ylim[1] <- .01
        
      }
      p <- p+ scale_y_continuous( limits = ylim)
    }
      
   
 #p <- p + geom_abline()
   #p <- p+ geom_segment(aes(x = 0.0004, y =0.004, xend = 1, yend = 1 ))

 print(p)
}
  
  
  
if( is.element(plot.type, "risk.t0")) { 
# trt = 0

   if(is.null(xlab)) xlab <- "% population below risk"
   if(is.null(ylab)) ylab <- "risk"
   if(is.null(xlim)) xlim <- c(0,100)
  # if(is.null(ylim)) ylim <- mylim
   if(is.null(main)) main <- "Risk curve for non treated individuals"
   
   
   data = data.frame(F.risk = F.risk.t0(sort(fittedrisk.c.t0))*100, risk = sort(fittedrisk.c.t0))
   p <- ggplot() + geom_step( data = data, direction="vh", color = line.color,  aes(x = F.risk, y = risk))
   

   obsdata <- data.frame(x = (1:groups/groups - 1/(2*groups))*100, y= obs.risk.t0)
   
   if( x$model.fit$link == "time-to-event"){
     obsdata$upper <- 1- sfit.t0$lower
     obsdata$lower <- 1- sfit.t0$upper
   }else{
     obsdata$upper <-  binom.confint(obsdata$y*ng.t0, ng.t0, methods = "wilson")$upper
     obsdata$lower <- binom.confint(obsdata$y*ng.t0, ng.t0, methods = "wilson")$lower
   }
   
   p <- p +geom_errorbar(data = obsdata, color  = point.color,  aes(ymin = lower, ymax = upper, x = x), width = 2)+
     geom_point(data = obsdata, aes(x = x, y = y), color = point.color) 
   p <- p + ylab(ylab) + xlab(xlab) + ggtitle(main) + #theme( text = element_text(size=16)) 
   if(!is.null(xlim)) p <- p + xlim(xlim)
   if(!is.null(ylim)) p <- p + ylim(ylim)
   print(p)
   
   
}

if(is.element(plot.type, "risk.t1")) { 
# trt = 1


if(is.null(xlab)) xlab <- "% population below risk"
   if(is.null(ylab)) ylab <- "risk"
   if(is.null(xlim)) xlim <- c(0,100)
  #if(is.null(ylim)) ylim <- mylim
   if(is.null(main)) main <- "Risk curve for treated individuals"

data = data.frame(F.risk = F.risk.t1(sort(fittedrisk.c.t1))*100, risk = sort(fittedrisk.c.t1))
p <- ggplot() + geom_step( data = data,  direction="vh", color = line.color, aes(x = F.risk, y = risk))




obsdata <- data.frame(x = (1:groups/groups - 1/(2*groups))*100, y= obs.risk.t1)

if( x$model.fit$link == "time-to-event"){
  obsdata$upper <- 1- sfit.t1$lower
  obsdata$lower <- 1- sfit.t1$upper
}else{
  obsdata$upper <-  binom.confint(obsdata$y*ng.t1, ng.t1, methods = "wilson")$upper
  obsdata$lower <- binom.confint(obsdata$y*ng.t1, ng.t1, methods = "wilson")$lower
}

p <- p +geom_errorbar(data = obsdata,  color  = point.color, aes(ymin = lower, ymax = upper, x = x), width = 2)+
  geom_point(data = obsdata, aes(x = x, y = y),  color  = point.color)#, size = 4) 

p <- p + ylab(ylab) + xlab(xlab) + ggtitle(main) + #theme( text = element_text(size=16)) 
if(!is.null(xlim)) p <- p + xlim(xlim)
if(!is.null(ylim)) p <- p + ylim(ylim)
print(p)


}

if( is.element("treatment effect", plot.type)) { 

   if(is.null(xlab)) xlab <- "% population below treatment effect"
   if(is.null(ylab)) ylab <- "treatment effect"
   if(is.null(xlim)) xlim <- c(0,100)
  # if(is.null(ylim)) ylim <- mylim
   if(is.null(main)) main <- "Treatment effect distribution"


data = data.frame(F.risk = F.delta(sort(fitteddelta))*100, risk = sort(fitteddelta))
p <-p <- ggplot() + geom_step( data = data,  direction="vh", color = line.color, aes(x = F.risk, y = risk))



obsdata <- data.frame(x = (1:groups/groups - 1/(2*groups))*100, y= obs.delta)

if( x$model.fit$link == "time-to-event"){

  obsdata$var <- sfit.t0.delta$std.err^2 + sfit.t1.delta$std.err^2

}else{
  obsdata$var <- obs.risk.t1.tmp*(1-obs.risk.t1.tmp)/ng.t1 + obs.risk.t0.tmp*(1-obs.risk.t0.tmp)/ng.t0
}


obsdata$upper <- obsdata$y + qnorm(.975)*sqrt(obsdata$var)
obsdata$lower <- obsdata$y + qnorm(.025)*sqrt(obsdata$var)

p <- p +geom_errorbar(data = obsdata,  color  = point.color, aes(ymin = lower, ymax = upper, x = x), width = 2)+
  geom_point(data = obsdata, aes(x = x, y = y),  color  = point.color)#, size = 4) 

p <- p + ylab(ylab) + xlab(xlab) + ggtitle(main) #+ theme( text = element_text(size=16)) 
if(!is.null(xlim)) p <- p + xlim(xlim)
if(!is.null(ylim)) p <- p + ylim(ylim)
print(p)






} 
  
 #reset plot parameters
if(is.element(plot.type, c("calibration", "risk.t0", "risk.t1", "treatment effect"))){

# par(mar = old.mar)
# plot.data <- data.frame(cbind("group" = rep(1:groups, 2), "F.risk"= rep(1:groups/groups - 1/(2*groups), 2), "observed" = c(obs.risk.t0, obs.risk.t1), "expected" = c(exp.risk.t0, exp.risk.t1), "treatment" = c(rep(0, length(obs.risk.t0)),rep(1, length(obs.risk.t1)) )))
 
}else{
 #plot.data=NULL
  p = NULL
}
res <- list( "HL.TestStat" = c(trt0 = hl.t0, trt1 = hl.t1), "p.value" = c(trt0 = pval.t0, trt1 = pval.t1), "Df" = c(Df), "plot" = p)#, "plot.data" = data )
class(res) = "calibrate.trtsel"
return( res )

}
