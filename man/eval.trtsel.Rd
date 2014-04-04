\name{eval.trtsel}
\alias{eval.trtsel}

\title{
evaluate the performance of a marker used for treatment selection
}
\description{

  Evaluates a treatment selection marker.  Summary measures of marker performance are estimated and confidence intervals are provided.  A test of non-null marker performance is performed.  An object of class "trtsel" must first be created using the function "trtsel" by supplying a data.frame containing marker, treatment, and event status information.  

}
\usage{
eval.trtsel(x, bootstraps = 1000, alpha = 0.05)
}

\arguments{
  \item{x}{
  An object of class "trtsel", created by using the function "trtsel."  
}
  \item{bootstraps}{
 Number of bootstrap replicates for creating confidence intervals for each performance measure. The default value is 1000. Set bootstraps = 0 if no confidence intervals are desired.   
}

  \item{alpha}{
  (1-alpha)*100\% confidence intervals are calculated. Default value is alpha = 0.05 which yields 95\% CI's.  The same alpha is used for the two-sided type-I error for the test of H0: No decrease in event rate under marker-based treatment.}
}

\value{
  A list with the following components (see Janes et al. (2013) for a description of the summary measures and estimators):

 \item{test.Null }{ List of results of a test of the null hypothesis H0: No decrease in event rate under marker-based treatment. Contains "reject" (logical; was H0 rejected), p.value, z.statistic, and alpha }
 \item{estimates }{data.frame of dimension 1x9 of summary measure estimates. Includes:
                    p.neg : proportion with negative treatment effect estimates (marker-negatives);
                    p.pos : proportion with positive treatment effect estimates (marker-negatives);
                    B.neg.emp, B.neg.mod: Average benefit of no treatment among marker-negatives, empirical and model-based estimates;
                    B.pos.emp, B.neg.mod: Average benefit of treatment among marker-positives, empirical and model-based estimates;
                    Theta.emp, Theta.mod: Decrease in event rate under marker-based treatment, empirical and model-based estimates; 
                    Var.Delta: variance in estimated treatment effect; 
                    TG: Total gain.  
                    ER.trt0.emp/mod, ER.trt1.emp/mod, ER.mkrbased.emp/mod: Event rates under trt = 0, trt = 1 or for marker-based treatment. 
                    Marker.Thresh: Marker positivity threshold-- defined as the maximum marker value such that estimated treatment effect < "thresh" (=treatment effect used to define the treatment rule, this is set when creating the trtsel object). If all observations are marker negative (or all are positive), Marker.Thresh has value NA}
 \item{conf.intervals}{ data.frame of dimension 2x9 with bootstrap-based confidence intervals for each summary measures. If bootstraps = 0 or boot = FALSE, this component is NULL. 
 
 }

}
\references{

Janes, Holly; Brown, Marshall D; Pepe, Margaret; Huang, Ying; "An Approach to Evaluating and Comparing Biomarkers for Patient Treatment Selection" The International Journal of Biostatistics. Volume 0, Issue 0, ISSN (Online) 1557-4679, ISSN (Print) 2194-573X, DOI: 10.1515/ijb-2012-0052, April 2014
}

\seealso{
\code{\link{trtsel}} for creating trtsel objects, \code{\link{plot.trtsel}} for plotting risk curves and more, \code{\link{calibrate.trtsel}} for assessing model calibration, and \code{\link{compare.trtsel}} to compare two trtsel object. 
}

\examples{

data(tsdata)

###########################
## Create trtsel objects 
###########################

trtsel.Y1 <- trtsel( event = "event",
                     trt = "trt",
                     marker = "Y1",
                     data = tsdata,
                     study.design = "randomized cohort")
trtsel.Y1

trtsel.Y2 <- trtsel( event = "event",
                     trt = "trt",
                     marker = "Y2",
                     data = tsdata,
                     study.design = "randomized cohort")
trtsel.Y2


#################################
## Evaluate marker performance
#################################

# Marker Y1
estimates.Y1 <- eval.trtsel(trtsel.Y1, bootstraps = 50)
estimates.Y1

# Without confidence intervals
eval.trtsel(trtsel.Y1, bootstraps = 0)

# Using alpha = 0.01
estimates.Y2 <- eval.trtsel(trtsel.Y2, bootstraps = 50, alpha = .01)
estimates.Y2

}
