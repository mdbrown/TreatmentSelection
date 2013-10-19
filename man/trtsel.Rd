\name{trtsel}
\alias{trtsel}

\title{
create a trtsel object
}
\description{
Creates an object of class "trtsel" given a data.frame containing marker, treatment, and adverse event status information.   The functions "plot", "evaltrtsel", and "calibrate" can then be used to plot risk and treatment effect curves, estimate summary measures, and check model calibration. The function "compare" can also be used to compare two treatment selection markers.  

}
\usage{
trtsel(event, trt, marker, data,
                             fittedrisk.t0 = NULL, 
                             fittedrisk.t1 = NULL,
                             thresh = 0, 
                             study.design = "randomized cohort",
                             cohort.attributes = NULL, 
                             marker.bounds =NULL,
                             link = "logit" ,
                             default.trt = "trt all")
}

\arguments{
  \item{event}{
   Name of indicator vector for adverse event found in data.frame "data". Coded 1 for occurrence of event and 0 otherwise.    

  }

  \item{trt}{
   Name of treatment status in data.frame "data". Coded 1 for "treated" and 0 for "un-treated." 
  }

  \item{marker}{
  Name of the marker to be evaluated in data.frame "data". This should be a numeric vector of the same length as "event." Binary markers are allowed, and should be coded as a 0,1 numeric vector. 
  }
\item{data}{
  data.frame object holding event, trt and marker data.   
  }
  
  \item{fittedrisk.t0}{
  Instead of providing a marker, fitted risks for T=0 and T=1 may be provided. This should be the column name of the fitted risk for T=0 that can be found in 'data'. If fitted risks are provided, a logistic model will not be fit, and all bootstrap confidence intervals will be conditional on the model fit provided. 
  
  
  }
  
  \item{fittedrisk.t1}{ Name of for the fitted risks given treatment = 1. 
  }

  \item{thresh}{
  The treatment effect threshold used to define the treatment rule: Do not treat if the marker-specific treatment effect is less than "thresh". This is a numeric constant with a default value of 0.  
  }

  \item{study.design}{
  Character string indicating the study design used to collect the data. The three options are "randomized cohort" (default), "nested case-control", or "stratified nested case-control".  A "randomized cohort" design is simply a randomized trial comparing T = 0 to T = 1 with the marker measured at baseline.  A nested case-control or stratified nested case-control study samples cases and controls for marker measurement, perhaps stratified on treatment assignment, from a randomized trial comparing T = 0 to T = 1.  See Janes et al. (2013) for a full description of these designs.

  If a "nested case-control" or "stratified nested case-control" design is specified,  "cohort.attributes"" must be provided (see below).  
  }

  \item{cohort.attributes}{
  If a "nested case-control" or "stratified nested case-control" design is specified, "cohort.attributes" must be provided. Order does matter. 

    For the "nested case-control" design, specify the following attributes of the randomized trial "cohort" from which the case-control sample was selected: \cr
    \cr
        cohort.attributes = c(cohort sample size, \cr
                              proportion treated in cohort (Pr(trt==1)), \cr
                              adverse event prevalance in cohort (Pr(event==1)), \cr 
                              fraction of cases sampled from cohort) \cr \cr

    For the "stratitified nested case-control" design, specify the following attributes of the randomized trial "cohort" from which the stratified case-control sample was selected: \cr
    \cr
        cohort.attributes = c(cohort sample size, \cr
                              Pr(trt==0 & event==0) in cohort, \cr
                              Pr(trt==0 & event==1) in cohort, \cr
                              Pr(trt==1 & event==0) in cohort, \cr
                              fraction of cases with trt == 0 sampled from cohort, \cr
                              fraction of cases with trt == 1 sampled from cohort )\cr \cr
}

 \item{marker.bounds}{
  For bounded markers, a vector of lower and upper bounds.
 }
 \item{link}{
  Link function used to fit the risk model. Options are "logit"(default), "probit", "cauchit", "log" and "cloglog." Link functions other than "logit" are available only when study.design = "randomized cohort". 
  
 } 

 \item{default.trt}{
  The default treatment assignment to compare with marker-based treatment. Can either be set at "trt all" (default) or "trt none". Use "trt all" if everyone is treated and the aim is to discover those who would benefit from no treatment, but use "trt none" if the common practice is to treat no-one and the goal is to discover those who would benefit from treatment.  
  }
 
}


\value{

 An object of class "trtsel," which is a list containing: 

 \item{model.fit }{ A list containing 
  "coefficients" -- a 4 x 4 matrix with columns for coefficient estimates, standard errors, t-statistics, and two-sided p-values. 
  "cohort.attributes" -- the vector of cohort.attributes provided
  "study.design" -- character string of study.design provided   }
 \item{derived.data }{ A data.frame with "event", "trt", "marker", "fittedrisk.t0" (risk estimates given no treatment), "fittedrisk.t1" (risk estimates given treatment), "trt.effect" (treatment effect estimates), and "marker.neg" (indicator of trt.effect < thresh) columns.   }
 \item{functions}{ For internal package use only }

}
\references{

Janes, Holly; Brown, Marshall D.; Pepe, Margaret; and Huang, Ying, "Statistical Methods for Evaluating and Comparing Biomarkers for Patient Treatment Selection" (January 2013). UW Biostatistics Working Paper Series. Working Paper 389.
http://biostats.bepress.com/uwbiostat/paper389
}

\seealso{
\code{\link{plot.trtsel}} for plotting risk curves and more, \code{\link{eval.trtsel}} for evaluating marker performance,  \code{\link{calibrate.trtsel}} for assessing model calibration, and \code{\link{compare.trtsel}} to compare two trtsel object. 
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




}
