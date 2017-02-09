\name{surv_tsdata}
\alias{surv_tsdata}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
sample data for R package TreatmentSelection
}
\description{
   Simulated data set consisting of two continuous markers, two discrete markers, treatment, and time-to-event status.
}
\usage{data(surv_tsdata)}
\format{
  A data frame with 1000 simulated observations reflecting the following clinical context.  Marker Y  for identifying a subset of women with estrogen-receptor positive breast cancer who do not benefit from adjuvant chemotherapy over and above tamoxifen.  The clinical outcome of interest, termed "event", is 5-year breast cancer recurrence or death.  Data come from a hypothetical randomized trial where subjects are randomly assigned to tamoxifen alone or tamoxifen plus chemotherapy and followed to ascertain this outcome; markers Y1 and Y2 are measured at baseline.
  \describe{
  
    \item{\code{xi}}{a numeric vector specifying follow up time for right censored data.}
    \item{\code{di}}{a numeric vector indicating event status  (1 for recurrence or death within 5 years, 0 for censoring)}
    
    \item{\code{Y}}{a numeric vector of marker values. }
    \item{\code{trt}}{a numeric vector indicating treatment status. }
  
}}



\seealso{
\code{\link{trtsel}} for creating trtsel objects, \code{\link{plot.trtsel}} for plotting risk curves and more, \code{\link{evaluate.trtsel}} for evaluating marker performance,  \code{\link{calibrate.trtsel}} for assessing model calibration, and \code{\link{compare.trtsel}} to compare two trtsel object. 
}


\keyword{datasets}
