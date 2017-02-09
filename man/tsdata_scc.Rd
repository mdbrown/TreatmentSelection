\name{tsdata_scc}
\alias{tsdata_scc}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
sample stratified nested case-control data for R package TreatmentSelection
}
\description{
   Simulated stratified case-control data set consisting of two continuous markers, two discrete markers, treatment, and event status.
}
\usage{data(tsdata)}
\format{
  A data frame with 468 observations reflecting a 1:1 stratified case control subcohort sample from the \code{tsdata} dataset. Observations are stratified on treatment status. See ?tsdata for the clinical context these data were simulated under. 
  \describe{
    \item{\code{trt}}{a numeric vector distinguishing chemotherapy-treated individuals (trt = 1) vs individuals treated with tamoxifen alone (trt = 0)}
    \item{\code{event}}{a numeric vector indicating event status  (1 for recurrence or death within 5 years, 0 otherwise)}
    \item{\code{Y1}}{a numeric vector of values for marker 1. This marker has relatively weak performance.}
    \item{\code{Y2}}{a numeric vector of values for marker 2. This marker has strong performance. }
    \item{\code{Y1_disc}}{a discrete version of marker 1, coded 1 if Y1 > mean(Y1) and 0 otherwise. }
  \item{\code{Y2_disc}}{a discrete version of marker 2, coded 1 if Y2 > 0, and 0 otherwise. }
  }
}

\note{
  In order to evaluate the markers in this dataset, we need to provide the function \code{trtsel} with information from the full cohort \code{tsdata}. See the example below for help.  
}

\references{
Janes, Holly; Brown, Marshall D; Pepe, Margaret; Huang, Ying; "An Approach to Evaluating and Comparing Biomarkers for Patient Treatment Selection" The International Journal of Biostatistics. Volume 0, Issue 0, ISSN (Online) 1557-4679, ISSN (Print) 2194-573X, DOI: 10.1515/ijb-2012-0052, April 2014
}

\seealso{
\code{\link{trtsel}} for creating trtsel objects, \code{\link{plot.trtsel}} for plotting risk curves and more, \code{\link{evaluate.trtsel}} for evaluating marker performance,  \code{\link{calibrate.trtsel}} for assessing model calibration, and \code{\link{compare.trtsel}} to compare two trtsel object. 
}



\keyword{datasets}
