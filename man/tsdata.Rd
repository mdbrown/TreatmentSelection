\name{tsdata}
\alias{tsdata}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
sample data for R package TreatmentSelection
}
\description{
   Simulated data set consisting of two continuous markers, two discrete markers, treatment, and event status.
}
\usage{data(tsdata)}
\format{
  A data frame with 1000 simulated observations reflecting the following clinical context.  Markers Y1 and Y2 are candidates for identifying a subset of women with estrogen-receptor positive breast cancer who do not benefit from adjuvant chemotherapy over and above tamoxifen.  The clinical outcome of interest, termed "event", is 5-year breast cancer recurrence or death.  Data come from a hypothetical randomized trial where subjects are randomly assigned to tamoxifen alone or tamoxifen plus chemotherapy and followed to ascertain this outcome; markers Y1 and Y2 are measured at baseline.
  \describe{
    \item{\code{trt}}{a numeric vector distinguishing chemotherapy-treated individuals (trt = 1) vs individuals treated with tamoxifen alone (trt = 0)}
    \item{\code{event}}{a numeric vector indicating event status  (1 for recurrence or death within 5 years, 0 otherwise)}
    \item{\code{Y1}}{a numeric vector of values for marker 1. This marker has relatively weak performance.}
    \item{\code{Y2}}{a numeric vector of values for marker 2. This marker has strong performance. }
    \item{\code{Y1_disc}}{a discrete version of marker 1, coded 1 if Y1 > mean(Y1) and 0 otherwise. }
  \item{\code{Y2_disc}}{a discrete version of marker 2, coded 1 if Y2 > 0, and 0 otherwise. }
  }
}

\references{
Janes, Holly; Brown, Marshall D; Pepe, Margaret; Huang, Ying; "An Approach to Evaluating and Comparing Biomarkers for Patient Treatment Selection" The International Journal of Biostatistics. Volume 0, Issue 0, ISSN (Online) 1557-4679, ISSN (Print) 2194-573X, DOI: 10.1515/ijb-2012-0052, April 2014
}

\seealso{
\code{\link{trtsel}} for creating trtsel objects, \code{\link{plot.trtsel}} for plotting risk curves and more, \code{\link{eval.trtsel}} for evaluating marker performance,  \code{\link{calibrate.trtsel}} for assessing model calibration, and \code{\link{compare.trtsel}} to compare two trtsel object. 
}


\examples{

}
\keyword{datasets}
