\name{tsdata_cc}
\alias{tsdata_cc}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
sample nested case-control data for R package TreatmentSelection
}
\description{
   Simulated case-cohort data set consisting of two continuous markers, two discrete markers, treatment, and event status.
}
\usage{data(tsdata)}
\format{
  A data frame with 468 observations reflecting a 1:1 case control subcohort sample from the \code{tsdata} dataset. See ?tsdata for the clinical context these data were simulated under. 
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
\code{\link{trtsel}} for creating trtsel objects, \code{\link{plot.trtsel}} for plotting risk curves and more, \code{\link{eval.trtsel}} for evaluating marker performance,  \code{\link{calibrate.trtsel}} for assessing model calibration, and \code{\link{compare.trtsel}} to compare two trtsel object. 
}


\examples{
data(tsdata_cc)
data(tsdata) #need this to provide cohort.attribute information to function trtsel 

tsdata_cc[1:10, ]

###########################
## Create trtsel objects 
###########################

#create cohort.attributes using the full cohort dataset tsdata
# c(cohort sample size, Pr(T=1) in cohort, Pr(Event==1) in cohort, fraction of cases)
rho.cc <- c(nrow(tsdata), mean(tsdata$trt==1), mean(tsdata$event==1), 1.0 )

trtsel.Y1 <- trtsel( event = "event", trt = "trt", marker ="Y1", 
                    data = tsdata_cc,
                    cohort.attributes = rho.cc,
                    study.design = "nested case-control")

trtsel.Y2 <- trtsel( event = "event", trt = "trt", marker ="Y2", 
                    data = tsdata_cc,
                    cohort.attributes = rho.cc,
                    study.design = "nested case-control")

#############################################
## Plot fitted risks and treatment effects
#############################################

# Plot risk curves
tmp <- plot.trtsel(trtsel.Y1, main = "Marker Y1", 
                       plot.type = "risk", bootstraps = 50,
                       trt.names=c("chemo.","no chemo."))

plot.trtsel(trtsel.Y2, main = "Marker Y2",  
                       plot.type = "risk", bootstraps = 50,
                       trt.names=c("chemo.","no chemo."))

# Plot treatment effect curves
plot.trtsel(trtsel.Y1, main = "Marker Y1", 
                       plot.type = "treatment effect", 
                       ci = "vertical",  bootstraps = 50)

plot.trtsel(trtsel.Y2, main = "Marker Y2",
                       plot.type = "treatment effect",
                       ci = "vertical", bootstraps = 50)



##############################
## Assess model calibration
##############################

 
cali.Y1 <- calibrate.trtsel(trtsel.Y1)
cali.Y1

cali.Y2 <- calibrate.trtsel(trtsel.Y2)
cali.Y2


#########################################
## Evaluate performance of each marker 
#########################################

estimates.Y1 <- eval.trtsel(trtsel.Y1, bootstraps = 50)
estimates.Y1

estimates.Y2 <- eval.trtsel(trtsel.Y2, bootstraps = 50)
estimates.Y2


###############################
## Compare marker performance
###############################


# Compare the markers based on summary measures
mycompare <- compare.trtsel(trtsel1 = trtsel.Y1, 
                            trtsel2 = trtsel.Y2,
                                bootstraps = 50)
mycompare


}
\keyword{datasets}
