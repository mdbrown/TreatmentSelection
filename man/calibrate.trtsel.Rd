\name{calibrate.trtsel}
\alias{calibrate.trtsel}

\title{
assess model calibration of a trtsel object
}
\description{
Assess calibration of fitted models for risk and treatment effect given marker.  Plots are used to compare observed vs. fitted risks and treatment effects and Hosmer-Lemeshow goodness-of-fit tests are reported.  An object of class "trtsel" must first be created using the function "trtsel" by supplying a data.frame containing marker, treatment, and event status information.    
 
 
}
\usage{
calibrate.trtsel(x, groups = 10, 
                    plot.type = "calibration", 
                    trt.names = c("Treatment", "No Treatment"), 
                    main = NULL, 
                    ylim = NULL, 
                    xlim = NULL, 
                    ylab = NULL, 
                    xlab = NULL) 
}

\arguments{
  \item{x}{
  An object of class "trtsel", created by using the function "trtsel."
}
  \item{groups}{
  Number of groups; observations are split into groups based on quantiles of predicted risk or treatment effect, depending on plot.type. For plot.type = "treatment effect", observations are split into groups based on quantile of predicted treatment effect; for plot.type= "calibration", "risk.t0", or "risk.t1", and for the Hosmer-Lemshow test statistic, observations on each treatment are split into groups based on quantile of predicted risk.   The default value is 10.  
}
  \item{plot.type}{
  Which type of plot to produce. Options are "calibration" (default), which plots average predicted vs. observed risks on a log scale; "risk.t1" and "risk.t0" which overlays observed risks on fitted risk curves for T = 1 and T = 0 subjects, respectively; and "treatment effect" which overlays observed treatment effects on the fitted treatment effect curve.    
}
  \item{trt.names}{
    A vector of length 2 indicating the names for the two treatment options, T= 1 and T = 0, respectively, for the plot legend. This option is only used when plot.type="calibration". The default value is c("Treatment", "No Treatment").
}
  \item{xlab}{
   A label for the x-axis. Default values depend on plot.type. Only applies if plot.type is specified.
}
  \item{ylab}{
   A label for the y-axis. Default values depend on plot.type.  Only applies if plot.type is specified.
}
  \item{xlim}{
   The limits for the x-axisof the plot, in the form c(lower,upper). Only applies if plot.type is specified.
}
  \item{ylim}{
   The limits for the y-axis of the plot, in the form c(lower,upper). Only applies if plot.type is specified.
}
  \item{main}{
   The main title for the plot.  Only applies if plot.type is specified.
}

}

\value{
    A list with the following components:
 \item{HL.TestStat}{Hosmer-Lemeshow test statistic for assessing fit of the risk models for the T = 0 and T = 1 groups.}
 \item{p.value}{P-values for the Hosmer-Lemeshow tests in each treatment group.}
 \item{Df}{Degrees of freedom for the chi-square distribution used to generate a p-value for the Hosmer-Lemeshow chi-square test. This equals "groups" - 2. }
 \item{plot}{If plot output was created, the ggplot plotting object.}
}
\references{

Janes, Holly; Brown, Marshall D; Pepe, Margaret; Huang, Ying; "An Approach to Evaluating and Comparing Biomarkers for Patient Treatment Selection" The International Journal of Biostatistics. Volume 0, Issue 0, ISSN (Online) 1557-4679, ISSN (Print) 2194-573X, DOI: 10.1515/ijb-2012-0052, April 2014
}



\seealso{
\code{\link{trtsel}} for creating trtsel objects, \code{\link{plot.trtsel}} for plotting risk curves and more, \code{\link{eval.trtsel}} for evaluating marker performance, and \code{\link{compare.trtsel}} to compare two trtsel object. 
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

##############################
## Assess model calibration
##############################

 
cali.Y1 <- calibrate.trtsel(trtsel.Y1, plot.type = "calibration")
cali.Y1

# A "treatment effect" plot 
calibrate.trtsel(trtsel.Y1, plot.type = "treatment effect")

# Increase the number of groups to 15
cali.Y2 <- calibrate.trtsel(trtsel.Y2, 
                            groups = 15, plot.type = "risk.t0")
cali.Y2


}
