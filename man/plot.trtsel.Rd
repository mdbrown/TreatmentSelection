\name{plot.trtsel}
\alias{plot.trtsel}
\alias{plot}
\title{

plot risk curves, treatment effect curves or cdf of risk for a trtsel object. 
}
\description{
          Plots a treatment selection marker.  Estimated "risk curves" and "treatment effect curves" are plotted.  An object of class "trtsel" must first be created using the function "trtsel" by supplying a data.frame containing marker, treatment, and event status information.  
}
\usage{
#S3 method for class ''trtsel''
\method{plot}{trtsel}(x, bootstraps = 500,  
                         plot.type = "risk",
                         ci = "default",
                         alpha = .05,
                         fixed.values = NULL,
                         offset = 0.01,  
                         conf.bands = TRUE,
                         conf.bandsN = 100, 
                         trt.names = c("Treatment", "    No \nTreatment"),
                         xlab = NULL, 
                         ylab = NULL, 
                         xlim = NULL, 
                         ylim = NULL,
                         main = NULL, ...)
}
%- maybe also "usage" for other objects documented here.
\arguments{
  \item{x}{
   An object of class "trtsel", created by using the function "trtsel."
}
  \item{bootstraps}{
   Number of bootstrap replicates for creating confidence intervals and bands. Default value is 500. Set bootstraps=0 if no confidence intervals or bands are desired. 
}

  \item{plot.type}{
   Type of plot to produce. Options are "risk" (default) for risk curves which show risk of adverse event as a function of marker and treatment, "treatment effect" for the treatment effect curve which shows the difference in marker-specific risk of adverse event with vs. without treatment, or "cdf" for the cumulative distribution function of the treatment effect. 
}
  \item{ci}{
   Indication of whether horizontal or vertical confidence intervals are desired. Character string of either "horizontal" (default) or "vertical." 
}
\item{alpha}{
  (1-alpha)*100\% confidence intervals are calculated. Default value is alpha = 0.05 which yields 95\% CI's. 
 }
  \item{fixed.values}{
   A numeric vector indicating fixed values on the x- or y-axes at which bootstrap confidence intervals are provided. If "fixed.values" are provided, point-wise confidence intervals will be printed (i.e. conf.bands will be taken as FALSE).   
}
   \item{offset}{
   If confidence intervals are to be plotted for specified fixed.values, offset is the amount of distance to offset confidence intervals so that they do not overlap on the plot. The default value is 0.01.  
   }
   \item{conf.bands}{
   Indication of whether pointwise confidence intervals are shown for the curve(s).  TRUE (default) or FALSE. If "fixed.values" are input, this option is ignored and no confidence bands are produced. 
}
\item{conf.bandsN}{
   If conf.bands = TRUE, the number of points along the x- or y-axis at which to calculate the pointwise confidence intervals. The default is 100. 
}
  \item{trt.names}{
   A vector of length 2 indicating the names for the two treatment options, T= 1 and T = 0, respectively, for the plot legend. This option is only used when plot.type="risk". The default value is c("Treatment", "No Treatment"). 
}
  \item{xlab}{
   A label for the x-axis. Default values depend on plot.type. 
}
  \item{ylab}{
   A label for the y-axis. Default values depend on plot.type. 
}
  \item{xlim}{
   The limits for the x-axisof the plot, in the form c(x1,x2)
}
  \item{ylim}{
   The limits for the y-axis of the plot, in the form c(y1,y2) 
}
  \item{main}{
   The main title for the plot. 
}
\item{\dots}{ N/A}
}


\value{

  
  Generates a plot. In addition, if the function call is assigned to a variable (as in "myplot <- plot(mytrtsel)"), plot.trtsel returns a list composed of:
\item{plot}{ ggplot object containing the printed plot. }
\item{ci.bounds}{ A data.frame containing the bounds of the bootstrap-based confidence intervals, along with the fixed.values they are centered around, if applicable. }

}
\references{

Janes, Holly; Brown, Marshall D.; Pepe, Margaret; and Huang, Ying, "Statistical Methods for Evaluating and Comparing Biomarkers for Patient Treatment Selection" (January 2013). UW Biostatistics Working Paper Series. Working Paper 389.
http://biostats.bepress.com/uwbiostat/paper389
}


\seealso{
\code{\link{trtsel}} for creating trtsel objects,  \code{\link{eval.trtsel}} for evaluating marker performance,  \code{\link{calibrate.trtsel}} for assessing model calibration, and \code{\link{compare.trtsel}} to compare two trtsel object. 
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

#discrete marker
trtsel.Y1_disc <- trtsel( event ="event", trt = "trt", marker = "Y1_disc", 
                          data =  tsdata,
                          study.design = "randomized cohort")

##########################
## Use the plot function 
##########################

# Plot risk curves
plot.trtsel(trtsel.Y1, main = "Marker Y1", 
                       plot.type = "risk", bootstraps = 50,
                       trt.names=c("chemo.","no chemo."))
     
# Now with confidence intervals around fixed.values 
 plot.trtsel(trtsel.Y1, main = "Marker Y1",
                        plot.type = "risk", ci = "horizontal", 
                        fixed.values = c(.2, .5), 
                        offset = .01, bootstraps = 50,
                        trt.names=c("chemo.","no chemo."))
                        
#discrete marker
plot.trtsel(trtsel.Y1_disc, main = "Discrete Marker Y1", 
            plot.type = "risk" , bootstraps = 50)

   

# Plot treatment effect curves
plot.trtsel(trtsel.Y1, main = "Marker Y1",
                       plot.type = "treatment effect",
                       ci = "horizontal",  bootstraps = 50 )

# Now with confidence intervals around fixed.values
plot.trtsel(trtsel.Y2, main = "Marker Y2", 
                       plot.type = "treatment effect",
                       ci = "vertical", conf.bands = FALSE,
                       fixed.values = c(.1, .2), bootstraps = 500)



}
