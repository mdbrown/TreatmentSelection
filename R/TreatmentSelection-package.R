

#' evaluate the performance of a marker used for treatment selection
#' 
#' 
#' Evaluates a treatment selection marker.  Summary measures of marker
#' performance are estimated and confidence intervals are provided.  A test of
#' non-null marker performance is performed.  An object of class "trtsel" must
#' first be created using the function "trtsel" by supplying a data.frame
#' containing marker, treatment, and event status information.
#' 
#' 
#' @param x An object of class "trtsel", created by using the function
#' "trtsel."
#' @param bootstraps Number of bootstrap replicates for creating confidence
#' intervals for each performance measure. The default value is 1000. Set
#' bootstraps = 0 if no confidence intervals are desired.
#' @param alpha (1-alpha)*100\% confidence intervals are calculated. Default
#' value is alpha = 0.05 which yields 95\% CI's.  The same alpha is used for
#' the two-sided type-I error for the test of H0: No decrease in event rate
#' under marker-based treatment.
#' @return A list with the following components (see Janes et al. (2013) for a
#' description of the summary measures and estimators):
#' 
#' \item{test.Null }{ List of results of a test of the null hypothesis H0: No
#' decrease in event rate under marker-based treatment. Contains "reject"
#' (logical; was H0 rejected), p.value, z.statistic, and alpha }
#' \item{estimates }{data.frame of dimension 1x9 of summary measure estimates.
#' Includes: p.neg : proportion with negative treatment effect estimates
#' (marker-negatives); p.pos : proportion with positive treatment effect
#' estimates (marker-negatives); B.neg.emp, B.neg.mod: Average benefit of no
#' treatment among marker-negatives, empirical and model-based estimates;
#' B.pos.emp, B.neg.mod: Average benefit of treatment among marker-positives,
#' empirical and model-based estimates; Theta.emp, Theta.mod: Decrease in event
#' rate under marker-based treatment, empirical and model-based estimates;
#' Var.Delta: variance in estimated treatment effect; TG: Total gain.
#' ER.trt0.emp/mod, ER.trt1.emp/mod, ER.mkrbased.emp/mod: Event rates under trt
#' = 0, trt = 1 or for marker-based treatment.  Marker.Thresh: Marker
#' positivity threshold-- defined as the maximum marker value such that
#' estimated treatment effect < "thresh" (=treatment effect used to define the
#' treatment rule, this is set when creating the trtsel object). If all
#' observations are marker negative (or all are positive), Marker.Thresh has
#' value NA} \item{conf.intervals}{ data.frame of dimension 2x9 with
#' bootstrap-based confidence intervals for each summary measures. If
#' bootstraps = 0 or boot = FALSE, this component is NULL.
#' 
#' }
#' @seealso \code{\link{trtsel}} for creating trtsel objects,
#' \code{\link{plot.trtsel}} for plotting risk curves and more,
#' \code{\link{calibrate.trtsel}} for assessing model calibration, and
#' \code{\link{compare.trtsel}} to compare two trtsel object.
#' @references
#' 
#' Janes, Holly; Brown, Marshall D; Pepe, Margaret; Huang, Ying; "An Approach
#' to Evaluating and Comparing Biomarkers for Patient Treatment Selection" The
#' International Journal of Biostatistics. Volume 0, Issue 0, ISSN (Online)
#' 1557-4679, ISSN (Print) 2194-573X, DOI: 10.1515/ijb-2012-0052, April 2014
#' @examples
#' 
#' 
#' data(tsdata)
#' 
#' ###########################
#' ## Create trtsel objects 
#' ###########################
#' 
#' trtsel.Y1 <- trtsel( event = "event",
#'                      trt = "trt",
#'                      marker = "Y1",
#'                      data = tsdata,
#'                      study.design = "randomized cohort")
#' trtsel.Y1
#' 
#' trtsel.Y2 <- trtsel( event = "event",
#'                      trt = "trt",
#'                      marker = "Y2",
#'                      data = tsdata,
#'                      study.design = "randomized cohort")
#' trtsel.Y2
#' 
#' 
#' #################################
#' ## Evaluate marker performance
#' #################################
#' 
#' # Marker Y1
#' estimates.Y1 <- eval.trtsel(trtsel.Y1, bootstraps = 50)
#' estimates.Y1
#' 
#' # Without confidence intervals
#' eval.trtsel(trtsel.Y1, bootstraps = 0)
#' 
#' # Using alpha = 0.01
#' estimates.Y2 <- eval.trtsel(trtsel.Y2, bootstraps = 50, alpha = .01)
#' estimates.Y2
#' 
#' 
NULL





#' 
#' Evaluate markers used to guide patient treatment selection decisions
#' 
#' A suite of descriptive and inferential methods designed to evaluate
#' individual treatment selection markers and to compare candidate markers.
#' 
#' \tabular{ll}{ Package: \tab TreatmentSelection\cr Type: \tab Package\cr
#' Version: \tab 1.1.3\cr Date: \tab 2015-05-20\cr License: \tab GPL-2\cr }
#' 
#' @name TreatmentSelection-package
#' @aliases TreatmentSelection-package TreatmentSelection
#' @docType package
#' @author Marshall Brown and Holly Janes
#' 
#' Maintainer: Marshall Brown <mdbrown@@fhcrc.org>
#' @seealso \code{\link{trtsel}} for creating trtsel objects,
#' \code{\link{plot.trtsel}} for plotting risk curves and more,
#' \code{\link{eval.trtsel}} for evaluating marker performance,
#' \code{\link{calibrate.trtsel}} for assessing model calibration, and
#' \code{\link{compare.trtsel}} to compare two trtsel object.
#' @references
#' 
#' Janes, Holly; Brown, Marshall D; Pepe, Margaret; Huang, Ying; "An Approach
#' to Evaluating and Comparing Biomarkers for Patient Treatment Selection" The
#' International Journal of Biostatistics. Volume 0, Issue 0, ISSN (Online)
#' 1557-4679, ISSN (Print) 2194-573X, DOI: 10.1515/ijb-2012-0052, April 2014
#' @keywords package
#' @examples
#' 
#' # See individual function man pages for examples. 
#' 
#' ?trtsel
#' ?plot.trtsel
#' ?eval.trtsel
#' ?compare.trtsel
#' ?calibrate.trtsel
#' 
NULL





#' %% ~~ data name/kind ... ~~ sample nested case-control data for R package
#' TreatmentSelection
#' 
#' Simulated case-cohort data set consisting of two continuous markers, two
#' discrete markers, treatment, and event status.
#' 
#' 
#' @name tsdata_cc
#' @docType data
#' @format A data frame with 468 observations reflecting a 1:1 case control
#' subcohort sample from the \code{tsdata} dataset. See ?tsdata for the
#' clinical context these data were simulated under.  \describe{
#' \item{list("trt")}{a numeric vector distinguishing chemotherapy-treated
#' individuals (trt = 1) vs individuals treated with tamoxifen alone (trt = 0)}
#' \item{list("event")}{a numeric vector indicating event status (1 for
#' recurrence or death within 5 years, 0 otherwise)} \item{list("Y1")}{a
#' numeric vector of values for marker 1. This marker has relatively weak
#' performance.} \item{list("Y2")}{a numeric vector of values for marker 2.
#' This marker has strong performance. } \item{list("Y1_disc")}{a discrete
#' version of marker 1, coded 1 if Y1 > mean(Y1) and 0 otherwise. }
#' \item{list("Y2_disc")}{a discrete version of marker 2, coded 1 if Y2 > 0,
#' and 0 otherwise. } }
#' @note In order to evaluate the markers in this dataset, we need to provide
#' the function \code{trtsel} with information from the full cohort
#' \code{tsdata}. See the example below for help.
#' @seealso \code{\link{trtsel}} for creating trtsel objects,
#' \code{\link{plot.trtsel}} for plotting risk curves and more,
#' \code{\link{eval.trtsel}} for evaluating marker performance,
#' \code{\link{calibrate.trtsel}} for assessing model calibration, and
#' \code{\link{compare.trtsel}} to compare two trtsel object.
#' @references Janes, Holly; Brown, Marshall D; Pepe, Margaret; Huang, Ying;
#' "An Approach to Evaluating and Comparing Biomarkers for Patient Treatment
#' Selection" The International Journal of Biostatistics. Volume 0, Issue 0,
#' ISSN (Online) 1557-4679, ISSN (Print) 2194-573X, DOI: 10.1515/ijb-2012-0052,
#' April 2014
#' @keywords datasets
#' @examples
#' 
#' data(tsdata_cc)
#' data(tsdata) #need this to provide cohort.attribute information to function trtsel 
#' 
#' tsdata_cc[1:10, ]
#' 
#' ###########################
#' ## Create trtsel objects 
#' ###########################
#' 
#' #create cohort.attributes using the full cohort dataset tsdata
#' # c(cohort sample size, Pr(T=1) in cohort, Pr(Event==1) in cohort, fraction of cases)
#' rho.cc <- c(nrow(tsdata), mean(tsdata$trt==1), mean(tsdata$event==1), 1.0 )
#' 
#' trtsel.Y1 <- trtsel( event = "event", trt = "trt", marker ="Y1", 
#'                     data = tsdata_cc,
#'                     cohort.attributes = rho.cc,
#'                     study.design = "nested case-control")
#' 
#' #############################################
#' ## Plot fitted risks
#' #############################################
#' 
#' # Plot risk curves
#' tmp <- plot.trtsel(trtsel.Y1, main = "Marker Y1", 
#'                        plot.type = "risk", bootstraps = 10, # set this higher in practice
#'                        trt.names=c("chemo.","no chemo."))
#' 
#' 
#' 
#' 
#' ##############################
#' ## Assess model calibration
#' ##############################
#' 
#'  
#' cali.Y1 <- calibrate.trtsel(trtsel.Y1)
#' cali.Y1
#' 
#' #########################################
#' ## Evaluate performance of each marker 
#' #########################################
#' 
#' estimates.Y1 <- eval.trtsel(trtsel.Y1, bootstraps = 10)
#' estimates.Y1
#' 
#' 
#' 
#' 
NULL





#' %% ~~ data name/kind ... ~~ sample stratified nested case-control data for R
#' package TreatmentSelection
#' 
#' Simulated stratified case-control data set consisting of two continuous
#' markers, two discrete markers, treatment, and event status.
#' 
#' 
#' @name tsdata_scc
#' @docType data
#' @format A data frame with 468 observations reflecting a 1:1 stratified case
#' control subcohort sample from the \code{tsdata} dataset. Observations are
#' stratified on treatment status. See ?tsdata for the clinical context these
#' data were simulated under.  \describe{ \item{list("trt")}{a numeric vector
#' distinguishing chemotherapy-treated individuals (trt = 1) vs individuals
#' treated with tamoxifen alone (trt = 0)} \item{list("event")}{a numeric
#' vector indicating event status (1 for recurrence or death within 5 years, 0
#' otherwise)} \item{list("Y1")}{a numeric vector of values for marker 1. This
#' marker has relatively weak performance.} \item{list("Y2")}{a numeric vector
#' of values for marker 2. This marker has strong performance. }
#' \item{list("Y1_disc")}{a discrete version of marker 1, coded 1 if Y1 >
#' mean(Y1) and 0 otherwise. } \item{list("Y2_disc")}{a discrete version of
#' marker 2, coded 1 if Y2 > 0, and 0 otherwise. } }
#' @note In order to evaluate the markers in this dataset, we need to provide
#' the function \code{trtsel} with information from the full cohort
#' \code{tsdata}. See the example below for help.
#' @seealso \code{\link{trtsel}} for creating trtsel objects,
#' \code{\link{plot.trtsel}} for plotting risk curves and more,
#' \code{\link{eval.trtsel}} for evaluating marker performance,
#' \code{\link{calibrate.trtsel}} for assessing model calibration, and
#' \code{\link{compare.trtsel}} to compare two trtsel object.
#' @references Janes, Holly; Brown, Marshall D; Pepe, Margaret; Huang, Ying;
#' "An Approach to Evaluating and Comparing Biomarkers for Patient Treatment
#' Selection" The International Journal of Biostatistics. Volume 0, Issue 0,
#' ISSN (Online) 1557-4679, ISSN (Print) 2194-573X, DOI: 10.1515/ijb-2012-0052,
#' April 2014
#' @keywords datasets
#' @examples
#' 
#' data(tsdata_scc)
#' data(tsdata) #need this to provide cohort.attribute information to function trtsel 
#' 
#' tsdata_scc[1:10, ]
#' 
#' ###########################
#' ## Create trtsel objects 
#' ###########################
#' 
#' #create cohort.attributes using the full cohort dataset tsdata
#' # c(cohort sample size, 
#' #Pr(trt==0 & event==0) in cohort, 
#' #Pr(trt==0 & event==1) in cohort, 
#' #Pr(trt==1 & event==0) in cohort, 
#' #fraction of cases with trt == 0 sampled from cohort, 
#' #fraction of cases with trt == 1 sampled from cohort )
#' 
#' rho.scc <- c( nrow(tsdata), with(tsdata, mean(trt==0 & event==0)), 
#'                             with(tsdata, mean(trt==0 & event==1)), 
#'                             with(tsdata, mean(trt==1 & event==0)),
#'                             1.0, 
#'                             1.0)
#' 
#' trtsel.Y1 <- trtsel( event = "event", trt = "trt", marker ="Y1", 
#'                     data = tsdata_scc,
#'                     cohort.attributes = rho.scc,
#'                     study.design = "stratified nested case-control")
#' 
#' ############################################
#' ## Plot risk curves
#' #############################################
#' 
#' # Plot risk curves
#' tmp <- plot.trtsel(trtsel.Y1, main = "Marker Y1", 
#'                        plot.type = "risk", bootstraps = 10, # set higher in practice
#'                        trt.names=c("chemo.","no chemo."))
#' 
#' 
#' 
#' 
#' ##############################
#' ## Assess model calibration
#' ##############################
#' 
#'  
#' cali.Y1 <- calibrate.trtsel(trtsel.Y1)
#' cali.Y1
#' 
#' 
#' #########################################
#' ## Evaluate performance of each marker 
#' #########################################
#' 
#' estimates.Y1 <- eval.trtsel(trtsel.Y1, bootstraps = 10)
#' estimates.Y1
#' 
#' 
#' 
NULL





#' %% ~~ data name/kind ... ~~ sample data for R package TreatmentSelection
#' 
#' Simulated data set consisting of two continuous markers, two discrete
#' markers, treatment, and event status.
#' 
#' 
#' @name tsdata
#' @docType data
#' @format A data frame with 1000 simulated observations reflecting the
#' following clinical context.  Markers Y1 and Y2 are candidates for
#' identifying a subset of women with estrogen-receptor positive breast cancer
#' who do not benefit from adjuvant chemotherapy over and above tamoxifen.  The
#' clinical outcome of interest, termed "event", is 5-year breast cancer
#' recurrence or death.  Data come from a hypothetical randomized trial where
#' subjects are randomly assigned to tamoxifen alone or tamoxifen plus
#' chemotherapy and followed to ascertain this outcome; markers Y1 and Y2 are
#' measured at baseline.  \describe{ \item{list("trt")}{a numeric vector
#' distinguishing chemotherapy-treated individuals (trt = 1) vs individuals
#' treated with tamoxifen alone (trt = 0)} \item{list("event")}{a numeric
#' vector indicating event status (1 for recurrence or death within 5 years, 0
#' otherwise)} \item{list("Y1")}{a numeric vector of values for marker 1. This
#' marker has relatively weak performance.} \item{list("Y2")}{a numeric vector
#' of values for marker 2. This marker has strong performance. }
#' \item{list("Y1_disc")}{a discrete version of marker 1, coded 1 if Y1 >
#' mean(Y1) and 0 otherwise. } \item{list("Y2_disc")}{a discrete version of
#' marker 2, coded 1 if Y2 > 0, and 0 otherwise. } }
#' @seealso \code{\link{trtsel}} for creating trtsel objects,
#' \code{\link{plot.trtsel}} for plotting risk curves and more,
#' \code{\link{eval.trtsel}} for evaluating marker performance,
#' \code{\link{calibrate.trtsel}} for assessing model calibration, and
#' \code{\link{compare.trtsel}} to compare two trtsel object.
#' @references Janes, Holly; Brown, Marshall D; Pepe, Margaret; Huang, Ying;
#' "An Approach to Evaluating and Comparing Biomarkers for Patient Treatment
#' Selection" The International Journal of Biostatistics. Volume 0, Issue 0,
#' ISSN (Online) 1557-4679, ISSN (Print) 2194-573X, DOI: 10.1515/ijb-2012-0052,
#' April 2014
#' @keywords datasets
#' @examples
#' 
#' 
#' 
#' data(tsdata)
#' 
#' tsdata[1:10, ]
#' 
#' ###########################
#' ## Create trtsel objects 
#' ###########################
#' 
#' trtsel.Y1 <- trtsel( event = "event",
#'                      trt = "trt",
#'                      marker = "Y1",
#'                      data = tsdata,
#'                      study.design = "randomized cohort")
#' trtsel.Y1
#' 
#' 
#' #############################################
#' ## Plot fitted risks 
#' #############################################
#' 
#' # Plot risk curves
#' tmp <- plot.trtsel(trtsel.Y1, main = "Marker Y1", 
#'                        plot.type = "risk", bootstraps = 10, # set this higher in practice
#'                        trt.names=c("chemo.","no chemo."))
#' 
#' 
#' 
#' ##############################
#' ## Assess model calibration
#' ##############################
#' 
#'  
#' cali.Y1 <- calibrate.trtsel(trtsel.Y1)
#' cali.Y1
#' 
#' 
#' #########################################
#' ## Evaluate performance of each marker 
#' #########################################
#' 
#' estimates.Y1 <- eval.trtsel(trtsel.Y1, bootstraps = 10)
#' estimates.Y1
#' 
#' 
#' 
NULL



