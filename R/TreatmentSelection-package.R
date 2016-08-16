#' 
#' Evaluate markers used to guide patient treatment selection decisions
#' 
#' A suite of descriptive and inferential methods designed to evaluate
#' one or more biomarkers for their ability to guide patient treatment 
#' recommendations.
#' 
#' \tabular{ll}{ Package: \tab TreatmentSelection\cr Type: \tab Package\cr
#' Version: \tab 2.0.-\cr Date: \tab 2016-07-28\cr License: \tab GPL-2\cr }
#' 
#' @name TreatmentSelection-package
#' @aliases TreatmentSelection-package TreatmentSelection
#' @docType package
#' @author Marshall Brown and Holly Janes
#' 
#' Maintainer: Marshall Brown <mdbrown@@fhcrc.org>
#' @seealso 
#' \code{\link{trtsel_measures}} for evaluating the performance of a user-specified marker-based treatment rule,
#' \code{\link{trtsel}} for creating trtsel objects,
#' \code{\link{plot.trtsel}} for plotting risk curves and more,
#' \code{\link{evaluate.trtsel}} for evaluating marker performance,
#' \code{\link{calibrate.trtsel}} for assessing model calibration, and
#' \code{\link{compare.trtsel}} to compare two trtsel object.
#' @references
#' 
#'Janes H, Brown MD, Huang Y, et al. An approach to evaluating and comparing
#'biomarkers for patient treatment selection. 
#'**Int J Biostat.** 2014;10(1):99-121.
#'
#' @keywords package
#' @examples
#' 
#' # See individual function man pages for examples. 
#' 
#' ?trtsel_measures
#' ?trtsel
#' ?plot.trtsel
#' ?evaluate.trtsel
#' ?compare.trtsel
#' ?calibrate.trtsel
#' 
NULL
