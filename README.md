TreatmentSelection
==================

This R package includes a suite of descriptive and inferential methods designed to evaluate
individual treatment selection markers and to compare candidate markers.  

functions included are:

- `trtsel` for creating trtsel objects
- `plot.trtsel` for plotting risk curves and more
- `eval.trtsel` for evaluating marker performance
- `calibrate.trtsel` for assessing model calibration
- `compare.trtsel` to compare two trtsel objects. 


To download and install the package directly from github, type:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("TreatmentSelection", "mdbrown")

```

A brief tutorial is available [here](http://rpubs.com/mdbrown/TreatmentSelectionTutorial).
