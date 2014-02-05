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

A manuscript describing the methods employed in the package can be found [here](http://biostats.bepress.com/uwbiostat/paper389/) and a brief tutorial is available [here](http://rpubs.com/mdbrown/TreatmentSelection).
