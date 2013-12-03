September 2 2013
Tutorial for R package TreatmentSelection 
========================================================

This tutorial uses the Treatment Selection package to analyze the example data provided in the package.


First, you need to download and install the package from github using:



```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("TreatmentSelection", "mdbrown")

```


Alternatively, you could download the package from [here](http://mdbrown.github.io/TreatmentSelection/) and install the package locally. 


Next load the package and look at the example data called `tsdata`. Four markers are included in the data example, a ''weak'' and a ''strong'' marker ($Y1$ and $Y2$ respectively), along with a weak/strong discrete markers. 



```r
library(TreatmentSelection)
```

```
## Loading required package: ggplot2
```

```
## Loading required package: grid
```

```r

data(tsdata)

tsdata[1:5, ]
```

```
##     trt event    Y1      Y2 Y1_disc Y2_disc
## 510   0     1 19.74 -0.2010       1       1
## 945   0     0 24.40  0.1095       1       0
## 476   1     0 20.30  1.2715       1       0
## 600   1     0 16.41 -0.7615       1       1
## 697   1     1 54.03 -1.8690       0       1
```


Create TrtSel objects
-------------------------

Once we have the package and our data loaded into R, we need to create a treatment selection R object using the function `trtsel`. This function takes as inputs a data.frame of treatment indicators, adverse event status, marker values, and other optional information. Once we have created this object, we can then use it to plot risk/treatment effect curves, estimate summary measures, and check model calibration. 

First let's create a `trtsel` object using the weak marker data `Y1`, and take a look at it's contents:



```r
trtsel.Y1 <- trtsel(event = "event", trt = "trt", marker = "Y1", data = tsdata, 
    study.design = "randomized cohort", link = "logit", default.trt = "trt all")

trtsel.Y1
```

```
## Study design: randomized cohort 
## 
## Model Fit:
## 
##  Link function: logit 
## 
##  Coefficients: 
##              Estimate Std. Error z value  Pr(>|z|)
## (Intercept) -2.417536    0.45705 -5.2894 1.227e-07
## trt          0.143513    0.63295  0.2267 8.206e-01
## marker       0.041652    0.01194  3.4872 4.881e-04
## trt:marker  -0.002608    0.01645 -0.1586 8.740e-01
## 
## 
## Derived Data: (first ten rows)
## 
##    trt event marker fittedrisk.t0 fittedrisk.t1 trt.effect marker.neg
## 1    0     1 19.742        0.1686        0.1819 -0.0132972          1
## 2    0     0 24.402        0.1976        0.2106 -0.0129702          1
## 3    1     0 20.302        0.1719        0.1852 -0.0132786          1
## 4    1     0 16.412        0.1501        0.1634 -0.0133030          1
## 5    1     1 54.032        0.4583        0.4590 -0.0006385          1
## 6    1     0  6.582        0.1050        0.1174 -0.0124743          1
## 7    0     0 28.472        0.2259        0.2382 -0.0123378          1
## 8    1     1 60.072        0.5211        0.5178  0.0032908          0
## 9    1     0 23.322        0.1906        0.2037 -0.0130819          1
## 10   1     0 44.362        0.3613        0.3677 -0.0064385          1
```



As we see above, the object contains information about the study design, model fit, fitted risks given treatment, and estimated treatment effect for each individual. To access information regarding the model fit (i.e. model coefficients), type:



```r
trtsel.Y1$model.fit
```

```
## $coefficients
##              Estimate Std. Error z value  Pr(>|z|)
## (Intercept) -2.417536    0.45705 -5.2894 1.227e-07
## trt          0.143513    0.63295  0.2267 8.206e-01
## marker       0.041652    0.01194  3.4872 4.881e-04
## trt:marker  -0.002608    0.01645 -0.1586 8.740e-01
## 
## $cohort.attributes
## NULL
## 
## $study.design
## [1] "randomized cohort"
## 
## $marker.bounds
## NULL
## 
## $link
## [1] "logit"
## 
## $thresh
## [1] 0
```



and to access the data derived from modelling (`derived.data`):



```r
Y1derived.data <- trtsel.Y1$derived.data

head(Y1derived.data)
```

```
##   trt event marker fittedrisk.t0 fittedrisk.t1 trt.effect marker.neg
## 1   0     1 19.742        0.1686        0.1819 -0.0132972          1
## 2   0     0 24.402        0.1976        0.2106 -0.0129702          1
## 3   1     0 20.302        0.1719        0.1852 -0.0132786          1
## 4   1     0 16.412        0.1501        0.1634 -0.0133030          1
## 5   1     1 54.032        0.4583        0.4590 -0.0006385          1
## 6   1     0  6.582        0.1050        0.1174 -0.0124743          1
```


Now create a `trtsel` object using a discrete marker. 



```r
# Y2_disc = as.numeric(Y2>0)
trtsel.Y2_disc <- trtsel(event = "event", trt = "trt", marker = "Y2_disc", data = tsdata, 
    study.design = "randomized cohort", link = "logit")
```



See `?trtsel` for more information. Now that we have created trtsel objects, we can plot, evaluate, calibrate and compare them. 

Use the plot function
--------------------------

Plot risk curves:



```r
plot.trtsel(trtsel.Y1, main = "Y1: Oncotype-DX-like marker", plot.type = "risk", 
    ci = "horizontal", conf.bands = TRUE, bootstraps = 100, trt.names = c("chemo.", 
        "no chemo."), show.marker.axis = FALSE)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 



For a binary marker, we calculate vertical confidence bands:



```r
tmp <- plot.trtsel(trtsel.Y2_disc, main = "Discrete version of Y2", plot.type = "risk", 
    ci = "vertical", conf.bands = TRUE, offset = 0.01, bootstraps = 100, trt.names = c("chemo.", 
        "no chemo."))
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 



`tmp` is now a list with elements `plot` that holds the ggplot output, and `ci.bounds` which holds the information regarding the confidence bounds. 



```r
tmp$ci.bounds
```

```
##      risk trt marker   lower  upper
## 1 0.23636   0      1 0.14154 0.3855
## 2 0.25000   0      0 0.15093 0.3590
## 3 0.09091   1      0 0.02996 0.1533
## 4 0.44615   1      1 0.32836 0.5672
```



We can also plot the distribution of treatment effects. 



```r
plot.trtsel(trtsel.Y1, plot.type = "treatment effect", ci = "horizontal", conf.bands = TRUE, 
    bootstraps = 100)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 



```r
plot.trtsel(trtsel.Y2_disc, plot.type = "treatment effect", conf.bands = TRUE, 
    bootstraps = 100)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 


Evaluate marker performance
----------------------------------



```r
tmp <- eval.trtsel(trtsel.Y1, bootstraps = 50)
tmp
```

```
## 
##   ##############################################
##   ###                WARNING!                ###
##   ##############################################
## 
##   ### Not enough evidence to reject the      ###
##   ### hypothesis test of:                    ###
##   ###                                        ###
##   ### H_0 : No marker-by-treatment           ###
##   ###              interaction               ###
##   ###                                        ###
##   ### Inference for Theta may be unreliable! ###
## 
##   ##############################################
## 
## 
## 
##   Hypothesis test:
##  ------------------
##   H0: No marker-by-treatment interaction
##                                        P value = 0.874
##                                        Z statistic = -0.159
## 
##   Summary Measure Estimates (with 95% confidence intervals) 
##  -----------------------------------------------------------
##   Decrease in event rate under marker-based treatment (Theta)
##     Empirical:    0.033 (-0.048,0.148) 
##     Model Based:  0.01 (0,0.109) 
## 
##   Proportion marker negative:
##    0.9 (0,0.998) 
##   Proportion marker positive:
##    0.1 (0.002,1) 
## 
##   Average benefit of no treatment among marker-negatives (B.neg)
##     Empirical:    0.036 (-0.499,0.189) 
##     Model Based:  0.011 (0,0.153) 
## 
##   Average benefit of treatment among marker-positives (B.pos)
##     Empirical:    0.156 (-0.047,0.354) 
##     Model Based:  0.006 (0.001,0.146) 
## 
## 
##   Variance in estimated treatment effect: 
##     0 (0,0.018) 
##   Total Gain: 
##     0.014 (0.013,0.106) 
## 
##   Marker positivity threshold:  54.74
## 
##   Event Rates:
##  --------------------
##              Treat all       Treat None    Marker-based Treatment
##  Empirical:     0.244           0.267          0.234    
##             (0.166,0.311)   (0.184,0.359)   (0.139,0.305) 
##  Model Based:   0.251           0.261          0.251    
##             (0.178,0.311)   (0.183,0.344)   (0.160,0.275)
```



```r
# access the estimates
tmp$estimates
```

```
##   p.neg p.pos B.neg.emp B.neg.mod B.pos.emp B.pos.mod Theta.emp Theta.mod
## 1   0.9   0.1   0.03632   0.01113    0.1558  0.006444   0.03269   0.01001
##   Var.Delta      TG ER.trt0.emp ER.trt0.mod ER.trt1.emp ER.trt1.mod
## 1  0.000239 0.01411      0.2437      0.2512      0.2672      0.2605
##   ER.mkrbased.emp ER.mkrbased.mod Marker.Thresh
## 1          0.2345          0.2505         54.74
```



```r
# discrete marker
eval.trtsel(trtsel.Y2_disc, bootstraps = 50)
```

```
## 
## 
##   Hypothesis test:
##  ------------------
##   H0: No marker-by-treatment interaction
##                                        P value = 0.00098
##                                        Z statistic = 3.296
## 
##   Summary Measure Estimates (with 95% confidence intervals) 
##  -----------------------------------------------------------
##   Decrease in event rate under marker-based treatment (Theta)
##     Empirical:    0.101 (0.03,0.179) 
##     Model Based:  0.101 (0.03,0.179) 
## 
##   Proportion marker negative:
##    0.48 (0.413,0.521) 
##   Proportion marker positive:
##    0.52 (0.479,0.587) 
## 
##   Average benefit of no treatment among marker-negatives (B.neg)
##     Empirical:    0.21 (0.067,0.391) 
##     Model Based:  0.21 (0.067,0.391) 
## 
##   Average benefit of treatment among marker-positives (B.pos)
##     Empirical:    0.159 (0.049,0.241) 
##     Model Based:  0.159 (0.049,0.241) 
## 
## 
##   Event Rates:
##  --------------------
##              Treat all       Treat None    Marker-based Treatment
##  Empirical:     0.244           0.267          0.166    
##             (0.166,0.315)   (0.186,0.352)   (0.101,0.235) 
##  Model Based:   0.243           0.261          0.161    
##             (0.167,0.314)   (0.191,0.349)   (0.100,0.232)
```


Assess model calibration
--------------------------------------------

Currently, model calibration is only available for continuous markers. 



```r
calibrate.trtsel(trtsel.Y1, groups = 10, plot = "calibration", trt.names = c("chemo.", 
    "no chemo."))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

```
## 
##   Hosmer - Lemeshow test for model calibration
##  ----------------------------------------------
## 
##    Number of Groups: 10 
## 
##    No Treatment (trt = 0):
##     Test Statistic = 12.79,   DF = 8,   p value = 0.1192
## 
##    Treated (trt = 1):
##     Test Statistic = 9.63,   DF = 8,   p value = 0.2919
```


See `?calibrate.trtsel` for more plot options. 


Compare markers
---------------------------------------

To compare markers, the trt and event labels must be identical for the two markers. Plots can not be generated if comparing a discrete marker with a continuous marker. 



```r
# trtsel object for the stronger marker 2
trtsel.Y2 <- trtsel(event = "event", trt = "trt", marker = "Y2", data = tsdata, 
    default.trt = "trt all")

# Compare the markers based on summary measures
mycompare <- compare.trtsel(trtsel1 = trtsel.Y1, trtsel2 = trtsel.Y2, marker.names = c("Weak", 
    "Strong"), bootstraps = 50, plot = TRUE, ci = "vertical", offset = 0.01, 
    conf.bands = TRUE)
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 

```r

mycompare
```

```
##                       Summary Measure Estimates 
##                     (with  95 % confidence intervals) 
## 
##                marker 1    |    marker 2    |   difference    (p-value)
##  ------------------------------------------------------------------------
## 
## Decrease in event rate under marker-based treatment (Theta)
##  Empirical:     0.033      |     0.089     |     -0.057         (0.02)
##             (-0.032,0.097) | (0.044,0.256) | (-0.115,-0.075) 
##  Model Based:   0.010      |     0.127      |     -0.117         (< 0.02)
##             (0.000,0.127)  | (0.068,0.168)  | (-0.140,-0.004) 
## 
## Proportion marker negative:
##                 0.900      |     0.436      |     0.464         (0.6)
##             (0.000,1.000)  | (0.321,0.583)  | (-0.366,0.540) 
## Proportion marker positive:
##                 0.100      |     0.564      |     -0.464         (0.58)
##             (0.000,1.000)  | (0.417,0.679)  | (-0.540,0.366) 
## 
## Average benefit of no treatment among marker-negatives (B.neg)
##  Empirical:     0.036      |     0.205     |     -0.169         (0.02)
##             (-0.244,0.166) | (0.124,0.353) | (-0.400,-0.061) 
##  Model Based:   0.011      |     0.291      |     -0.280         (< 0.02)
##             (0.000,0.136)  | (0.173,0.393)  | (-0.356,-0.139) 
## 
## Average benefit of treatment among marker-positives (B.pos)
##  Empirical:     0.156      |     0.130     |     0.026         (0.34)
##             (-0.062,0.265) | (0.042,0.226) | (-0.220,0.124) 
##  Model Based:   0.006      |     0.190      |     -0.184         (< 0.02)
##             (0.000,0.097)  | (0.116,0.256)  | (-0.236,-0.075) 
## 
## 
## Variance in estimated treatment effect : 
##                 0.000      |     0.084      |     -0.084         (< 0.02)
##             (0.000,0.013)  | (0.040,0.124)  | (-0.123,-0.036) 
## 
## Total Gain: 
##                 0.014      |     0.238      |     -0.224         (< 0.02)
##             (0.006,0.094)  | (0.162,0.302)  | (-0.279,-0.123)
```





```r
## access the estimates and ci bounds.
mycompare$estimates.marker1  #estimates from trtsel1
```

```
##   p.neg p.pos B.neg.emp B.neg.mod B.pos.emp B.pos.mod Theta.emp Theta.mod
## 1   0.9   0.1   0.03632   0.01113    0.1558  0.006444   0.03269   0.01001
##   Var.Delta      TG ER.trt0.emp ER.trt0.mod ER.trt1.emp ER.trt1.mod
## 1  0.000239 0.01411      0.2437      0.2512      0.2672      0.2605
##   ER.mkrbased.emp ER.mkrbased.mod
## 1          0.2345          0.2505
```

```r
mycompare$estimates.marker2
```

```
##   p.neg p.pos B.neg.emp B.neg.mod B.pos.emp B.pos.mod Theta.emp Theta.mod
## 1 0.436 0.564    0.2051    0.2914    0.1302    0.1903   0.08942    0.1271
##   Var.Delta     TG ER.trt0.emp ER.trt0.mod ER.trt1.emp ER.trt1.mod
## 1   0.08377 0.2378      0.2437      0.2433      0.2672       0.263
##   ER.mkrbased.emp ER.mkrbased.mod
## 1          0.1778          0.1359
```

```r
mycompare$ci.marker1
```

```
##       p.neg p.pos B.neg.emp B.neg.mod B.pos.emp B.pos.mod Theta.emp
## lower     0     0   -0.2437     0.000  -0.06211   0.00000  -0.03214
## upper     1     1    0.1664     0.136   0.26548   0.09661   0.12719
##       Theta.mod Var.Delta       TG
## lower    0.0000 7.025e-05 0.005696
## upper    0.0995 1.300e-02 0.094389
```







```r
## Compare two discrete markers Y1_disc = as.numeric(Y1>mean(Y1))
trtsel.Y1_disc <- trtsel(event = "event", trt = "trt", marker = "Y1_disc", data = tsdata, 
    study.design = "randomized cohort", link = "logit")


compare.trtsel(trtsel1 = trtsel.Y1_disc, trtsel2 = trtsel.Y2_disc, ci = "vertical", 
    offset = 0.2, bootstraps = 50, plot = TRUE, conf.bands = TRUE, annotate.plot = FALSE)
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18.png) 

```
##                       Summary Measure Estimates 
##                     (with  95 % confidence intervals) 
## 
##                marker 1    |    marker 2    |   difference    (p-value)
##  ------------------------------------------------------------------------
## 
## Decrease in event rate under marker-based treatment (Theta)
##  Empirical:     0.025      |     0.101     |     -0.075         (0.1)
##             (-0.023,0.182) | (0.036,0.280) | (-0.159,0.003) 
##  Model Based:   0.025      |     0.101      |     -0.075         (0.1)
##             (-0.023,0.087)  | (0.036,0.164)  | (-0.159,0.015) 
## 
## Proportion marker negative:
##                 0.564      |     0.480      |     0.084         (0.12)
##             (0.497,0.615)  | (0.413,0.548)  | (-0.013,0.156) 
## Proportion marker positive:
##                 0.436      |     0.520      |     -0.084         (0.1)
##             (0.385,0.503)  | (0.452,0.587)  | (-0.156,0.013) 
## 
## Average benefit of no treatment among marker-negatives (B.neg)
##  Empirical:     0.045      |     0.210     |     -0.165         (0.1)
##             (-0.041,0.156) | (0.075,0.378) | (-0.312,0.014) 
##  Model Based:   0.045      |     0.210      |     -0.165         (0.1)
##             (-0.041,0.156)  | (0.075,0.378)  | (-0.312,0.014) 
## 
## Average benefit of treatment among marker-positives (B.pos)
##  Empirical:     0.010      |     0.159     |     -0.149         (0.06)
##             (-0.191,0.182) | (0.001,0.280) | (-0.320,0.003) 
##  Model Based:   0.010      |     0.159      |     -0.149         (0.06)
##             (-0.191,0.182)  | (0.001,0.280)  | (-0.320,0.003) 
## 
## 
## Variance in estimated treatment effect : 
##                 0.001      |     0.034      |     -0.033         (0.06)
##             (0.000,0.017)  | (0.007,0.085)  | (-0.081,0.001) 
## 
## Total Gain: 
##                 0.027      |     0.184      |     -0.158         (0.06)
##             (0.004,0.129)  | (0.074,0.289)  | (-0.240,0.008)
```


See `?compare.trtsel` for more options.

Including fitted risks 
----------------------------
Alternative to including a marker and fitting a logistic model, the user can specify fitted risks for trt = 0 and trt = 1. In this case, no model fitting will be implemented and all bootstrap confidence intervals will be conditional on the provided fitted model. 


```r

# calculate model fit
mymod <- glm(event ~ trt * Y2, data = tsdata, family = binomial("logit"))

tsdata$fitted.t0 <- predict(mymod, newdata = data.frame(trt = 0, Y2 = tsdata$Y2), 
    type = "response")
tsdata$fitted.t1 <- predict(mymod, newdata = data.frame(trt = 1, Y2 = tsdata$Y2), 
    type = "response")


myfitted.trtsel <- trtsel(event = "event", trt = "trt", data = tsdata, fittedrisk.t0 = "fitted.t0", 
    fittedrisk.t1 = "fitted.t1", study.design = "randomized cohort", default.trt = "trt all")
```


We can now use this `trtsel` object just as before, but confidence intervals will be smaller because we do not account for the variation due to model fitting. 


```r
plot.trtsel(myfitted.trtsel, bootstraps = 50, plot.type = "risk", ci = "horizontal", 
    show.marker.axis = FALSE)
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20.png) 





References
------------------------
Janes H, Brown MD, Pepe MS, Huang Y. Statistical methods for evaluating and comparing biomarkers for patient treatment selection. *International Journal of Biostatistics* (under review).
