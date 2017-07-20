---
title: 'Tutorial for R package TreatmentSelection '
output:
  html_document:
    theme: united
---

#  {.tabset .tabset-fade .tabset-pills}

## Overview of the package

This package implements basic methodology for evaluating one or more biomarkers for their ability to guide patient treatment recommendations.

The methodology assumes that the data come from a randomized and controlled trial (RCT) comparing two treatment options, which we refer to as "treatment" and "no treatment". These could be, for example, two different active prophylactic or therapeutic interventions, or an experimental intervention and a standard of care.  Subjects are followed for the development of a binary clinical outcome or event within a specified time-frame following treatment/no treatment.  The biomarker(s) are assumed to be measured at baseline; they could be anything from patient demographics or clinical characteristics, to traditional biomarker measurments or the results of imaging or genetic or proteomic analyses.  The methodology accommodates settings where the biomarker(s) are measured on all RCT participants, and also settings in which participants are retrospectively sub-sampled based on clinical outcome and/or treatment assignment for marker measurement. 


## Install the package 

This tutorial uses the Treatment Selection package (v 2.0.0) to analyze the example data provided in the package.


The current version of the package is on github. Use the `devtools` package to install the package:


```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("mdbrown/TreatmentSelection")
```

First, load the example data set for this package called `tsdata`. Four markers are included in the data example, a ''weak'' and a ''strong'' marker ($Y1$ and $Y2$ respectively), along with a weak/strong discrete markers. 

## First steps 

Load the example data set for this package called `tsdata`. These are hypothetical data from an RCT with a binary outcome (`outcome`) and treatment assignment (`trt`). Four markers, measured on all RCT participants, are included-- a ''weak'' and a ''strong'' marker (`Y1` and `Y2` respectively), along with binary versions of these markers. 


```r
library(TreatmentSelection)
set.seed(12321)

data(tsdata)
data(surv_tsdata)

tsdata[1:5, ]
```

```
##   trt event      Y1      Y2 Y1_disc Y2_disc
## 1   1     1 39.9120 -0.8535       1       0
## 2   1     0  6.6820  0.2905       0       1
## 3   1     0  6.5820  0.0800       0       1
## 4   0     0  1.3581  1.1925       0       1
## 5   0     0  7.6820 -0.2070       0       0
```

### Evaluate performance of user-specified marker-based treatment rule

For a pre-defined marker-based treatment rule, we provide a simple function to estimate point estimates of performance measures. This is done using the `trtsel_measures()` function.  The user must specify a vector of clinical outcomes, a vector of treatment assigments, and a vector of marker-based treatment recommendations based on the pre-specified rule.

Here we let `Y1_disc` represent a user-specified treatment rule and evaluate its performance.  


```r
trtsel_measures(event = tsdata$event, trt = tsdata$trt, trt.rule = tsdata$Y1_disc, default.trt = "trt all" )
```

```
## Estimates of trt.effect are not provided. Only empirical estimates will be calculated.
```

```
## 
##   Summary Measure Estimates
##  -----------------------------------------------------------
##   Decrease in rate of outcomes under marker-based treatment rule (Theta)
##     Empirical:    0.011
##     Model Based:  NA
## 
##   Proportion recommended no treatment:    0.57
##   Proportion recommended treatment:    0.43
## 
##   Average benefit of no treatment among those recommended no treatment (B.neg)
##     Empirical:    0.019
##     Model Based:  NA
## 
##   Average benefit of treatment among those recommended treatment (B.pos)
##     Empirical:    0.106
##     Model Based:  NA
## 
##   Variance in estimated treatment effect:   NA
##   Total Gain:     NA
## 
##   Event Rates:
##  --------------------
##              Treat all       Treat None    Marker-based Treatment
##  Empirical:     0.217           0.251          0.206    
## 
```

```r
trtsel_measures(event = surv_tsdata$di, 
                time = surv_tsdata$xi,
                trt = surv_tsdata$trt, 
                trt.rule = as.numeric(surv_tsdata$Y > 0), 
                prediction.time = 1, 
                default.trt = "trt none" )
```

```
## Estimates of trt.effect are not provided. Only empirical estimates will be calculated.
```

```
## 
##   Summary Measure Estimates
##  -----------------------------------------------------------
##   Decrease in rate of outcomes under marker-based treatment rule (Theta)
##     Empirical:    -0.015
##     Model Based:  NA
## 
##   Proportion recommended no treatment:    0.484
##   Proportion recommended treatment:    0.516
## 
##   Average benefit of no treatment among those recommended no treatment (B.neg)
##     Empirical:    -0.037
##     Model Based:  NA
## 
##   Average benefit of treatment among those recommended treatment (B.pos)
##     Empirical:    -0.029
##     Model Based:  NA
## 
##   Variance in estimated treatment effect:   NA
##   Total Gain:     NA
## 
##   Event Rates:
##  --------------------
##              Treat all       Treat None    Marker-based Treatment
##  Empirical:     0.132           0.134          0.149    
## 
```


We can also fit our own risk model using GLM, use this model to develop a marker-based treatment recommendation, and evaluate its performance. This allows us to obtain model-based estimates of performance:



```r
mod <- glm(event~trt*Y1_disc,  data = tsdata, family = binomial())

tsdata.0 <- tsdata; 
tsdata.0$trt = 0 
tsdata.1 <- tsdata;
tsdata.1$trt = 1
delta.hat <- predict(mod, newdata= tsdata.0, type = "response") - predict(mod, newdata= tsdata.1, type = "response")

trtsel_measures(event = tsdata$event, trt = tsdata$trt, trt.rule = 1- tsdata$Y1_disc, trt.effect = delta.hat )
```

```
## 
##   Summary Measure Estimates
##  -----------------------------------------------------------
##   Decrease in rate of outcomes under marker-based treatment rule (Theta)
##     Empirical:    -0.046
##     Model Based:  -0.046
## 
##   Proportion recommended no treatment:    0.43
##   Proportion recommended treatment:    0.57
## 
##   Average benefit of no treatment among those recommended no treatment (B.neg)
##     Empirical:    -0.106
##     Model Based:  -0.106
## 
##   Average benefit of treatment among those recommended treatment (B.pos)
##     Empirical:    -0.019
##     Model Based:  -0.019
## 
##   Variance in estimated treatment effect:   0.004
##   Total Gain:     0.061
## 
##   Event Rates:
##  --------------------
##              Treat all       Treat None    Marker-based Treatment
##  Empirical:     0.217           0.251          0.263    
## 
```

## Create TrtSel objects 

An alternative to providing a pre-specified treatment rule the user can use the data to fit a risk model and to develop a treatment rule based on estimated treatment effect. 

The user can do this by first creating a treatment selection R object using the function `trtsel`. Creating this object entails estimating risk of the clinical outcome given treatment and marker, i.e. fitting a "risk model".  Here, logistic regression is used (`link="logit"`) and the formula is specified in the first argument.  The default treatment strategy-- that which would be recommended in the absence of marker measurements-- is specified as `default.trt="trt all"`.  The methodology considers marker-specific treatment rules or policies that recommend treatment if the marker-specific treatment effect-- on the risk difference scale-- is above a specified `thresh`; the default is `thresh = 0`.  (Alternatively, see the tab on "additional features"" to see how an arbitrary user-specified treatment rule can be evaluated.)  The `study design="RCT"` indicates that the markers are measured on all RCT participants.   




```r
tsdata$event.c <- tsdata$event + rnorm( nrow(tsdata), mean = 0, sd = .1 )
trtsel.Y1 <- trtsel(event.c ~ Y1*trt, 
                    treatment.name = "trt", 
                    data = tsdata, 
                    study.design = "RCT", 
                    default.trt = "trt all")
```

```
## Continuous outcome detected: setting family = gaussian(link = 'identity')
```

```r
trtsel.Y1
```

```
## Study design: RCT 
## 
## Model Fit:
## Family: gaussian 
## Link function: identity 
## 
##  Coefficients: 
##                 Estimate  Std. Error    t value     Pr(>|t|)
## (Intercept)  0.009816524 0.034405603  0.2853176 7.754601e-01
## Y1           0.009223341 0.001068410  8.6327768 2.342538e-17
## trt          0.075586414 0.047338544  1.5967203 1.106453e-01
## Y1:trt      -0.004432840 0.001425346 -3.1100095 1.923994e-03
## 
## 
## Derived Data: (first ten rows)
## 
##        event.c      Y1 trt fittedrisk.t0 fittedrisk.t1  trt.effect
## 1   1.10092454 39.9120   1    0.37793852    0.27660143  0.10133709
## 2   0.18109594  6.6820   1    0.07144689    0.11741307 -0.04596618
## 3   0.02623565  6.5820   1    0.07052456    0.11693402 -0.04640946
## 4  -0.15532924  1.3581   0    0.02234274    0.09190892 -0.06956617
## 5   0.13559407  7.6820   0    0.08067023    0.12220357 -0.04153334
## 6   0.04679060 41.1720   0    0.38955993    0.28263746  0.10692247
## 7  -0.04385784 19.4920   1    0.18959789    0.17877939  0.01081850
## 8   0.95397045 20.8220   1    0.20186494    0.18515076  0.01671418
## 9   0.04681453  6.9620   0    0.07402943    0.11875441 -0.04472498
## 10 -0.01950354  2.5020   0    0.03289332    0.09738877 -0.06449545
##    rec.no.trt
## 1           0
## 2           1
## 3           1
## 4           1
## 5           1
## 6           0
## 7           0
## 8           0
## 9           1
## 10          1
```



As we see above, the `trtsel.Y1` object contains information about the study design, the fitted risk model, the fitted risk given each treatment, and the estimated marker-specific treatment effect (on the risk difference scale) for each individual.  This object also contains the treatment recommendation for each subject, based on a treatment rule that recommends treatment if the estimated treatment effect is above `thresh`; here `thresh = 0`. 

This is what a `trtsel` object looks like for a binary marker. 


```r
# Y2_disc = as.numeric(Y2>0)

trtsel.Y2_disc <- trtsel(event ~ trt*(Y2_disc), 
                         treatment.name = "trt",
                         data = tsdata, 
                         study.design = "RCT", 
                         family = binomial("logit"))
```



See `?trtsel` for more information. 

Now that we have created trtsel objects, we can assess the calibration of the risk models; and plot, evaluate, and compare markers. 



## Assess model calibration 

It is important to assess the fit of the risk model to the data.  The package implements methods for examining calibration.  

Here we compare (average) predicted and observed risk values by decile ('group = 10') of fitted risk, for each treatment group.  The risks are shown on the log scale.  The Hosmer-Lemeshow goodness of fit test is used to detect evidence of mis-calibration by treatment group.































