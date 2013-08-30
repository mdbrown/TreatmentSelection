

# This R script uses the Treatment Selection package to analyze the data example provided in the package

# Install the package

#windows
install.packages(pkgs = "DIRECTORY/TO/PACKAGE/TreatmentSelection_1.2.zip", repos = NULL)  # only have to do this once
#unix 
install.packages(pkgs = "DIRECTORY/TO/PACKAGE/TreatmentSelection_1.2.tar.gz", repos = NULL)

library("TreatmentSelection")

# Load the package data example
data(tsdata) 

tsdata[1:10, ]


attach(tsdata)

###########################
## Create TrtSel objects
###########################

# Two markers are contained in the data example, a "weak" and "strong" marker

# "Weak" marker
trtsel.Y1 <- trtsel( event ="event", trt = "trt", marker = "Y1", data = tsdata,
                     study.design = "randomized cohort", link = "logit", 
                     default.trt = "trt all")

trtsel.Y1

head(trtsel.Y1$derived.data) #shows data, along with fitted risks and estimated treatment effects
trtsel.Y1$model.fit  #information regarding the model fit

# "strong" marker
trtsel.Y2 <- trtsel( event ="event", trt = "trt", marker = "Y2", data = tsdata,
                     study.design = "randomized cohort", link = "logit", 
                     default.trt = "trt none")
trtsel.Y2

detach(tsdata)

##########################
## Use the plot function 
##########################

# Plot risk curves

tmp <- plot.trtsel(trtsel.Y1, main = "Y1: Oncotype-DX-like marker", plot.type = "risk", ci = "vertical",
            conf.bands = FALSE, fixed.values = c(20, 50), offset = .01, bootstraps = 50,trt.names=c("chemo.","no chemo."))

tmp <- plot.trtsel(trtsel.Y1, main = "Y1: Oncotype-DX-like marker", plot.type = "risk", ci = "horizontal",
                    bootstraps = 50,trt.names=c("chemo.","no chemo."))


tmp <- plot.trtsel(trtsel.Y2, main = "Y1: Oncotype-DX-like marker", plot.type = "risk", ci = "vertical",
                   conf.bands = FALSE, fixed.values = c(20, 50), offset = .01, bootstraps = 50,trt.names=c("chemo.","no chemo."))

#tmp is now made up of a list with elements "curves" and "ci.bounds"

head(tmp$curves) #gives points of the plotted curve
tmp$ci.bounds  #shows bounds



plot.trtsel(trtsel.Y1, main = "Y1: Oncotype-DX-like marker", plot.type = "cdf", bootstraps = 50,
                    ci = "horizontal", offset = .015)

plot.trtsel(trtsel.Y2, main = "Y1: Oncotype-DX-like marker", plot.type = "cdf", bootstraps = 50,
            trt.names=c("chemo.","no chemo."), fixed.values = c(.5, .3), conf.bands = TRUE, ci = "horizontal", offset = .015)

plot.trtsel(trtsel.Y2, main = "Y2: Strong marker", plot.type = "cdf", ci = "horizontal",
            bootstraps = 50)


#default is to plot with confidence intervals for the whole curve

plot.trtsel(trtsel.Y2, main = "Y2: Strong marker", plot.type = "risk", bootstraps = 50,
     trt.names=c("chemo.","no chemo."))

# Plot treatment effect curves
plot.trtsel(trtsel.Y1, main = "Y1: Oncotype-DX-like marker", plot.type = "treatment effect", ci = "vertical",  bootstraps = 50 )
plot.trtsel(trtsel.Y2, main = "Y2: Strong marker", plot.type = "treatment effect", ci = "horizontal", bootstraps = 50)


plot.trtsel(trtsel.Y1, main = "Y1: Oncotype-DX-like marker", plot.type = "cdf", ci = "vertical",  bootstraps = 50)#, fixed.values = seq(from=-1, to=1, by = .1) , xlim=c(-1, 1))


#for more help, see ?plot.trtsel

################################
## Evaluate marker performance
################################


estimates.Y1 <- eval.trtsel(trtsel.Y1, bootstraps = 50)
estimates.Y1 

estimates.Y1$estimates #point estimates
estimates.Y1$conf.intervals 

# "strong" marker
trtsel.Y2 <- trtsel( event ="event", trt = "trt", marker = "Y2", data = tsdata,
                     study.design = "randomized cohort", link = "logit", 
                     default.trt = "trt all")


#don't compute confidence intervals
estimates.Y2 <- eval.trtsel(trtsel.Y2, bootstraps = 50)
estimates.Y2


#for more information see ?eval.trtsel



##############################
## Assess model calibration
##############################



#default is to show calibration plot with 10 groups
cali.Y2 <- calibrate.trtsel(trtsel.Y2)
cali.Y2

#no plot
cali.Y1 <- calibrate.trtsel(trtsel.Y1, plot.type=NA, groups = 8)
cali.Y1

#risk among non-treated individuals
calibrate.trtsel(trtsel.Y2,  plot.type = "risk.t0")
#treated individuals
calibrate.trtsel(trtsel.Y2,  plot.type = "risk.t1")

#treatment effect, with more groups

calibrate.trtsel(trtsel.Y2, plot.type="treatment effect", groups = 15, main = "main title here")

#for more help and to see all the options, see ?calibrate.trtsel

###############################
## Compare marker performance
###############################


# Compare the markers based on summary measures
mycompare <- compare.trtsel(trtsel1 = trtsel.Y1, trtsel2 = trtsel.Y2,
                                bootstraps = 50, plot = TRUE, fixed.values = c(.05), ci = "vertical", offset = .01, conf.bands = TRUE)
mycompare

mycompare$estimates.marker1 #estimates from trtsel1

mycompare$estimates.marker2 

mycompare$ci.marker1

#see ?compare.trtsel for a list of all output values

#no confidence intervals
compare.trtsel(trtsel1 = trtsel.Y1, trtsel2 = trtsel.Y2,
                            bootstraps = 50, plot = TRUE, ci = "horizontal")
#with confidence bands
compare.trtsel(trtsel1 = trtsel.Y3, trtsel2 = trtsel.Y2,
                            bootstraps = 50, plot = TRUE, ci = "horizontal", conf.bands = TRUE)




tsdata$Y3 <- as.numeric(!(tsdata$Y2>-.5))+1
tsdata$Y3[tsdata$Y3 ==2] <- 0

trtsel.Y3 <- trtsel( event ="event", trt = "trt", marker = "Y3", data = tsdata,
                     study.design = "randomized cohort", link = "logit", 
                     default.trt = "trt all")

trtsel.Y3
tmp = eval.trtsel(trtsel.Y3, bootstrap = 50)


plot.trtsel(trtsel.Y3, bootstrap = 50, plot.type ="risk" )

plot.trtsel(trtsel.Y3, bootstrap = 50, plot.type ="treatment effect"  )

plot.trtsel(trtsel.Y3, bootstrap = 50, plot.type ="cdf" )



tsdata$Y4 <- round(tsdata$Y2, 1)

trtsel.Y4 <- trtsel( event ="event", trt = "trt", marker = "Y4", data = tsdata[1:25,],
                     study.design = "randomized cohort", link = "logit", 
                     default.trt = "trt all")

trtsel.Y4
eval.trtsel(trtsel.Y5, bootstrap = 50)

tmp <- plot(trtsel.Y3, bootstrap = 50, ci = "horizontal")

tmp <- plot.trtsel(trtsel.Y3,bootstrap = 50, plot = "cdf", ci = "vertical")

plot.trtsel(trtsel.Y1, bootstrap = 50, plot = "treatment effect")
plot.trtsel(trtsel.Y5, bootstrap = 50, plot = "treatment effect")


tsdata$Y5 <- -tsdata$Y1 
trtsel.Y5 <- trtsel( event ="event", trt = "trt", marker = "Y5", data = tsdata,
                     study.design = "randomized cohort", link = "logit", 
                     default.trt = "trt all")

trtsel.Y1














tmp <- plot.trtsel(trtsel.Y1, plot.type = "risk", ci = "vertical",
                   bootstraps = 50)
tmp <- plot.trtsel(trtsel.Y1, plot.type = "risk", ci = "horizontal",
                   bootstraps = 50)

tmp <- plot.trtsel(trtsel.Y1, plot.type = "cdf", ci = "vertical",
                   bootstraps = 50)
tmp <- plot.trtsel(trtsel.Y1, plot.type = "cdf", ci = "horizontal",
                   bootstraps = 50)

tmp <- plot.trtsel(trtsel.Y1, plot.type = "treatment effect", ci = "vertical",
                   bootstraps = 50)
tmp <- plot.trtsel(trtsel.Y1, plot.type = "treatment effect", ci = "horizontal",
                   bootstraps = 50)

#cutoff
tmp <- plot.trtsel(trtsel.Y3, plot.type = "risk", ci = "vertical",
                   bootstraps = 50)
tmp <- plot.trtsel(trtsel.Y3, plot.type = "risk", ci = "horizontal",
                   bootstraps = 50)

tmp <- plot.trtsel(trtsel.Y3, plot.type = "treatment effect", ci = "vertical",
                   bootstraps = 50)
tmp <- plot.trtsel(trtsel.Y3, plot.type = "treatment effect", ci = "horizontal",
                   bootstraps = 50)



tmp <- compare.trtsel(trtsel1 = trtsel.Y1, trtsel2 = trtsel.Y2,
               bootstraps = 50, plot = TRUE, ci = "horizontal", conf.bands = TRUE, 
                      fixed.values = c(-0.5, .1))

tmp <- compare.trtsel(trtsel1 = trtsel.Y1, trtsel2 = trtsel.Y2,
                      bootstraps = 50, plot = TRUE, ci = "vertical", conf.bands = TRUE,
                      fixed.values = c(25, 75), offset = 5, marker.names = c("weak", "strong"))


