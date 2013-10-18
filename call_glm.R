All the functions that call 'glm'

get.coef.R #duh
 - trtsel
 - trtsel.boot
   - one.boot.eval
   - one.boot.compare
 - one.boot.plot

## test code to run it 

data(tsdata)

head(tsdata)

mymod <- glm(event~trt*Y1, data= tsdata, family = binomial("logit"))


tsdata$fitted.t0 <- predict(mymod, newdata=data.frame(trt = 0, Y1 = tsdata$Y1), type = "response")
tsdata$fitted.t1 <- predict(mymod, newdata=data.frame(trt = 1, Y1 = tsdata$Y1), type = "response")

#test my predictions

trtsel.Y1 <- trtsel( event ="event", trt = "trt", marker = "Y1", data = tsdata,
                     study.design = "randomized cohort", link = "logit", 
                     default.trt = "trt all")
trtsel.Y2 <- trtsel( event ="event", trt = "trt", marker = "Y2", data = tsdata,
                     study.design = "randomized cohort", link = "logit", 
                     default.trt = "trt all")

all.equal(trtsel.Y1$derived.data$fittedrisk.t0, unname(tsdata$fitted.t0))
all.equal(trtsel.Y1$derived.data$fittedrisk.t1, unname(tsdata$fitted.t1))

trtsel.fitted.1 <- trtsel( event ="event", trt = "trt",  
                         data = tsdata,
                         fitted_risk_t0 = "fitted.t0",
                         fitted_risk_t1 = "fitted.t1",
                         study.design = "randomized cohort", 
                         default.trt = "trt all")

mymod <- glm(event~trt*Y2, data= tsdata, family = binomial("logit"))


tsdata$fitted.t0 <- predict(mymod, newdata=data.frame(trt = 0, Y2 = tsdata$Y2), type = "response")
tsdata$fitted.t1 <- predict(mymod, newdata=data.frame(trt = 1, Y2 = tsdata$Y2), type = "response")


trtsel.fitted.2 <- trtsel( event ="event", trt = "trt",  
                           data = tsdata,
                           fitted_risk_t0 = "fitted.t0",
                           fitted_risk_t1 = "fitted.t1",
                           study.design = "randomized cohort", 
                           default.trt = "trt all")

#now we have to plot, eval, calibrate and compare....here we go!

#weird bumps for trtsel.fitted.1 ...maybe a sorting issue? 
tmp <- plot.trtsel(trtsel.fitted.1, bootstraps = 50, plot.type = "risk",
            ci = "horizontal") ### need to switch from using F.Y to F.delta

with(trtsel.fitted.1$derived.data, plot(1:1000,sort(fittedrisk.t1)))
# this works perfectly however....strange!
tmp <- plot.trtsel(trtsel.fitted.2, bootstraps = 50, plot.type = "risk",
                   ci = "horizontal") ### need to switch from using F.Y to F.delta



#works!
plot.trtsel(trtsel.fitted.1, bootstraps = 50, plot.type = "treatment effect", 
            ci = "horizontal")
plot.trtsel(trtsel.fitted.2, bootstraps = 50, plot.type = "treatment effect", 
            ci = "horizontal")

#works!
plot.trtsel(trtsel.fitted.1, bootstraps = 50, plot.type = "cdf", 
            ci = "vertical")
plot.trtsel(trtsel.fitted.2, bootstraps = 50, plot.type = "cdf", 
            ci = "vertical")

#eval

#works!
tmp <- eval.trtsel(trtsel.fitted.1, bootstraps = 50)

#calibrate should already work

calibrate.trtsel(trtsel.fitted)
calibrate.trtsel(trtsel.fitted, plot.type="treatment effect")

#now for compare.... it should break if we are comparing two different bootstrap approaches. 

compare.trtsel(trtsel.fitted.2, trtsel.fitted.1, bootstraps = 100, plot = TRUE, ci = "vertical")



