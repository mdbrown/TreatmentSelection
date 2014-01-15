

#define the data.frame to check



## discrete cc example

data(tsdata)
data(tsdata_cc)

# c(cohort sample size, Pr(T=1) in cohort, Pr(Event==1) in cohort, fraction of cases)
rho.cc <- c(nrow(tsdata), mean(tsdata$trt==1), mean(tsdata$event==1), 1.0 )

#strong and weak cont
myTrtsel <- trtsel( event = "event", trt = "trt", marker ="Y2_disc", 
                    data = tsdata_cc,
                    cohort.attributes = rho.cc,
                    study.design = "nested case-control")

myTrtsel.all <- trtsel( event = "event", trt = "trt", marker ="Y2", 
                    data = tsdata)

