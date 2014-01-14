## This script comprehensively checks all functionality of the 
## TreatmentSelection package function 'plot' 


## this script is meant to be called from another script, which will define:

# rho
# a trtsel object called 'myTrtsel'
# fileName: where to print all the plots to




 ## plot
   #risk curves
   plot.trtsel(myTrtsel, bootstraps = 50, plot.type = "risk", 
               ci = "horizontal")
   plot.trtsel(myTrtsel, bootstraps = 50, plot.type = "risk", 
               ci = "horizontal", fixed.values = seq(0, 1, by = .2))
   
   
   plot.trtsel(myTrtsel, bootstraps = 50, plot.type = "risk", 
               ci = "vertical")
   plot.trtsel(myTrtsel, bootstraps = 50, plot.type = "risk", 
               ci = "vertical", fixed.values = seq(0, 100, by = 20))
   
   

   #trt effect
   plot.trtsel(myTrtsel, bootstraps = 50, plot.type = "treatment effect")

   #cdf
   plot.trtsel(myTrtsel, bootstraps = 50, plot.type = "cdf")

