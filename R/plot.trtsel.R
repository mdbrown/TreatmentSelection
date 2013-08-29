plot.trtsel <-
function(x, bootstraps = 500,  
            plot.type = "risk", 
            ci = "default", 
            alpha = .05, 
            fixed.values = NULL,  
            offset = 0.01, 
            conf.bands = TRUE, 
            conf.bandsN = 100, 
            trt.names = c("Treatment", "    No \nTreatment"), 
            xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, main = NULL, mar = NULL, ...)

{

  if(!is.trtsel(x)) stop("x must be an object of class 'trtsel' created by using the function 'trtsel' see ?trtsel for more help")
 
  if(!is.element(plot.type, c("risk", "treatment effect", "cdf"))){ 
    stop("plot.type must be one of \"risk\", \"treatment effect\", or \"cdf\"")
  }
  stopifnot(length(plot.type) ==1)
  
  if(!is.element(ci, c("default", "horizontal", "vertical", "none"))){ 
    stop("ci must be one of \"default\",  \"horizontal\", or  \"vertical\" or \"none\" ")
  }
  

  if(alpha<0 | alpha > 1) stop("Error: alpha should be between 0 and 1")
  if(bootstraps < 2) warning("Number of bootstraps must be greater than 1, bootstrap confidence intervals will not be computed") 
  if(ci == "none") bootstraps = 0; 

  #set default ci's 

  if(ci =="default"){
    #continuous marker
    if(is.null(x$model.fit$disc.marker.neg)){
      if(substr(plot.type, 1, 3) =="ris") ci = "horizontal"
      if(substr(plot.type, 1, 3) =="tre") ci = "horizontal"
      if(substr(plot.type, 1, 3) =="cdf") ci = "vertical"
    }else{
      
      if(substr(plot.type, 1, 3) =="ris") ci = "vertical"
      if(substr(plot.type, 1, 3) =="tre") ci = "vertical"
      if(substr(plot.type, 1, 3) =="cdf") ci = "horizontal"
    }

  
  }
  #no cdf plots for binary marker
  if(!is.null(x$model.fit$disc.marker.neg)){
  if(substr(plot.type, 1, 3) =="cdf") stop("cdf plots cannot be created for a binary marker. Please choose plot.type to be \"risk\" or \"treatment effect\" ")
  }
  #save the current plot parameters
  #old.par <- par(no.readonly = TRUE)



#extract the needed data from x, which is our TrtSel object
    
  marker <- x$derived.data$marker
  trt <- x$derived.data$trt
  event <- x$derived.data$event

  n <- length(marker)
  rho  <- x$model.fit$cohort.attributes
  study.design <- x$model.fit$study.design
  link <- x$model.fit$link
  boot.sample <- x$functions$boot.sample
  get.F <- x$functions$get.F
  delta <- x$derived.data$trt.effect   
  
  
 if(is.null(x$model.fit$disc.marker.neg)){
    plot.functions <- list(  predcurvePLOT_gg, trteffectPLOT_gg, CDFdeltaPLOT_gg)
    
  if(length(fixed.values)!=0) conf.bands = FALSE
  if(conf.bands & length(fixed.values)==0 ){
   #create fixed values

   if(substr(ci, 1,1 )=="v"){
       
      if(is.element(substr(plot.type, 1,3), c("tre", "ris"))) fixed.values = seq(from = 0.001, to = 1, length.out = conf.bandsN)
      else if(substr(plot.type, 1,3)=="cdf") fixed.values = seq(from = min(delta), to = max(delta), length.out = conf.bandsN)
      
   }else{
      
      if(is.element(substr(plot.type, 1,3), c("cdf"))) fixed.values = seq(from = 0.001, to = 1, length.out = conf.bandsN)
      else if(substr(plot.type, 1,3)=="tre") fixed.values = seq(from = min(delta), to = max(delta), length.out = conf.bandsN)
      else if(substr(plot.type, 1,3)=="ris"){
       allrisks <- c(x$derived.data$fittedrisk.t0, x$derived.data$fittedrisk.t1)
       fixed.values = seq(from = min(allrisks), to = max(allrisks), length.out = conf.bandsN)

      }

   }
   offset = 0

   }
    ##Bootstrap for confidence intervals...
    #bootstrapping done by fixing response and strapping marker 
    
    if((conf.bands & bootstraps>1) | (length(fixed.values)>0 & bootstraps > 1)){ 
      ci.bounds <- get.plot.ci(marker, trt, event, study.design, rho, plot.type, ci, bootstraps, fixed.values =fixed.values, obp.boot.sample = boot.sample, obp.get.F = get.F, link = link, alpha = alpha)
    }else{ 
      ci.bounds <- NULL
      conf.bands = FALSE
    }
    ## end Bootstrap
    
    
    
    
  }else{
    #discrete marker...ignore fixed values and ci bands, we only need ci's around the observed points
    
    plot.functions <- list(  predcurvePLOT_gg_disc, trteffectPLOT_gg_disc, CDFdeltaPLOT_gg_disc)

    ##Bootstrap for confidence intervals...
    #much simpler for a discrete marker

    if(( bootstraps>1) ){ 
      
      ci.bounds <- get.plot.ci_disc(marker, trt, event, rho, plot.type, ci, bootstraps, obp.boot.sample = boot.sample, obp.get.F = get.F, alpha = alpha)
      ci = ci.bounds$newci
      ci.bounds = ci.bounds$myconf.ints
      }else{ 
      ci.bounds <- NULL
      conf.bands = FALSE
    }
    ## end Bootstrap
    
    
  }

  tmp.plotfun <- plot.functions[[match(plot.type, c("risk", "treatment effect", "cdf"))]]
   

if(is.null(x$model.fit$disc.marker.neg)){
  if(substring(plot.type, 1, 4) == "risk"){

  curves <- tmp.plotfun(x, ci, ci.bounds, get.F, fixed.values, conf.bands,  rho, trt.names, xlab, ylab, xlim, ylim, main, offset = offset,mar,  ...)

    if(!is.null(ci.bounds)){
      ci.bounds <- data.frame(t(curves[[2]]))
      ci.bounds <- cbind(fixed.values, ci.bounds)
      names(ci.bounds) <- c("fixed.values", "trt0.lower", "trt0.upper", "trt1.lower", "trt1.upper")
      }
    }else{

      curves <- tmp.plotfun(x, ci, ci.bounds, get.F, fixed.values, conf.bands, rho, xlab, ylab, xlim, ylim, main, mar = mar,  ...)
      if(!is.null(ci.bounds)){
        ci.bounds <- data.frame(t(curves[[2]]))
        ci.bounds <- cbind(fixed.values, ci.bounds)
        names(ci.bounds) <- c("fixed.values", "lower", "upper")
      }
    }
  
}else{
  
  if(substring(plot.type, 1, 4) == "risk"){
    
    curves <- tmp.plotfun(x, ci, ci.bounds, get.F, fixed.values, conf.bands,  rho, trt.names, xlab, ylab, xlim, ylim, main, offset = offset,mar,  ...)
    
    if(!is.null(ci.bounds)){
      ci.bounds <- data.frame(curves[[2]])
    }
  }else{
    
    curves <- tmp.plotfun(x, ci, ci.bounds, get.F, fixed.values, conf.bands, rho, xlab, ylab, xlim, ylim, main, mar = mar,  ...)
   
    if(!is.null(ci.bounds)){
      ci.bounds <- data.frame(curves[[2]])

    }
  }
  
}    

  #par(old.par)

invisible(list("plot" = curves[[1]], "ci.bounds" = ci.bounds))
}
