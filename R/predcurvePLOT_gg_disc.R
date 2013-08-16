predcurvePLOT_gg_disc <-
function(x, ci, ci.bounds, get.F, fixed.values, conf.bands, rho, trt.names, xlab, ylab, xlim, ylim, main, offset = .01, mar,...){ 

  
  fittedrisk.t0 <- x$derived.data$fittedrisk.t0
  fittedrisk.t1 <- x$derived.data$fittedrisk.t1
  marker <- x$derived.data$marker
  event <- x$derived.data$event
  trt <- x$derived.data$trt
  F.Y <- get.F(marker, event, trt, rho = rho)*100
  mydata <- data.frame(risk = fittedrisk.t0*(1-trt)+fittedrisk.t1*trt, trt = 1-trt, Fy = F.Y, marker)
  mydata <- mydata[with(mydata, order(Fy)),]
  mydata <- unique(mydata)
  mydata$lower <- rep(NA, nrow(mydata))
  mydata$upper <- rep(NA, nrow(mydata))

   
  if(!is.null(ci.bounds)){
    
  
  mval = sort(unique(marker))
  #order matters here!
  mydata[mydata$trt==0 & mydata$marker==mval[1], 5:6] <- ci.bounds[,1]
  mydata[mydata$trt==1 & mydata$marker==mval[1], 5:6] <- ci.bounds[,2]
  mydata[mydata$trt==0 & mydata$marker==mval[2], 5:6] <- ci.bounds[,3]
  mydata[mydata$trt==1 & mydata$marker==mval[2], 5:6] <- ci.bounds[,4]
  
  
  if(substr(ci, 1,1)=="h"){
    mydata[,5:6] <- mydata[,5:6]*100
   p <- ggplot(mydata, aes(y = Fy, x = risk, ymin = lower, ymax = upper, group = factor(trt), shape = factor(trt), linetype = factor(trt) ))
   p <- p + geom_pointrange(size = 1)
   p <- p + coord_flip()
  }else{
    p <- ggplot(mydata, aes(x = Fy, y = risk, ymin = lower, ymax = upper, group = factor(trt), shape = factor(trt), linetype = factor(trt) ))
    p <- p + geom_pointrange(size = 1)
    p <- p 
  }
  
  }else{
    if(substr(ci, 1,1)=="h"){
      p <- ggplot(mydata, aes(y = Fy, x = risk, ymin = lower, ymax = upper, group = factor(trt), shape = factor(trt), linetype = factor(trt) ))
      p <- p + geom_point(size = 3)
      p <- p + coord_flip()
    }else{
      p <- ggplot(mydata, aes(x = Fy, y = risk, ymin = lower, ymax = upper, group = factor(trt), shape = factor(trt), linetype = factor(trt) ))
      p <- p + geom_point(size = 3)

    }
  }

  if(is.null(xlab)) xlab <- "% population below marker value"
  if(is.null(ylab)) ylab <- "risk given marker"
  if(is.null(xlim)) xlim <- c(0,100)
  if(is.null(ylim)) ylim <- c(0,1)
  if(is.null(main)) main <- "Risk curves by treatment"
  breaks = sort(unique(mydata$Fy))
  
  if(substr(ci, 1, 1)=="h"){
    #we used coord_flip, so we switch x and y, otherwise dont

    #add x/y labels and main
    p <- p + ylab(xlab) + xlab(ylab)  + xlim(ylim[1], ylim[2]) + ggtitle(main) 
    #change the names for the legend
    p <- p + scale_shape_manual(values = c(16, 17), labels = trt.names) +
             scale_linetype_manual(values = c(1, 1), labels = trt.names)+
      theme(legend.title = element_blank(),  text = element_text(size=18))
           # legend.text = element_text(size = 16))
    p <- p + scale_y_continuous(breaks = breaks,limits = xlim) 
    p <- p + theme(plot.margin = unit(c(1,1,4,1), "lines"))
 
    p <- p + annotation_custom(grob = xaxisGrob( at = breaks, label = mval, gp = gpar(col = gray(.55), fontsize=15)), 
                               ymin = 0, ymax = 1, xmin = ylim[1]-diff(ylim)*.25, xmax = ylim[1]-diff(ylim)*.25)
    p <- p + annotation_custom(grob = textGrob( label = "marker value", gp = gpar( fontsize=18)), 
                               ymin = mean(xlim), ymax = mean(xlim), xmin = ylim[1]-diff(ylim)*.4, xmax = ylim[1]-diff(ylim)*.4)
    
  }else{

  #add x/y labels and main
  p <- p + xlab(xlab) + ylab(ylab) + ylim(ylim[1], ylim[2]) + ggtitle(main) 
  #change the names for the legend
  p <- p + scale_shape_manual(values = c(16, 17), labels = trt.names) +
    scale_linetype_manual(values = c(1, 1), labels = trt.names)+
    theme(legend.title = element_blank(),  text = element_text(size=18))
  #legend.text = element_text(size = 16))
  p <- p + scale_x_continuous(breaks = breaks, limits = xlim)
  
  p <- p + theme(plot.margin = unit(c(1,1,4,1), "lines"))
  
  p <- p + annotation_custom(grob = xaxisGrob( at = breaks, label = mval, gp = gpar(col = gray(.55), fontsize=15)), 
                             xmin = 0, xmax = 1, ymin = ylim[1]-diff(ylim)*.25, ymax = ylim[1]-diff(ylim)*.25)
  p <- p + annotation_custom(grob = textGrob( label = "marker value", gp = gpar( fontsize=18)), 
                             xmin = mean(xlim), xmax = mean(xlim), ymin = ylim[1]-diff(ylim)*.4, ymax = ylim[1]-diff(ylim)*.4)
  
  }

  # Code to override clipping
  gt <- ggplotGrob((p))
  gt$layout$clip[gt$layout$name=="panel"] <- "off"
  grid.draw(gt)
  

  
  return(list(p, mydata))
}
