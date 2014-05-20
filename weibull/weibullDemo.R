# Disclaimer --------------------------------------------
# Author: Yishin Lin
# Date: 20 May, 2014
# Description: 
rm(list=ls())
library(plyr); library(ggplot2); library(grid); library(FAdist)

# Generate parameters -------------------------------------------------
n <- 3001
x <- seq(0, 1.5, length=n)

# scale changes does not move the curve, rather it change the 
# height (kurtosis, and skew) and the sd.
scaley <- c(.3,.4)

# shift changes only move an identical curve left or right
shifty <- c(.1,.2)

# the larger the shape, the peakier a distribution is.
# shape changes, it shift the mean as well as changing the sd, skew 
# and kurtosis.
shapey <- c(1.3,2.3) 

# Generate ys --------------------------------------------------------
scale1y <- dweibull3(x,thres=.2, shape=1.3, 
                 scale=scaley[1]) 
scale2y <- dweibull3(x,thres=.2, shape=1.3, 
                 scale=scaley[2]) 
shift1y <- dweibull3(x,thres=shifty[1], shape=1.3, 
                     scale=.32) 
shift2y <- dweibull3(x,thres=shifty[2], shape=1.3, 
                     scale=.32) 
shape1y <- dweibull3(x,thres=.2, shape=shapey[1], 
                     scale=.32) 
shape2y <- dweibull3(x,thres=.2, shape=shapey[2], 
                     scale=.32) 

# Bind as a data frame ------------------------------------------------
x0 <- data.frame(x=rep(x, 6), 
                   density=c(scale1y,scale2y,
                             shift1y,shift2y,
                             shape1y,shape2y),
                   cond=rep(rep(c(1,2), each=n), 3),
                   para=rep(c("scale", "shift", "shape"), each=2*n))

# Plot figures -------------------------------------------------------
p <- ggplot(x0, aes(x=x, y=density, colour=factor(cond))) + 
  geom_line(size=2) + theme_bw() + facet_grid(.~para) +
  scale_x_continuous(name = "RT (s)") +
  scale_y_continuous(name = "Probability density") +
  scale_colour_grey(start=.1, end=.8) +
  theme(axis.title.x = element_text(size=34), 
        axis.text.x  = element_text(angle=80, size=30, 
                                    vjust = 1, hjust = 1),   
        axis.title.y = element_text(angle=90, size=34),
        axis.text.y  = element_text(size=30),
        strip.text.x = element_text(size=30), 
        strip.background = element_blank(),
        strip.text.y = element_text(size=30, angle=90),
        legend.position=  "none")

jpeg(filename = "./figures/weibull/weibullDemo.jpeg",
     width = 1280, height = 1024, units = "px", pointsize = 8,
     quality = 95, bg = "white")
p
dev.off()

