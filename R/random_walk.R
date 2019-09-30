rm(list = ls())
library(ggplot2); library(ggthemes); library(gridExtra)
setwd("~/Documents/Attention-Guidance-Visual-Search/")
source('R/functions/rw.R')

## Histogram ----------
nobs <- 1e3
res0 <- random_walk(nobs, a=20, z = 10, u = 1e2, verbose=T, maxsize=5e2)
res1 <- random_walk(nobs, a=20, z = 10, u = 3e2, verbose=T, maxsize=5e2)
res2 <- random_walk(nobs, a=15, z = 7.5, u = 1e2, verbose=T, maxsize=5e2)
res3 <- random_walk(nobs, a=20, z = 10, u = 1e2, verbose=T, maxsize=5e2)

str(res0)
# List of 3
# $ R : num [1:1000] 1 1 1 1 1 1 0 0 1 0 ...
# $ RT: num [1:1000] 0.077 0.038 0.066 0.17 0.051 0.093 0.066 0.075 0.057 0.047 ...
# $ y : num [1:500, 1:1000] 10 8.85 8.16 10.62 8.93 ...

d0 <- make_df(res0)
d1 <- make_df(res1)
d2 <- make_df(res2)
d3 <- make_df(res3)

str(d0)
# 'data.frame':	1000 obs. of  2 variables:
# $ value: num  0.066 0.075 0.047 0.071 0.138 0.113 0.044 0.093 0.087 0.063 ...
# $ C    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...

## Set up for ggplot2's data.frame
bins <- seq(0, max(c(res0$RT, res1$RT, res2$RT, res3$RT))+.01, by=.01)

d0$condition <- rep('low', nrow(d0))
d1$condition <- rep('high', nrow(d1))
d2$condition <- rep('low', nrow(d2))
d3$condition <- rep('high', nrow(d3))

d0$gp <- rep('v', nrow(d0))
d1$gp <- rep('v', nrow(d1))
d2$gp <- rep('a', nrow(d2))
d3$gp <- rep('a', nrow(d3))

d0 <- rbind(d0, d1, d2, d3)
d0$condition <- factor(d0$condition)
d0$gp <- factor(d0$gp)
d0$acc <- ifelse(d0$C==TRUE, 'O', 'X') 

tibble::as_tibble(d0)
# A tibble: 4,000 x 5
# value C     condition gp    acc  
# <dbl> <lgl> <fct>     <fct> <chr>
# 1 0.066 FALSE low       v     X    
# 2 0.075 FALSE low       v     X    
# 3 0.047 FALSE low       v     X    
# 4 0.071 FALSE low       v     X    
# 5 0.138 FALSE low       v     X    
# 6 0.113 FALSE low       v     X    
# 7 0.044 FALSE low       v     X    
# 8 0.093 FALSE low       v     X    
# 9 0.087 FALSE low       v     X    
# 10 0.063 FALSE low       v     X    
# ... with 3,990 more rows

p0 <- ggplot(d0[d0$C==TRUE,], aes(x = value, fill = condition)) +
  geom_histogram(color='grey50', 
                 position = "identity", 
                 breaks = bins, 
                 alpha = .5) +
  xlab('Time') + ylab('Frequency') +
  facet_grid(.~gp, switch = "y") 

p0

## Random walk plot ----------
ls0 <- ls1 <- ls2 <-  numeric(nobs)

for(i in 1:nobs) {
  ls0[i] <- min( which(is.na(res0$y[,i])) ) - 1
  ls1[i] <- min( which(is.na(res1$y[,i])) ) - 1
  ls2[i] <- min( which(is.na(res2$y[,i])) ) - 1
}

i <- sample(1:nobs, 1)
tmp0 <- max(c(ls0[i], ls1[i], ls2[i]))
tmp1 <- max(c(res0$y[,i], res1$y[,i], res2$y[,i]), na.rm=TRUE)

xlim <- c(0, tmp0)
ylim <- c(0, tmp1)

x <- c(0:ls0[i], 0:ls1[i], 0:ls2[i])
y <- c(res0$y[1:(ls0[i]+1),i], 
       res1$y[1:(ls1[i]+1),i],
       res2$y[1:(ls2[i]+1),i])
step <- c(length(0:ls0[i]),
          length(0:ls1[i]),
          length(0:ls2[i]))
tr <- c( rep('v=.1, a=20', step[1]),
rep('v=.3, a=20', step[2]),
rep('v=.1, a=15', step[3]))

d1 <- data.frame(x=x, y=y, tr=factor(tr))
tibble::as_tibble(d1)

p1 <- ggplot(d1, aes(x=x, y=y, color=tr, gp=tr)) +
  geom_line(size=.8) +
  geom_hline(yintercept = 0, size=.3, linetype='dotted') +
  geom_hline(yintercept = 15, size=.3, linetype='dotted') +
  geom_hline(yintercept = 20, size=.3, linetype='dotted') +
  xlab('Time step') + ylab('Evidence')
p1


p2 <- p0 + theme_few() + pub_size(c(.9, .8))
p3 <- p1 + theme_few() + pub_size(c(.9, .25))

## png("figs/histogram.png", 800, 600)
grid.arrange(p2, p3)
## dev.off()
# save(d0, d1, p0, p1, file='data/histogram.RData')

## load('data/histogram.RData') ----------------
# p2 <- p0 + theme_few() + pub_size(c(.9, .8),  basesize=12)
# p3 <- p1 + theme_few() + pub_size(c(.85, 25), basesize=12)
# pdf("figs/histogram.pdf", width = 4, height = 4)
# grid.arrange(p2, p3)
# dev.off()

