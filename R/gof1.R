#########################################################################0
## Fixed cue vs. varied cue
## Version: 0.0.3
## Authors: Yi-Shin Lin (yishinlin001@gmail.com)
## Date: 25 Sept, 2019 -- Draft one
## License: GPL 2
## Description: 1. 
rm( list = ls() )
setwd("/media/yslin/MERLIN/Documents/Attention-Guidance-Visual-Search/")
setwd("D:/Documents/Attention-Guidance-Visual-Search")

require(ggdmc) ## version 0.2.7.0
require(ggplot2)
load("data/visual_search.RData")
load("data/modelling_data/e1hfit3.RData") ## Q model  a~Q & v~SS
load("data/modelling_data/e1hfit4.RData") ## PrimingQ a~Q & v~SS+Q
load("data/E1-pp.RData")
source("R/functions/predict_one.R")


## Prepare data ------------
e1$R <- factor( ifelse(e1$R == "L", "left", ifelse(e1$R == "R", "right", NA)))
d1 <- data.table::data.table(e1)
nsub <- length(table(d1$s))
levels(d1$s)
d1$s <- factor( ifelse(d1$s =='E1-1', '1',
        ifelse(d1$s =='E1-10', '10',
        ifelse(d1$s =='E1-11', '11',
        ifelse(d1$s =='E1-12', '12',
        ifelse(d1$s =='E1-13', '13',
        ifelse(d1$s =='E1-14', '14',
        ifelse(d1$s =='E1-15', '15',
        ifelse(d1$s =='E1-16', '16',
        ifelse(d1$s =='E1-17', '17',
        ifelse(d1$s =='E1-18', '18',
        ifelse(d1$s =='E1-19', '19',
        ifelse(d1$s =='E1-20', '20',
        ifelse(d1$s =='E1-2', '2',
        ifelse(d1$s =='E1-3', '3',
        ifelse(d1$s =='E1-4', '4',
        ifelse(d1$s =='E1-5', '5',
        ifelse(d1$s =='E1-6', '6',
        ifelse(d1$s =='E1-7', '7',
        ifelse(d1$s =='E1-8', '8',
        ifelse(d1$s =='E1-9', '9', NA)))))))))))))))))))) )

subjects0 <- c("1", "10","11", "12","13","14","15","16","17","18","19",
               "2", "20", "3", "4","5","6","7","8","9")
names(hfit3) <- subjects0
names(hfit4) <- subjects0
subjects <- c("1", "2", "3", "4","5","6","7","8","9","10","11",
              "12","13","14","15","16","17","18","19","20")

# Summed BPIC: -27095.49  ## a~Q & v~SS
# Summed BPIC: -27099.7   ## a~Q & v~SS+Q model4 = M3
dev3 <- DIC(hfit3, BPIC=TRUE)
dev4 <- DIC(hfit4, BPIC=TRUE)

 pp3 <- pp4 <- vector('list', nsub)
 names(pp3) <- names(pp4) <- subjects
 for(i in 1:nsub) {
   j <- subjects[i]
   pp3[[j]] <- predict_one(hfit3[[j]], xlim = c(0, 5))
   pp4[[j]] <- predict_one(hfit4[[j]], xlim = c(0, 5))
   pp3[[j]]$C <- ifelse(pp3[[j]]$S == "L" & pp3[[j]]$R == "left",  TRUE,
                 ifelse(pp3[[j]]$S == "R" & pp3[[j]]$R == "right", TRUE,
                 ifelse(pp3[[j]]$S == "L" & pp3[[j]]$R == "right", FALSE,
                 ifelse(pp3[[j]]$S == "R" & pp3[[j]]$R == "left",  FALSE, NA))))
   pp4[[j]]$C <- ifelse(pp4[[j]]$S == "L" & pp4[[j]]$R == "left",  TRUE,
                 ifelse(pp4[[j]]$S == "R" & pp4[[j]]$R == "right", TRUE,
                 ifelse(pp4[[j]]$S == "L" & pp4[[j]]$R == "right", FALSE,
                 ifelse(pp4[[j]]$S == "R" & pp4[[j]]$R == "left",  FALSE, NA))))
 }

save(pp3, pp4, file = "data/E1-pp.RData")

## Goodness-of-fit ----
d1True <- data.table::data.table( d1[d1$C==TRUE, c('s', 'SS', 'Q', 'S', 'C', 'RT')] )
dplyr::tbl_df(d1True)

tmp <- matrix(numeric(nsub*2), ncol=2)
for(i in 1:nsub) {
  j <- subjects[i]
  tmp[i,1] <- min( pp3[[ j ]]$RT )
  tmp[i,2] <- max( pp3[[ j ]]$RT )
}



scale <- .06 ## bin size
lb <- min(d1True$RT, tmp)-scale
ub <- max(d1True$RT, tmp)+scale
bin <- seq(lb, ub, scale); bin

plot_3F_dat <- plot_5F_dat <- plot_7F_dat <- plot_9F_dat <- vector("list", length=nsub)
plot_3V_dat <- plot_5V_dat <- plot_7V_dat <- plot_9V_dat <- vector("list", length=nsub)
sim_3F_dat <- sim_5F_dat <- sim_7F_dat <- sim_9F_dat <- vector("list", length=nsub)
sim_3V_dat <- sim_5V_dat <- sim_7V_dat <- sim_9V_dat <- vector("list", length=nsub)

for(i in 1:nsub) {
  plot_3F_dat[[i]] <- hist(d1True[s==subjects[i] & SS == '3' & Q == 'F']$RT, 
                           breaks = bin, plot = FALSE)
  plot_5F_dat[[i]] <- hist(d1True[s==subjects[i] & SS == '5' & Q == 'F']$RT, 
                           breaks = bin, plot = FALSE)
  plot_7F_dat[[i]] <- hist(d1True[s==subjects[i] & SS == '7' & Q == 'F']$RT, 
                           breaks = bin, plot = FALSE)
  plot_9F_dat[[i]] <- hist(d1True[s==subjects[i] & SS == '9' & Q == 'F']$RT, 
                           breaks = bin, plot = FALSE)
  
  plot_3V_dat[[i]] <- hist(d1True[s==subjects[i] & SS == '3' & Q == 'V']$RT, 
                           breaks = bin, plot = FALSE)
  plot_5V_dat[[i]] <- hist(d1True[s==subjects[i] & SS == '5' & Q == 'V']$RT, 
                           breaks = bin, plot = FALSE)
  plot_7V_dat[[i]] <- hist(d1True[s==subjects[i] & SS == '7' & Q == 'V']$RT, 
                           breaks = bin, plot = FALSE)
  plot_9V_dat[[i]] <- hist(d1True[s==subjects[i] & SS == '9' & Q == 'V']$RT, 
                           breaks = bin, plot = FALSE)
  
  sim_3F_dat[[i]] <- pp3[[subjects[i]]][SS == '3' & Q == 'F']
  sim_5F_dat[[i]] <- pp3[[subjects[i]]][SS == '5' & Q == 'F']
  sim_7F_dat[[i]] <- pp3[[subjects[i]]][SS == '7' & Q == 'F']
  sim_9F_dat[[i]] <- pp3[[subjects[i]]][SS == '9' & Q == 'F']
  
  sim_3V_dat[[i]] <- pp3[[subjects[i]]][SS == '3' & Q == 'V']
  sim_5V_dat[[i]] <- pp3[[subjects[i]]][SS == '5' & Q == 'V']
  sim_7V_dat[[i]] <- pp3[[subjects[i]]][SS == '7' & Q == 'V']
  sim_9V_dat[[i]] <- pp3[[subjects[i]]][SS == '9' & Q == 'V']
}


names(plot_3F_dat) <- subjects
names(plot_5F_dat) <- subjects
names(plot_7F_dat) <- subjects
names(plot_9F_dat) <- subjects
names(plot_3V_dat) <- subjects
names(plot_5V_dat) <- subjects
names(plot_7V_dat) <- subjects
names(plot_9V_dat) <- subjects

names(sim_3F_dat) <- subjects
names(sim_5F_dat) <- subjects
names(sim_7F_dat) <- subjects
names(sim_9F_dat) <- subjects
names(sim_3V_dat) <- subjects
names(sim_5V_dat) <- subjects
names(sim_7V_dat) <- subjects
names(sim_9V_dat) <- subjects


max_count <- 0
tmp <- numeric(8)
for(i in 1:nsub) {
  tmp[1] <- max( plot_3F_dat[[i]]$counts )
  tmp[2] <- max( plot_5F_dat[[i]]$counts )
  tmp[3] <- max( plot_7F_dat[[i]]$counts )
  tmp[4] <- max( plot_9F_dat[[i]]$counts )
  tmp[5] <- max( plot_3V_dat[[i]]$counts )
  tmp[6] <- max( plot_5V_dat[[i]]$counts )
  tmp[7] <- max( plot_7V_dat[[i]]$counts )
  tmp[9] <- max( plot_9V_dat[[i]]$counts )
  
  tmp0 <- max(tmp)
  if(tmp0 > max_count) max_count <- tmp0
  
}

max_count
max(d1True$RT)

fontsize <- 1.5
mainsize <- 2
# xlim <- c(lb, ub)
xlim <- c(0, 2)
ylim <- c(0, max_count)
grayscale <- gray(0:8 / 8, alpha=1/2)
fontsize <- 1.5

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")

obj0 <- list(plot_3F_dat, plot_5F_dat, plot_7F_dat, plot_9F_dat)
obj1 <- list(sim_3F_dat, sim_5F_dat, sim_7F_dat, sim_9F_dat)
obj2 <- list(plot_3V_dat, plot_5V_dat, plot_7V_dat, plot_9V_dat)
obj3 <- list(sim_3V_dat, sim_5V_dat, sim_7V_dat, sim_9V_dat)
names(obj0) <- c('3', '5', '7', '9')
names(obj1) <- c('3', '5', '7', '9')
names(obj2) <- c('3', '5', '7', '9')
names(obj3) <- c('3', '5', '7', '9')

x0 <- obj0[[1]]
x1 <- obj0[[2]]
x2 <- obj0[[3]]
x3 <- obj0[[4]]
y0 <- obj1[[1]]
y1 <- obj1[[2]]
y2 <- obj1[[3]]
y3 <- obj1[[4]]

z0 <- obj2[[1]]
z1 <- obj2[[2]]
z2 <- obj2[[3]]
z3 <- obj2[[4]]
w0 <- obj3[[1]]
w1 <- obj3[[2]]
w2 <- obj3[[3]]
w3 <- obj3[[4]]


png("figs/e1-RT-histogram-F-tmp.png", 1024, 768)
par(mfrow = c(4, 5), mar = c(2.5, 5.3, .1, 1), mai = c(.05, 0.05, .1, 0.1), 
    oma = c(2, 2, .2, .2))
for(i in 1:nsub) 
{
  j <- subjects[i]
  if (j == 1 | j==6 | j==11 | j==16) ytick <- 's' else ytick <- 'n'
  if (j == 16 | j==17 | j==18 | j==19 | j==20) xtick <- 's' else xtick <- 'n'
  plot(x0[[j]], 
       col = rethinking::col.alpha(cbPalette[1], .30),
       main = "", 
       xlim = xlim,
       ylim = ylim,
       xlab = "", ylab = "Frequency", cex.lab = fontsize,
       cex.axis = fontsize, 
       xaxt = xtick,
       yaxt = ytick,
       border = NA)
  if (j == "5") text(1.25, 45, labels = paste0("Constant target"), cex=2.0)
  
  shade(y0[[j]], bin, cbPalette[1], median = TRUE)
  text(.20, 40, labels = paste0("S", j), cex=1.5)
  plot(x1[[j]], col = rethinking::col.alpha(cbPalette[2], .15), add = TRUE, border=NA)
  shade(y1[[j]], bin, cbPalette[2], median=TRUE)
  plot(x2[[j]], col = rethinking::col.alpha(cbPalette[3], .30), border = NA, add=TRUE)
  shade(y2[[j]], bin, cbPalette[3], median = TRUE)
  plot(x3[[j]], col = rethinking::col.alpha(cbPalette[4], .30), border = NA, add=TRUE)
  shade(y3[[j]], bin, cbPalette[4], median = TRUE)

  if (j=='20') 
  {
    legend(1, 50, legend=c("3", "5", "7", "9"),
           col=cbPalette[1:4], lty="solid", lwd=2, cex=1.5, box.lty=0)
  }
  
}
dev.off()


png("figs/e1-RT-histogram-V-tmp.png", 1024, 768)
par(mfrow = c(4, 5), mar = c(2.5, 5.3, .1, 1), mai = c(.05, 0.05, .1, 0.1), 
    oma = c(2, 2, .2, .2))
for(i in 1:nsub) 
{
  j <- subjects[i]
  if (j == 1 | j==6 | j==11 | j==16) ytick <- 's' else ytick <- 'n'
  if (j == 16 | j==17 | j==18 | j==19 | j==20) xtick <- 's' else xtick <- 'n'
  
  plot(z0[[j]], 
       col = rethinking::col.alpha(cbPalette[1], .30),
       main = "", 
       xlim = xlim,
       ylim = ylim,
       xlab = "", ylab = "Frequency", cex.lab = fontsize,
       cex.axis = fontsize, 
       xaxt = xtick,
       yaxt = ytick,
       border = NA)
  shade(w0[[j]], bin, cbPalette[1], median = TRUE)
  text(.15, 40, labels = paste0("S", j), cex=1.5)
  if (j == "5") text(1.25, 40, labels = paste0("Varying target"), cex=2)
  
  plot(z1[[j]], col = rethinking::col.alpha(cbPalette[2], .15), add = TRUE, border=NA)
  shade(w1[[j]], bin, cbPalette[2], median=TRUE)
  
  plot(z2[[j]], col = rethinking::col.alpha(cbPalette[3], .30), border = NA, add=TRUE)
  shade(w2[[j]], bin, cbPalette[3], median = TRUE)
  
  plot(z3[[j]], col = rethinking::col.alpha(cbPalette[4], .30), border = NA, add=TRUE)
  shade(w3[[j]], bin, cbPalette[4], median = TRUE)
  
  if (j=='20') 
  {
    legend(1, 50, legend=c("3", "5", "7", "9"),
           col=cbPalette[1:4], lty="solid", lwd=2, cex=1.5, box.lty=0)
  }
}
dev.off()

## Parameters ------------
res <- summary(hfit3, hyper = TRUE)
round(res$quantiles, 2)
hyper <- attr(hfit3, "hyper")
fit_loc <- hyper$phi[[1]]
pnames <- dimnames(fit_loc)[[1]]
anames <- grep('a.', pnames, value=TRUE); anames
vnames <- grep('v.', pnames, value=TRUE); vnames

scale <- .04 ## bin size
lbv <- min(fit_loc[vnames,,])-scale
ubv <- max(fit_loc[vnames,,])+scale
binv <- seq(lbv, ubv, scale)

max_countv <- 0
plot_datv <- vector("list", length=length(vnames))
for(i in 1:length(vnames)) {
  condition <- vnames[i]
  plot_datv[[i]] <- hist(fit_loc[condition,,], breaks = binv, plot = FALSE)
  tmp <- max(plot_datv[[i]]$counts)
  if(tmp > max_countv) max_countv <- tmp
}


lba <- min(fit_loc[anames,,])-scale
uba <- max(fit_loc[anames,,])+scale
bina <- seq(lba, uba, scale)

max_counta <- 0
plot_data <- vector("list", length=length(anames))
for(i in 1:length(anames)) {
  condition <- anames[i]
  plot_data[[i]] <- hist(fit_loc[condition,,], breaks = bina, plot = FALSE)
  tmp <- max(plot_data[[i]]$counts)
  if(tmp > max_counta) max_counta <- tmp
}

xlimv <- c(lbv, ubv)
ylimv <- c(0, max_countv+500)
xlima <- c(lba, uba)
ylima <- c(0, max_counta+500)

estv <- res$quantiles[paste0(vnames, ".h1"), "50%"] 
est_charv <- as.character( round(estv, 2) )
SSnames <- paste0("v", levels(e1$SS))
esta <- res$quantiles[paste0(anames, ".h1"), "50%"] 
est_chara <- as.character( round(esta, 2) )
Qnames <- paste0("a-", levels(e1$Q))

## Figure v ---------
png("figs/e1-av-posterior-dist-tmp.png", 1024, 768)
par(mfrow = c(2, 1), mar = c(2.5, 5.3, .1, 1), mai = c(.05, 0.05, .1, 0.1),
    oma = c(2, 2, .2, .2))

plot(plot_datv[[1]], 
     col = grayscale[7],
     main = "", 
     xlim = xlimv,
     ylim = ylimv,
     xlab = "", ylab = "Frequency", cex.lab = fontsize,
     cex.main = mainsize, cex.axis = fontsize)

# abline(v=est[1], lwd = 3, col = grayscale[7], lty="dashed")
text(estv[1] + 0.3, max_counta*1/2, cex = fontsize, 
     label=paste(SSnames[1], " = ", est_charv[1]))

for(i in 2:4) {
  plot(plot_datv[[i]], col = grayscale[5], add = TRUE)
  text(estv[i] + 0.3, max_counta*7/8, cex = fontsize, 
       label=paste(SSnames[i], "=", est_charv[i]))
}

## F
plot(plot_data[[1]], 
     col = grayscale[7],
     main = "", 
     xlim = xlima,
     ylim = ylima,
     xlab = "", ylab = "Frequency", cex.lab = fontsize,
     cex.main = mainsize, cex.axis = fontsize) 

text(esta[1] - 0.2, 1300, cex = fontsize, 
     label=paste(Qnames[1], "=", est_chara[1]))

## V
plot(plot_data[[2]], col = grayscale[5], add = TRUE)
text(esta[2] + 0.2, max_counta*7/8, cex = fontsize, 
     label=paste(Qnames[2], "=", est_chara[2]))

dev.off()

