#########################################################################0
## Fixed cue vs. varied cue
## Version: 0.0.3
## Authors: Yi-Shin Lin (yishinlin001@gmail.com)
## Date: 25 Sept, 2019 -- Draft one
## License: GPL 2
## Description: 1. 
rm( list = ls() )
# setwd("/media/yslin/MERLIN/Documents/Attention-Guidance-Visual-Search/")
setwd("D:/Documents/Attention-Guidance-Visual-Search")

require(ggdmc) ## version 0.2.7.0
require(ggplot2)
load("data/visual_search.RData")

## Summed BPIC: -15746.85 ## a~I+Q & v~SS      model7 M8 
## Summed BPIC: -15745.77 ## a~I+Q & v~SS+Q    model8 M9
load("data/modelling_data/e2hfit7.RData")
load("data/modelling_data/e2hfit8.RData") 
load("data/E2-pp.RData")


source("R/functions/predict_one.R")
hgelman(hfit7);
hgelman(hfit8);
## Prepare data ------------
e2$R <- factor( ifelse(e2$R == "L", "left", ifelse(e2$R == "R", "right", NA)))
d2 <- data.table::data.table(e2)
nsub <- length(table(d2$s))
levels(d2$s)
d2$s <- factor( 
        ifelse(d2$s =='E2-10', '10',
        ifelse(d2$s =='E2-11', '11',
        ifelse(d2$s =='E2-12', '12',
        ifelse(d2$s =='E2-13', '13',
        ifelse(d2$s =='E2-14', '14',
        ifelse(d2$s =='E2-15', '15',
        ifelse(d2$s =='E2-16', '16',
        ifelse(d2$s =='E2-17', '17',
        ifelse(d2$s =='E2-18', '18',
        ifelse(d2$s =='E2-19', '19',
        ifelse(d2$s =='E2-20', '20',
        ifelse(d2$s =='E2-21', '21',
        ifelse(d2$s =='E2-23', '23',
        ifelse(d2$s =='E2-3', '3',
        ifelse(d2$s =='E2-4', '4',
        ifelse(d2$s =='E2-5', '5',
        ifelse(d2$s =='E2-7', '7',
        ifelse(d2$s =='E2-8', '8',
        ifelse(d2$s =='E2-9', '9', NA)))))))))))))))))))) 

subjects0 <- c("10", "11", "12","13","14","15","16","17","18","19",
               "20", "21", "23", "3", "4","5", "7","8","9")
names(hfit7) <- subjects0
names(hfit8) <- subjects0
subjects <- c("3", "4","5","7","8","9","10","11","12","13","14","15","16","17",
              "18","19","20", "21", "23")

dev7 <- DIC(hfit7, BPIC=TRUE)
dev8 <- DIC(hfit8, BPIC=TRUE)
pp7 <- pp8 <- vector('list', nsub)
names(pp7) <- names(pp8) <- subjects
for(i in 1:nsub) {
  j <- subjects[i]
  pp7[[j]] <- predict_one(hfit7[[j]], xlim = c(0, 5))
  pp8[[j]] <- predict_one(hfit8[[j]], xlim = c(0, 5))
  pp7[[j]]$C <- ifelse(pp7[[j]]$S == "L" & pp7[[j]]$R == "left",  TRUE,
                ifelse(pp7[[j]]$S == "R" & pp7[[j]]$R == "right", TRUE,
                ifelse(pp7[[j]]$S == "L" & pp7[[j]]$R == "right", FALSE,
                ifelse(pp7[[j]]$S == "R" & pp7[[j]]$R == "left",  FALSE, NA))))
  pp8[[j]]$C <- ifelse(pp8[[j]]$S == "L" & pp8[[j]]$R == "left",  TRUE,
                ifelse(pp8[[j]]$S == "R" & pp8[[j]]$R == "right", TRUE,
                ifelse(pp8[[j]]$S == "L" & pp8[[j]]$R == "right", FALSE,
                ifelse(pp8[[j]]$S == "R" & pp8[[j]]$R == "left",  FALSE, NA))))
}

save(pp7,pp8, file = "data/E2-pp.RData")

## Goodness-of-fit ----
d2True <- data.table::data.table( d2[d2$C==TRUE, c('s', 'SS', 'Q', 'S', 'C', 'RT')] )
dplyr::tbl_df(d2True)

tmp <- matrix(numeric(nsub*2), ncol=2)
for(i in 1:nsub) {
  j <- subjects[i]
  tmp[i,1] <- min( pp7[[ j ]]$RT )
  tmp[i,2] <- max( pp7[[ j ]]$RT )
}

scale <- .06 ## bin size
lb <- min(d2True$RT, tmp)-scale
ub <- max(d2True$RT, tmp)+scale
bin <- seq(lb, ub, scale)

plot_3F_dat <- plot_7F_dat <- plot_9F_dat <- vector("list", length=nsub)
plot_3V_dat <- plot_7V_dat <- plot_9V_dat <- vector("list", length=nsub)
sim_3F_dat  <- sim_7F_dat  <- sim_9F_dat  <- vector("list", length=nsub)
sim_3V_dat  <- sim_7V_dat  <- sim_9V_dat  <- vector("list", length=nsub)

## a~I+Q & v~SS      model7 
for(i in 1:nsub) {
  plot_3F_dat[[i]] <- hist(d2True[s==subjects[i] & SS == '3' & Q == 'F']$RT, 
                           breaks = bin, plot = FALSE)
  plot_7F_dat[[i]] <- hist(d2True[s==subjects[i] & SS == '7' & Q == 'F']$RT, 
                           breaks = bin, plot = FALSE)
  plot_9F_dat[[i]] <- hist(d2True[s==subjects[i] & SS == '9' & Q == 'F']$RT, 
                           breaks = bin, plot = FALSE)
  
  plot_3V_dat[[i]] <- hist(d2True[s==subjects[i] & SS == '3' & Q == 'V']$RT, 
                           breaks = bin, plot = FALSE)
  plot_7V_dat[[i]] <- hist(d2True[s==subjects[i] & SS == '7' & Q == 'V']$RT, 
                           breaks = bin, plot = FALSE)
  plot_9V_dat[[i]] <- hist(d2True[s==subjects[i] & SS == '9' & Q == 'V']$RT, 
                           breaks = bin, plot = FALSE)
  
  sim_3F_dat[[i]] <- pp7[[subjects[i]]][SS == '3' & Q == 'F']
  sim_7F_dat[[i]] <- pp7[[subjects[i]]][SS == '7' & Q == 'F']
  sim_9F_dat[[i]] <- pp7[[subjects[i]]][SS == '9' & Q == 'F']
  
  sim_3V_dat[[i]] <- pp7[[subjects[i]]][SS == '3' & Q == 'V']
  sim_7V_dat[[i]] <- pp7[[subjects[i]]][SS == '7' & Q == 'V']
  sim_9V_dat[[i]] <- pp7[[subjects[i]]][SS == '9' & Q == 'V']
}


names(plot_3F_dat) <- subjects
names(plot_7F_dat) <- subjects
names(plot_9F_dat) <- subjects
names(plot_3V_dat) <- subjects
names(plot_7V_dat) <- subjects
names(plot_9V_dat) <- subjects
names(sim_3F_dat) <- subjects
names(sim_7F_dat) <- subjects
names(sim_9F_dat) <- subjects
names(sim_3V_dat) <- subjects
names(sim_7V_dat) <- subjects
names(sim_9V_dat) <- subjects

max_count <- 0
tmp <- numeric(6)
for(i in 1:nsub) {
  tmp[1] <- max( plot_3F_dat[[i]]$counts )
  tmp[2] <- max( plot_7F_dat[[i]]$counts )
  tmp[3] <- max( plot_9F_dat[[i]]$counts )
  tmp[4] <- max( plot_3V_dat[[i]]$counts )
  tmp[5] <- max( plot_7V_dat[[i]]$counts )
  tmp[6] <- max( plot_9V_dat[[i]]$counts )
  tmp0 <- max(tmp)
  if(tmp0 > max_count) max_count <- tmp0
}

max_count

margin <- c(2.5, 5.3, .1, 1)
fontsize <- 1.5
mainsize <- 2
# xlim <- c(lb, ub)
xlim <- c(0, 2)
ylim <- c(0, max_count)
grayscale <- gray(0:8 / 8, alpha=1/2)
fontsize <- 1.5

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")
obj0 <- list(plot_3F_dat,plot_7F_dat, plot_9F_dat)
obj1 <- list(sim_3F_dat, sim_7F_dat, sim_9F_dat)
obj2 <- list(plot_3V_dat, plot_7V_dat, plot_9V_dat)
obj3 <- list(sim_3V_dat, sim_7V_dat, sim_9V_dat)
names(obj0) <- c('3', '7', '9')
names(obj1) <- c('3', '7', '9')
names(obj2) <- c('3', '7', '9')
names(obj3) <- c('3', '7', '9')

x0 <- obj0[[1]]
x1 <- obj0[[2]]
x2 <- obj0[[3]]
y0 <- obj1[[1]]
y1 <- obj1[[2]]
y2 <- obj1[[3]]
z0 <- obj2[[1]]
z1 <- obj2[[2]]
z2 <- obj2[[3]]
w0 <- obj3[[1]]
w1 <- obj3[[2]]
w2 <- obj3[[3]]


png("figs/e2-RT-histogram-F-tmp.png", 1024, 768)
par(mfrow = c(4, 5), mar = margin, mai = c(.05, 0.05, .1, 0.1), 
    oma = c(2, 2, .2, .2))
for(i in 1:nsub) 
{
  j <- subjects[i]
  if (j =="3" | j=="9" | j=="14" | j=="19") ytick <- 's' else ytick <- 'n'
  if (j =="19" | j=="20" | j=="21" | j=="23") xtick <- 's' else xtick <- 'n'
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
  if (j == "8") text(1.25, 40, labels = paste0("Constant target"), cex=2.0)
  # idx <- which.max(plot_3F_dat[[j]]$density)
  # shade(sim_3F_dat[[j]], bin, cbPalette[1])
  shade(y0[[j]], bin, cbPalette[1], median = TRUE)
  text(.20, 40, labels = paste0("S", j), cex=1.5)
  plot(x1[[j]], col = rethinking::col.alpha(cbPalette[3], .15), add = TRUE, border=NA)
  shade(y1[[j]], bin, cbPalette[3], median=TRUE)
  plot(x2[[j]], col = rethinking::col.alpha(cbPalette[4], .30), border = NA, add=TRUE)
  shade(y2[[j]], bin, cbPalette[4], median = TRUE)
  # text(plot_7F_dat[[j]]$mids[idx+1], 8, labels = paste0(j, ", ", "7-F"))
  if (j=='23') 
  {
    legend(1.5, 50, legend=c("3", "7", "9"),
           col=cbPalette[c(1, 3, 4)], lty="dotted", lwd=2, cex=1.5, box.lty=0)
  }
}
dev.off()


png("figs/e2-RT-histogram-V-tmp.png", 1024, 768)
par(mfrow = c(4, 5), mar = margin, mai = c(.05, 0.05, .1, 0.1), 
    oma = c(2, 2, .2, .2))
for(i in 1:nsub) 
{
  j <- subjects[i]
  if (j =="3" | j=="9" | j=="14" | j=="19") ytick <- 's' else ytick <- 'n'
  if (j =="19" | j=="20" | j=="21" | j=="23") xtick <- 's' else xtick <- 'n'
  
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
  if (j == "8") text(1.25, 40, labels = paste0("Varying target"), cex=2)
  
  plot(z1[[j]], col = rethinking::col.alpha(cbPalette[3], .15), add = TRUE, border=NA)
  shade(w1[[j]], bin, cbPalette[3], median=TRUE)
  
  plot(z2[[j]], col = rethinking::col.alpha(cbPalette[4], .30), border = NA, add=TRUE)
  shade(w2[[j]], bin, cbPalette[4], median = TRUE)
  
  if (j=='23') 
  {
    legend(1.5, 50, legend=c("3", "7", "9"),
           col=cbPalette[c(1,3,4)], lty="dotted", lwd=2, cex=1.5, box.lty=0)
  }
}
dev.off()

## Parameters and data fit ------------
## a~I+Q & v~SS      model7 M8
res <- summary(hfit7, hyper = TRUE)  
plot(hfit7, hyper=T, pll=F, den=T)
hyper <- attr(hfit7, "hyper")
fit_loc <- hyper$phi[[1]]
round(res$quantiles, 2)


## v figure -------
pnames <- dimnames(fit_loc)[[1]]; pnames
anames <- grep('a.', pnames, value=T); anames
vnames <- grep('v.', pnames, value=T); vnames

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
bina <- seq(lba, uba, scale); 

max_counta <- 0
plot_data <- vector("list", length=length(anames))
for(i in 1:length(anames)) {
  condition <- anames[i]
  plot_data[[i]] <- hist(fit_loc[condition,,], breaks = bina, plot = FALSE)
  
  tmp <- max(plot_data[[i]]$counts)
  if(tmp > max_counta) max_counta <- tmp
}

margin <- c(2.5, 5.3, .1, 1)
fontsize <- 1.5
mainsize <- 2
grayscale <- gray(0:8 / 8, alpha=1/2)
xlimv <- c(lbv, ubv)
ylimv <- c(0, max_countv+500)
xlima <- c(lba, uba); xlima
ylima <- c(0, max_counta+500)

estv <- res$quantiles[paste0(vnames, ".h1"), "50%"] 
est_charv <- as.character( round(estv, 2) )
SSnames <- paste0("v", levels(e2$SS))
esta <- res$quantiles[paste0(anames, ".h1"), "50%"]; esta
est_chara <- as.character( round(esta, 2) ); est_chara
Qnames <- paste0("a-", levels(e2$I)); Qnames

## a~I+Q & v~SS      model7 M8

png("figs/e2-av-posterior-dist-tmp.png", 1024, 768)
par(mfrow = c(3, 1), mar = margin, mai = c(.05, 0.5, .1, 0.1), oma = c(2, 1, .2, .2))

plot(plot_datv[[1]], 
     col = grayscale[7],
     main = "", 
     xlim = xlimv,
     ylim = ylimv,
     xlab = "", ylab = "", cex.lab = fontsize,
     cex.main = mainsize, cex.axis = fontsize) 
text(estv[1] + 0.3, 1700, cex = fontsize, label=paste(SSnames[1], "=", est_charv[1]))

for(i in 2:3) {
  plot(plot_datv[[i]], col = grayscale[5], add = TRUE)
  text(estv[i] + 0.3, max_countv*2/3, cex = fontsize, 
       label=paste(SSnames[i], "=", est_charv[i]))
}

plot(plot_data[[1]], 
     col = grayscale[7],
     main = "", 
     xlim = xlima,
     ylim = ylima,
     xlab = "", ylab = "", cex.lab = fontsize,
     cex.main = mainsize, cex.axis = fontsize) 
text(esta[1] + 0.3, 1300, cex = fontsize, 
     label=paste(Qnames[1], "=", est_chara[1]))

plot(plot_data[[2]], col = grayscale[5], add = TRUE)
text(esta[2] - 0.3, 1300, cex = fontsize, 
     label=paste(Qnames[2], "=", est_chara[2]))
text(2.7, 2000, labels = paste0("Constant target"), cex=2.0)

plot(plot_data[[3]], 
     col = grayscale[7],
     main = "", 
     xlim = xlima,
     ylim = ylima,
     xlab = "", ylab = "", cex.lab = fontsize,
     cex.main = mainsize, cex.axis = fontsize) 
text(esta[3] + 0.3, 1300, cex = fontsize, 
     label=paste(Qnames[1], "=", est_chara[3]))

plot(plot_data[[4]], col = grayscale[5], add = TRUE)
text(esta[4] - 0.3, 1300, cex = fontsize, 
     label=paste(Qnames[2], "=", est_chara[4]))
text(2.7, 2000, labels = paste0("Varying target"), cex=2.0)
dev.off()

