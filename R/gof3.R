#########################################################################0
## Fixed cue vs. varied cue
## Version: 0.0.3
## Authors: Yi-Shin Lin (yishinlin001@gmail.com)
## Date: 25 Sept, 2019 -- Draft one
## License: GPL 2
## Description: 1. 
rm( list = ls() )
setwd("/media/yslin/MERLIN/Documents/Attention-Guidance-Visual-Search/")
require(ggdmc) ## version 0.2.7.0
require(ggplot2)
load("data/visual_search.RData")
load("data/modelling_data/e3hfit7.RData")
load("data/modelling_data/e3hfit8.RData")
load("data/E3-pp.RData")
source("R/functions/predict_one.R")
hgelman(hfit7); hgelman(hfit8); 

## Prepare data ------------
e3$R <- factor( ifelse(e3$R == "L", "left", ifelse(e3$R == "R", "right", NA)))
d3 <- data.table::data.table(e3)
nsub <- length(table(d3$s))
levels(d3$s)
d3$s <- factor( 
        ifelse(d3$s =='E3-24', '24',
        ifelse(d3$s =='E3-26', '26',
        ifelse(d3$s =='E3-27', '27',
        ifelse(d3$s =='E3-30', '30',
        ifelse(d3$s =='E3-31', '31',
        ifelse(d3$s =='E3-32', '32',
        ifelse(d3$s =='E3-33', '33',
        ifelse(d3$s =='E3-34', '34',
        ifelse(d3$s =='E3-36', '36',
        ifelse(d3$s =='E3-37', '37',
        ifelse(d3$s =='E3-38', '38',
        ifelse(d3$s =='E3-39', '39',
        ifelse(d3$s =='E3-40', '40',
        ifelse(d3$s =='E3-41', '41',
        ifelse(d3$s =='E3-42', '42',
        ifelse(d3$s =='E3-43', '43',
        ifelse(d3$s =='E3-45', '45',
        ifelse(d3$s =='E3-46', '46',
        ifelse(d3$s =='E3-47', '47', 
        ifelse(d3$s =='E3-49', '49', NA)))))))))))))))))))))

subjects0 <- c("24", "26", "27","30","31","32","33","34","36","37",
               "38", "39", "40", "41", "42","43", "45","46","47","49")
names(hfit7) <- subjects0 ## a~I+Q & v~SS
names(hfit8) <- subjects0 ## a~I+Q & v~SS+Q

subjects <- c("24", "26","27","30","31","32","33","34","36","37","38","39","40",
              "41","42","43","45", "46", "47","49")

dev7 <- DIC(hfit7, BPIC=TRUE)
dev8 <- DIC(hfit8, BPIC=TRUE)
pp7 <- pp8 <- vector('list', nsub)
names(pp7) <- subjects0
names(pp8) <- subjects0
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

save(pp7, pp8, file = "data/E3-pp.RData")

## Goodness-of-fit ----
d3True <- data.table::data.table( d3[d3$C==TRUE, c('s', 'SS', 'Q', 'S', 'C', 'RT')] )
dplyr::tbl_df(d3True)

tmp <- matrix(numeric(nsub*2), ncol=2)
for(i in 1:nsub) {
  j <- subjects[i]
  tmp[i,1] <- min( pp8[[ j ]]$RT )
  tmp[i,2] <- max( pp8[[ j ]]$RT )
}

scale <- .06 ## bin size
lb <- min(d3True$RT, tmp)-scale
ub <- max(d3True$RT, tmp)+scale
bin <- seq(lb, ub, scale)

plot_3F_dat <- plot_7F_dat <- plot_9F_dat <- vector("list", length=nsub)
plot_3V_dat <- plot_7V_dat <- plot_9V_dat <- vector("list", length=nsub)
sim_3F_dat  <- sim_7F_dat  <- sim_9F_dat  <- vector("list", length=nsub)
sim_3V_dat  <- sim_7V_dat  <- sim_9V_dat  <- vector("list", length=nsub)
## Summed BPIC: -49480.46 ## a~I+Q & v~SS+Q model8 

for(i in 1:nsub) {
  plot_3F_dat[[i]] <- hist(d3True[s==subjects[i] & SS == '3' & Q == 'F']$RT, 
                           breaks = bin, plot = FALSE)
  plot_7F_dat[[i]] <- hist(d3True[s==subjects[i] & SS == '7' & Q == 'F']$RT, 
                           breaks = bin, plot = FALSE)
  plot_9F_dat[[i]] <- hist(d3True[s==subjects[i] & SS == '9' & Q == 'F']$RT, 
                           breaks = bin, plot = FALSE)
  
  plot_3V_dat[[i]] <- hist(d3True[s==subjects[i] & SS == '3' & Q == 'V']$RT, 
                           breaks = bin, plot = FALSE)
  plot_7V_dat[[i]] <- hist(d3True[s==subjects[i] & SS == '7' & Q == 'V']$RT, 
                           breaks = bin, plot = FALSE)
  plot_9V_dat[[i]] <- hist(d3True[s==subjects[i] & SS == '9' & Q == 'V']$RT, 
                           breaks = bin, plot = FALSE)
  
  sim_3F_dat[[i]] <- pp8[[subjects[i]]][SS == '3' & Q == 'F']
  sim_7F_dat[[i]] <- pp8[[subjects[i]]][SS == '7' & Q == 'F']
  sim_9F_dat[[i]] <- pp8[[subjects[i]]][SS == '9' & Q == 'F']
  sim_3V_dat[[i]] <- pp8[[subjects[i]]][SS == '3' & Q == 'V']
  sim_7V_dat[[i]] <- pp8[[subjects[i]]][SS == '7' & Q == 'V']
  sim_9V_dat[[i]] <- pp8[[subjects[i]]][SS == '9' & Q == 'V']
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

## Figures ---------
png("figs/e3-RT-histogram-F-tmp.png", 1024, 768)
par(mfrow = c(4, 5), mar = margin, mai = c(.05, 0.05, .1, 0.1), 
    oma = c(2, 2, .2, .2))
for(i in 1:nsub) 
{
  j <- subjects[i]
  if (j =="24" | j=="32" | j=="38" | j=="43") ytick <- 's' else ytick <- 'n'
  if (j =="43" | j=="45" | j=="46" | j=="47" | j=="49") xtick <- 's' else xtick <- 'n'
  plot(x0[[j]], 
       col = rethinking::col.alpha(cbPalette[1], .30),
       # col = grayscale[7],
       main = "", 
       xlim = xlim,
       ylim = ylim,
       xlab = "", ylab = "Frequency", cex.lab = fontsize,
       cex.axis = fontsize, 
       xaxt = xtick,
       yaxt = ytick,
       border = NA)
  if (j == "31") text(1.25, 100, labels = paste0("Constant target"), cex=2.0)
  
  shade(y0[[j]], bin, cbPalette[1], median = TRUE)
  text(.20, 40, labels = paste0("S", j), cex=1.5)
  
  plot(x1[[j]], col = rethinking::col.alpha(cbPalette[3], .15), add = TRUE, border=NA)
  
  shade(y1[[j]], bin, cbPalette[3], median=TRUE)
  
  plot(x2[[j]], col = rethinking::col.alpha(cbPalette[4], .30), border = NA, add=TRUE)
  shade(y2[[j]], bin, cbPalette[4], median = TRUE)
  
  if (j=='49') 
  {
    legend(1, 100, legend=c("3", "7", "9"),
           col=cbPalette[c(1,3,4)], lty="solid", lwd=2, cex=1.5, box.lty=0)
  }
  
}
dev.off()


png("figs/e3-RT-histogram-V-tmp.png", 1024, 768)
par(mfrow = c(4, 5), mar = margin, mai = c(.05, 0.05, .1, 0.1), 
    oma = c(2, 2, .2, .2))
for(i in 1:nsub) 
{
  j <- subjects[i]
  if (j =="24" | j=="32" | j=="38" | j=="43") ytick <- 's' else ytick <- 'n'
  if (j =="43" | j=="45" | j=="46" | j=="47" | j=="49") xtick <- 's' else xtick <- 'n'
  
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
  if (j == "31") text(1.25, 100, labels = paste0("Varying target"), cex=2)
  plot(z1[[j]], col = rethinking::col.alpha(cbPalette[3], .15), add = TRUE, border=NA)
  shade(w1[[j]], bin, cbPalette[3], median=TRUE)
  plot(z2[[j]], col = rethinking::col.alpha(cbPalette[4], .30), border = NA, add=TRUE)
  shade(w2[[j]], bin, cbPalette[4], median = TRUE)
  if (j=='49') 
  {
    legend(1, 100, legend=c("3", "7", "9"),
           col=cbPalette[c(1,3,4)], lty="solid", lwd=2, cex=1.5, box.lty=0)
  }
}
dev.off()

## Parameters and data fit ------------
res <- summary(hfit8, hyper = TRUE)  
round( res$quantiles, 2)
hyper <- attr(hfit8, "hyper")
fit_loc <- hyper$phi[[1]]

## figures -------
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
bina <- seq(lba, uba, scale)

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
xlima <- c(lba, uba)
ylima <- c(0, max_counta+500)


estv <- res$quantiles[paste0(vnames, ".h1"), "50%"] 
est_charv <- as.character( round(estv, 2) )
SSnames <- paste0("v", levels(e3$SS))
esta <- res$quantiles[paste0(anames, ".h1"), "50%"] 
est_chara <- as.character( round(esta, 2) )
Qnames <- paste0("a-", levels(e3$I)); Qnames



## a~I+Q & v~SS+Q
png("figs/e3-av-posterior-dist-tmp.png", 1024, 768)
par(mfrow = c(2, 2), mar = margin, mai = c(.05, 0.5, .1, 0.1), oma = c(2, 1, .2, .2))

plot(plot_datv[[1]], 
     col = grayscale[7],
     main = "", 
     xlim = xlimv,
     ylim = ylimv,
     xlab = "", ylab = "", cex.lab = fontsize,
     cex.main = mainsize, cex.axis = fontsize) 
text(estv[1] + 0.5, 1450, cex = fontsize, label=paste(SSnames[1], "=", est_charv[1]))
plot(plot_datv[[2]], col = grayscale[5], add = TRUE)
plot(plot_datv[[3]], col = grayscale[3], add = TRUE) 
text(estv[2] + 0.65, max_countv*1/2, cex = fontsize, 
     label=paste(SSnames[2], "=", est_charv[2]))
text(estv[3] + 0.3, max_countv*4/5, cex = fontsize, 
     label=paste(SSnames[3], "=", est_charv[3]))
text(5.5, 3000, labels = paste0("Constant target"), cex=2.0)

## Boundary separation
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
text(2.2, 3500, labels = paste0("Constant target"), cex=2.0)

## Rates
plot(plot_datv[[4]], 
     col = grayscale[7],
     main = "", 
     xlim = xlimv,
     ylim = ylimv,
     xlab = "", ylab = "", cex.lab = fontsize,
     cex.main = mainsize, cex.axis = fontsize) 
text(estv[4] + 0.3, 1800, cex = fontsize, label=paste(SSnames[1], "=", est_charv[4]))
plot(plot_datv[[5]], col = grayscale[5], add = TRUE) 
plot(plot_datv[[6]], col = grayscale[3], add = TRUE) 
text(estv[5] + 0.65, max_countv*2/3, cex = fontsize, 
     label=paste(SSnames[2], "=", est_charv[5]))
text(estv[6] - 0.35, max_countv*4/5, cex = fontsize, 
     label=paste(SSnames[3], "=", est_charv[6]))
text(5.5, 3000, labels = paste0("Varying target"), cex=2.0)

## Boundary separation
plot(plot_data[[3]], 
     col = grayscale[7],
     main = "", 
     xlim = xlima,
     ylim = ylima,
     xlab = "", ylab = "", cex.lab = fontsize,
     cex.main = mainsize, cex.axis = fontsize) 
text(esta[3] + 0.2, 2000, cex = fontsize, 
     label=paste(Qnames[1], "=", est_chara[3]))

plot(plot_data[[4]], col = grayscale[5], add = TRUE)
text(esta[4] - 0.25, 2000, cex = fontsize, 
     label=paste(Qnames[2], "=", est_chara[4]))
text(2.2, 3500, labels = paste0("Varying target"), cex=2.0)

dev.off()

