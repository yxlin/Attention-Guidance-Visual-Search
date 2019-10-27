#########################################################################0
## Fixed cue vs. varied cue
## Version: 0.0.3
## Authors: Yi-Shin Lin (yishinlin001@gmail.com)
## Date: 25 Sept, 2019 -- Draft one
## License: GPL 2
## Description: 1. 
rm( list = ls() )
setwd("D:/Documents/Attention-Guidance-Visual-Search")
## setwd("/media/yslin/MERLIN/Documents/Attention-Guidance-Visual-Search/")
require(ggdmc) ## version 0.2.7.0
require(ggplot2)
load("data/visual_search.RData")

e1$R <- factor( ifelse(e1$R == "L", "left", ifelse(e1$R == "R", "right", NA)))
d1 <- data.table::data.table(e1)
dat0 <- data.frame(d1)
levels(dat0$s)

## model1; v~SS+Q ------------------
model1 <- BuildModel(
  p.map     = list(a = "1", v = c("SS", "Q"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","5","7","9"), Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar1 <- length(GetPNames(model1))
dmi1 <- BuildDMI(dat0, model1)

## We assume the following drift rates F > V and 3 > 7 > 9.
p.prior1 <- BuildPrior(
  dists = rep("tnorm", npar1),
  p1    = c(a = 2,   v.3.F=3, v.5.F=3.5, v.7.F=4, v.9.F=5, 
            v.3.V=2, v.5.V=2.5, v.7.V=3, v.9.V=4, z = .5, t0 = .2),
  p2    = rep(2, npar1),
  lower = c(0, rep(-5, 8), rep(0, 2)),
  upper = c(8, rep(10, 8),  3, 3))
mu.prior1 <- BuildPrior(
  dists = rep("tnorm", npar1),
  p1    = c(a = 2,   v.3.F=3, v.5.F=3.5, v.7.F=4, v.9.F=5, 
            v.3.V=2, v.5.V=2.5, v.7.V=3, v.9.V=4, z = .5, t0 = .2),
  p2    = rep(2, npar1),
  lower = c(0, rep(-5,8), rep(0, 2)),
  upper = c(8, rep(10,8), 3, 3))
sigma.prior1 <- BuildPrior(
  dists = rep("unif", npar1),
  p1    = rep(0, npar1),
  p2    = rep(3, npar1),
  upper = rep(NA, npar1))
names(sigma.prior1) <- GetPNames(model1)
hpriors1 <- list(pprior=p.prior1, location=mu.prior1, scale=sigma.prior1)

## 10 mins
hburnin1 <- StartNewsamples(data=dmi1, prior=hpriors1, thin=4)
save(hburnin1, dmi1, hpriors1, file = "data/modelling_data/e1hfit1.RData")
hfit1 <- run(hburnin1, thin=2)
save(hfit1, hburnin1, dmi1, hpriors1, file = "data/modelling_data/e1hfit1.RData")
rhat0 <- hgelman(hfit1)

## model2; v~SS-------------
model2 <- BuildModel(
  p.map     = list(a = "1", v = c("SS"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","5", "7","9")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar2 <- length(GetPNames(model2))
dmi2 <- BuildDMI(dat0, model2)

## We assume the following drift rates 3 > 5> 7 > 9.
p.prior2 <- BuildPrior(
  dists = rep("tnorm", npar2),
  p1    = c(a = 2,   v.3=3,  v.5=3.5,  v.7=4, v.9=5, z = .5, t0 = .2),
  p2    = rep(2, npar2),
  lower = c(0, rep(-5, 4), rep(0, 2)),
  upper = c(8, rep(10, 4),  3, 3))
mu.prior2 <- BuildPrior(
  dists = rep("tnorm", npar2),
  p1    = c(a = 2,   v.3=3,  v.5=3.5,  v.7=4, v.9=5, z = .5, t0 = .2),
  p2    = rep(2, npar2),
  lower = c(0, rep(-5,4), rep(0, 2)),
  upper = c(8, rep(10,4), 3, 3))
sigma.prior2 <- BuildPrior(
  dists = rep("unif", npar2),
  p1    = rep(0, npar2),
  p2    = rep(3, npar2),
  upper = rep(NA, npar2))
names(sigma.prior2) <- GetPNames(model2)
hpriors2 <- list(pprior=p.prior2, location=mu.prior2, scale=sigma.prior2)

## Prior plots
p1 <- plot(hpriors2[[1]], save = TRUE)
p2 <- plot(hpriors2[[2]], save = TRUE)
p3 <- plot(hpriors2[[3]], save = TRUE)
p1$prior <- "I"
p2$prior <- "PL"
p3$prior <- "PS"
DT <- rbind(p1, p2, p3)
DT$gp <- paste(DT$prior, DT$Parameter, sep = ".")

p0 <- ggplot(DT, aes_string(x = "xpos", y = "ypos")) +
  geom_line() +
  xlab("")+ ylab("")+
  facet_wrap(~gp, scales="free") +
  theme_bw() +
  theme(legend.position = "none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_blank())

png("figs/priors-tmp.png", 1024, 768)
print(p0)
dev.off()

hburnin2 <- StartNewsamples(data=dmi2, prior=hpriors2, thin=2)
save(hburnin2, dmi2, hpriors2, file = "data/modelling_data/e1hfit2.RData")
hfit2 <- run(hburnin2, thin=4)
save(hfit2, hburnin2, dmi2, hpriors2, file = "data/modelling_data/e1hfit2.RData")
res <- hgelman(hfit2)

## model3; a-Q & v~SS ------------
model3 <- BuildModel(
  p.map     = list(a = "Q", v = c("SS"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","5", "7","9"), Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar3 <- length(GetPNames(model3))
dmi3 <- BuildDMI(dat0, model3)

p.prior3 <- BuildPrior(
  dists = rep("tnorm", npar3),
  p1    = c(a.F = 2, a.V = 2.5, v.3=3, v.5=3.5, v.7=4, v.9=5, z = .5, t0 = .2),
  p2    = rep(2, npar3),
  lower = c(0, 0, rep(-5, 4), rep(0, 2)),
  upper = c(8, 8, rep(10, 4),  3, 3))
mu.prior3 <- BuildPrior(
  dists = rep("tnorm", npar3),
  p1    = c(a.F = 2, a.V = 2.5, v.3=3, v.5=3.5, v.7=4, v.9=5, z = .5, t0 = .2),
  p2    = rep(2, npar3),
  lower = c(0, 0, rep(-5,4), rep(0, 2)),
  upper = c(8, 8, rep(10,4), 3, 3))
sigma.prior3 <- BuildPrior(
  dists = rep("unif", npar3),
  p1    = rep(0, npar3),
  p2    = rep(3, npar3),
  upper = rep(NA, npar3))
names(sigma.prior3) <- GetPNames(model3)
hpriors3 <- list(pprior=p.prior3, location=mu.prior3, scale=sigma.prior3)

## 425.48 s
hburnin3 <- StartNewsamples(data=dmi3, prior=hpriors3, thin=4)
save(hburnin3, dmi3, hpriors3, file = "data/modelling_data/e1hfit3.RData")

hfit3 <- run(hburnin3, thin=8)
save(hfit3, hburnin3, dmi3, hpriors3, file = "data/modelling_data/e1hfit3.RData")

# rhat0 <- hgelman(hfit3)
# plot(hfit3)
# plot(hfit3, hyper = TRUE)
# rhat0 <- hgelman(hburnin3, start = 150)

## model4; a~Q & v~SS+Q ------------------
model4 <- BuildModel(
  p.map     = list(a = "Q", v = c("SS", "Q"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","5","7","9"), Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar4 <- length(GetPNames(model4))
dmi4 <- BuildDMI(dat0, model4)

p.prior4 <- BuildPrior(
  dists = rep("tnorm", npar4),
  p1    = c(a.F = 2, a.V=2.5,  v.3.F=3, v.5.F=3.5, v.7.F=4, v.9.F=5, 
            v.3.V=2, v.5.V=2.5, v.7.V=3, v.9.V=4, z = .5, t0 = .2),
  p2    = rep(2, npar4),
  lower = c(0, 0, rep(-5, 8), rep(0, 2)),
  upper = c(8, 8, rep(10, 8),  3, 3))
mu.prior4 <- BuildPrior(
  dists = rep("tnorm", npar4),
  p1    = c(a.F = 2, a.V=2.5,  v.3.F=3, v.5.F=3.5, v.7.F=4, v.9.F=5, 
            v.3.V=2, v.5.V=2.5, v.7.V=3, v.9.V=4, z = .5, t0 = .2),
  p2    = rep(2, npar4),
  lower = c(0, 0, rep(-5,8), rep(0, 2)),
  upper = c(8, 8, rep(10,8), 3, 3))
sigma.prior4 <- BuildPrior(
  dists = rep("unif", npar4),
  p1    = rep(0, npar4),
  p2    = rep(3, npar4),
  upper = rep(NA, npar4))

names(sigma.prior4) <- GetPNames(model4)
hpriors4 <- list(pprior=p.prior4, location=mu.prior4, scale=sigma.prior4)

## 10 mins
hburnin4 <- StartNewsamples(data=dmi4, prior=hpriors4, thin=4)
hfit4 <- run(hburnin4, thin=4)
save(hfit4, hburnin4, dmi4, hpriors4, file = "data/modelling_data/e1hfit4.RData")
rhat0 <- hgelman(hfit4)


## Model comparison ------------------------
## No model 0, because no ISI factor
load("data/modelling_data/e1hfit1.RData") ## model1        v~SS+Q (done)
load("data/modelling_data/e1hfit2.RData") ## model2        v~SS  (done)
load("data/modelling_data/e1hfit3.RData") ## model3  a~Q & v~SS (done)
load("data/modelling_data/e1hfit4.RData") ## model4  a~Q & v~SS+Q (done)
source("R/functions/predict_one.R")
hgelman(hfit1);
hgelman(hfit2);
hgelman(hfit3);
hgelman(hfit4);

res1 <- summary(hfit1, hyper = TRUE)
round(res1$quantiles, 2)

res2 <- summary(hfit2, hyper = TRUE)
round(res2$quantiles, 2)

res3 <- summary(hfit3, hyper = TRUE)
round(res3$quantiles, 2)

res4 <- summary(hfit4, hyper = TRUE)
round(res4$quantiles, 2)

plot(hfit1, hyper=TRUE, pll=F, den=T)
plot(hfit2, hyper=TRUE, pll=F, den=T)
plot(hfit3, hyper=TRUE, pll=F, den=T)
plot(hfit4, hyper=TRUE, pll=F, den=T)

## No model0
dev1 <- DIC(hfit1, BPIC=TRUE)
dev2 <- DIC(hfit2, BPIC=TRUE)
dev3 <- DIC(hfit3, BPIC=TRUE)
dev4 <- DIC(hfit4, BPIC=TRUE)
ics <- c(sum(dev1), sum(dev2), sum(dev3), sum(dev4))
res0 <- ggdmc:::weightIC(ics)
## Summed BPIC: -27008.99  ##       v~SS+Q model1
## Summed BPIC: -26544.42  ##       v~SS   model2
## Summed BPIC: -27095.49  ## a~Q & v~SS   model3
## Summed BPIC: -27099.7   ## a~Q & v~SS+Q model4
##           M1     M2   M3   M4
## IC-min 90.71 555.29 4.21 0.00
## w       0.00   0.00 0.11 0.89

word_lab <- c('M2', 'M0', 'M1', 'M3')
eqn <- c('v~SS+Q', 'v~SS', 'a~Q & v~SS', 'a~Q & v~SS+Q')
res1 <- round(ics - sum(dev2), 2)
dat <- data.frame(M=word_lab, IC=ics, rIC=res1, E=eqn)
dat[order(dat[,1]),]

##    M        IC     rIC            E
## 2 M0 -26544.42    0.00         v~SS
## 3 M1 -27095.49 -551.08   a~Q & v~SS
## 1 M2 -27008.99 -464.58       v~SS+Q
## 4 M3 -27099.70 -555.29 a~Q & v~SS+Q

