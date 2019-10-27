#########################################################################0
## Fixed cue vs. varied cue
## Version: 0.0.3
## Authors: Yi-Shin Lin (yishinlin001@gmail.com)
## Date: 25 Sept, 2019 -- Draft one
## License: GPL 2
## Description: 1. 
rm( list = ls() )
## setwd("D:/Documents/Attention-Guidance-Visual-Search")
## setwd("~/Documents/Attention-Guidance-Visual-Search/")
setwd("/media/yslin/MERLIN/Documents/Attention-Guidance-Visual-Search/")
require(ggdmc) ## version 0.2.6.8
load("data/visual_search.RData")

dplyr::tbl_df(e2)
e2$R <- factor( ifelse(e2$R == "L", "left", ifelse(e2$R == "R", "right", NA)))
d2 <- data.table::data.table(e2)
sapply(d2, levels)
d2[, .N, .(SS,Q,I,s)]
length(table(d2$s))

## model0; v ~ SS + I + Q -------------------
model0 <- BuildModel(
  p.map     = list(a = "1", v = c("SS", "I", "Q"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), I = c("50", "400"),
                   Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar0 <- length(GetPNames(model0))

## We assume the following drift rates 400.F > 50.F > 400.V > 50.V, and
## SS3 > SS7 > SS9
dat0 <- data.frame(d2)
dmi0 <- BuildDMI(dat0, model0)
p.prior0 <- BuildPrior(
  dists = rep("tnorm", npar0),
  p1    = c(2, 4.01, 3.53, 2.93, 4.92, 4.50, 3.98, 2.08, 1.48, 1.03, 2.98,
            2.49, 2.04, .5, .19),
  p2    = rep(2, npar0),
  lower = c(0, rep(-5, 12), rep(0, 2)),
  upper = c(8, rep(10, 12),  3, 3))
mu.prior0 <- BuildPrior(
  dists = rep("tnorm", npar0),
  p1    = c(2, 4.01, 3.53, 2.93, 4.92, 4.50, 3.98, 2.08, 1.48, 1.03, 2.98,
            2.49, 2.04, .5, .19),
  p2    = rep(2, npar0),
  lower = c(0, rep(-5,12), rep(0, 2)),
  upper = c(8, rep(10,12), 3, 3))
sigma.prior0 <- BuildPrior(
  dists = rep("unif", npar0),
  p1    = rep(0, npar0),
  p2    = rep(5, npar0),
  upper = rep(NA, npar0))

names(p.prior0) <- GetPNames(model0)
names(mu.prior0) <- GetPNames(model0)
names(sigma.prior0) <- GetPNames(model0)
hpriors0 <- list(pprior=p.prior0, location=mu.prior0, scale=sigma.prior0)

# hburnin0 <- StartNewsamples(data=dmi0, prior=hpriors0)
# hfit0 <- run(hburnin0)
# hfit0 <- run(hfit0, thin=2)
# Start sampling: 100 200 300 400 500 
# save(hfit0, hburnin0, e2, dmi0, hpriors0, file = "data/modelling_data/e2hfit0.RData")
# hgelman(hfit0)

## model1; v ~ SS + Q ------------------
model1 <- BuildModel(
  p.map     = list(a = "1", v = c("SS", "Q"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar1 <- length(GetPNames(model1))
dmi1 <- BuildDMI(data.frame(d2), model1)

p.prior1 <- BuildPrior(
  dists = rep("tnorm", npar1),
  p1    = c(a = 2,   v.3.F=3,  v.7.F=4, v.9.F=5, v.3.V=2,  v.7.V = 3,
            v.9.V=4, z = .5, t0 = .2),
  p2    = rep(2, npar1),
  lower = c(0, rep(-5, 6), rep(0, 2)),
  upper = c(8, rep(10, 6),  3, 3))
mu.prior1 <- BuildPrior(
  dists = rep("tnorm", npar1),
  p1    = c(a = 2,   v.3.F=3,  v.7.F=4, v.9.F=5, v.3.V=2,  v.7.V = 3,
            v.9.V=4, z = .5, t0 = .2),
  p2    = rep(2, npar1),
  lower = c(0, rep(-5,6), rep(0, 2)),
  upper = c(8, rep(10,6), 3, 3))
sigma.prior1 <- BuildPrior(
  dists = rep("unif", npar1),
  p1    = rep(0, npar1),
  p2    = rep(3, npar1),
  upper = rep(NA, npar1))
names(sigma.prior1) <- GetPNames(model1)
hpriors1 <- list(pprior=p.prior1, location=mu.prior1, scale=sigma.prior1)
hburnin1 <- StartNewsamples(data=dmi1, prior=hpriors1, thin=2)
save(hburnin1, dmi1, hpriors1, file = "data/modelling_data/e2hfit1.RData")

hfit1 <- run(hburnin1)
save(hfit1, hburnin1, dmi1, hpriors1, file = "data/modelling_data/e2hfit1.RData")
rhat0 <- hgelman(hfit1)

## model2; v ~ SS -------------
model2 <- BuildModel(
  p.map     = list(a = "1", v = c("SS"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar2 <- length(GetPNames(model2))
dmi2 <- BuildDMI(data.frame(d2), model2)

pop.scale2 <- c(a = .05, v.3 =.15, v.7 = .15, v.9=.15, z = .1, t0 = .05)
p.prior2 <- BuildPrior(
  dists = rep("tnorm", npar2),
  p1    = c(a = 2,   v.3=3,  v.7=4, v.9=5, z = .5, t0 = .2),
  p2    = rep(2, npar2),
  lower = c(0, rep(-5, 3), rep(0, 2)),
  upper = c(8, rep(10, 3),  3, 3))
mu.prior2 <- BuildPrior(
    dists = rep("tnorm", npar2),
    p1    = c(a = 2,   v.3=3,  v.7=4, v.9=5, z = .5, t0 = .2),
    p2    = rep(2, npar2),
    lower = c(0, rep(-5,3), rep(0, 2)),
    upper = c(8, rep(10,3), 3, 3))
sigma.prior2 <- BuildPrior(
  dists = rep("unif", npar2),
  p1    = rep(0, npar2),
  p2    = rep(3, npar2),
  upper = rep(NA, npar2))
names(sigma.prior2) <- GetPNames(model2)
hpriors2 <- list(pprior=p.prior2, location=mu.prior2, scale=sigma.prior2)

hburnin2 <- StartNewsamples(data=dmi2, prior=hpriors2, thin=4)
save(hburnin2, dmi2, hpriors2, file = "data/modelling_data/e2hfit2.RData")
hfit2 <- run(hburnin2, thin=8)
save(hfit2, hburnin2, dmi2, hpriors2, file = "data/modelling_data/e2hfit2.RData")
rhat0 <- hgelman(hfit2)

## model3; a~Q + v~SS -------------------
model3 <- BuildModel(
  p.map     = list(a = "Q", v = c("SS"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), I = c("50", "400"),
                   Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar3 <- length(GetPNames(model3))
dmi3 <- BuildDMI(data.frame(d2), model3)

p.prior3 <- BuildPrior(
  dists = rep("tnorm", npar3),
  p1    = c(2, 2.5, 4.01, 2.93, 4.92, .5, .19),
  p2    = rep(2, npar3),
  lower = c(0, 0, rep(-5, 3), rep(0, 2)),
  upper = c(8, 8, rep(10, 3),  3, 3))
mu.prior3 <- BuildPrior(
  dists = rep("tnorm", npar3),
  p1    = c(2, 2.5, 4.01, 2.93, 4.92, .5, .19),
  p2    = rep(2, npar3),
  lower = c(0, 0, rep(-5,3), rep(0, 2)),
  upper = c(8, 8, rep(10,3), 3, 3))
sigma.prior3 <- BuildPrior(
  dists = rep("unif", npar3),
  p1    = rep(0, npar3),
  p2    = rep(3, npar3),
  upper = rep(NA, npar3))

names(p.prior3) <- GetPNames(model3)
names(mu.prior3) <- GetPNames(model3)
names(sigma.prior3) <- GetPNames(model3)
hpriors3 <- list(pprior=p.prior3, location=mu.prior3, scale=sigma.prior3)

# load("data/e2hfit3.RData")
hburnin3 <- StartNewsamples(data=dmi3, prior=hpriors3, thin=4)
save(hburnin3, e2, dmi3, hpriors3, file = "data/modelling_data/e2hfit3.RData")

hfit3 <- run(hburnin3, thin=8)
save(hfit3, hburnin3, e2, dmi3, hpriors3, file = "data/modelling_data/e2hfit3.RData")
hgelman(hfit3)

## model4; a ~ Q & v ~ SS + Q -------------------
model4 <- BuildModel(
  p.map     = list(a = "Q", v = c("SS", "Q"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), I = c("50", "400"),
                   Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar4 <- length(GetPNames(model4))
dmi4 <- BuildDMI(data.frame(d2), model4)

p.prior4 <- BuildPrior(
  dists = rep("tnorm", npar4),
  p1    = c(2, 2.5, 4.01, 3.53, 2.93, 4.92, 4.50, 3.98, .5, .19),
  p2    = rep(2, npar4),
  lower = c(0, 0, rep(-5, 6), rep(0, 2)),
  upper = c(8, 8, rep(10, 6),  3, 3))
mu.prior4 <- BuildPrior(
  dists = rep("tnorm", npar4),
  p1    = c(2, 2.5, 4.01, 3.53, 2.93, 4.92, 4.50, 3.98, .5, .19),
  p2    = rep(2, npar4),
  lower = c(0, 0, rep(-5,6), rep(0, 2)),
  upper = c(8, 8, rep(10,6), 3, 3))
sigma.prior4 <- BuildPrior(
  dists = rep("unif", npar4),
  p1    = rep(0, npar4),
  p2    = rep(3, npar4),
  upper = rep(NA, npar4))

names(p.prior4) <- GetPNames(model4)
names(mu.prior4) <- GetPNames(model4)
names(sigma.prior4) <- GetPNames(model4)
hpriors4 <- list(pprior=p.prior4, location=mu.prior4, scale=sigma.prior4)

hburnin4 <- StartNewsamples(data=dmi4, prior=hpriors4, thin=4)
save(hburnin4, dmi4, hpriors4, file = "data/modelling_data/e2hfit4.RData")
hfit4 <- run(hburnin4, thin=4)
save(hfit4, hburnin4, dmi4, hpriors4, file = "data/modelling_data/e2hfit4.RData")

## model5; a~I & v~SS -------------------
model5 <- BuildModel(
  p.map     = list(a = "I", v = c("SS"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), I = c("50", "400"),
                   Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar5 <- length(GetPNames(model5))
dat0 <- data.frame(d2)
dmi5 <- BuildDMI(dat0, model5)

p.prior5 <- BuildPrior(
  dists = rep("tnorm", npar5),
  p1    = c(2.5, 2.5, 4.01, 2.93, 4.92, .5, .19),
  p2    = rep(2, npar5),
  lower = c(0, 0, rep(-5, 3), rep(0, 2)),
  upper = c(8, 8, rep(10, 3),  3, 3))
mu.prior5 <- BuildPrior(
  dists = rep("tnorm", npar5),
  p1    = c(2.5, 2.5, 4.01, 2.93, 4.92, .5, .19),
  p2    = rep(2, npar5),
  lower = c(0, 0, rep(-5,3), rep(0, 2)),
  upper = c(8, 8, rep(10,3), 3, 3))
sigma.prior5 <- BuildPrior(
  dists = rep("unif", npar5),
  p1    = rep(0, npar5),
  p2    = rep(3, npar5),
  upper = rep(NA, npar5))

names(p.prior5) <- GetPNames(model5)
names(mu.prior5) <- GetPNames(model5)
names(sigma.prior5) <- GetPNames(model5)
hpriors5 <- list(pprior=p.prior5, location=mu.prior5, scale=sigma.prior5)

# load("data/modelling_data/e2hfit5.RData")
hburnin5 <- StartNewsamples(data=dmi5, prior=hpriors5, thin=4)
save(hburnin5, dmi5, hpriors5, file = "data/modelling_data/e2hfit5.RData")
hfit5 <- run(hburnin5, thin=8)
save(hfit5, hburnin5, dmi5, hpriors5, file = "data/modelling_data/e2hfit5.RData")
hgelman(hfit5)

## model6; a~I & v~SS+Q-------------------
model6 <- BuildModel(
  p.map     = list(a = "I", v = c("SS", "Q"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), I = c("50", "400"),
                   Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar6 <- length(GetPNames(model6))
dmi6 <- BuildDMI(data.frame(d2), model6)

p.prior6 <- BuildPrior(
  dists = rep("tnorm", npar6),
  p1    = c(2.5, 2.5, 4.01, 3.53, 2.93, 4.92, 4.50, 3.98, .5, .19),
  p2    = rep(2, npar6),
  lower = c(0, 0, rep(-5, 6), rep(0, 2)),
  upper = c(8, 8, rep(10, 6),  3, 3))
mu.prior6 <- BuildPrior(
  dists = rep("tnorm", npar6),
  p1    = c(2.5, 2.5, 4.01, 3.53, 2.93, 4.92, 4.50, 3.98, .5, .19),
  p2    = rep(2, npar6),
  lower = c(0, 0, rep(-5,6), rep(0, 2)),
  upper = c(8, 8, rep(10,6), 3, 3))
sigma.prior6 <- BuildPrior(
  dists = rep("unif", npar6),
  p1    = rep(0, npar6),
  p2    = rep(3, npar6),
  upper = rep(NA, npar6))

names(p.prior6) <- GetPNames(model6)
names(mu.prior6) <- GetPNames(model6)
names(sigma.prior6) <- GetPNames(model6)
hpriors6 <- list(pprior=p.prior6, location=mu.prior6, scale=sigma.prior6)

hburnin6 <- StartNewsamples(data=dmi6, prior=hpriors6, thin=4)
save(hburnin6, dmi6, hpriors6, file = "data/modelling_data/e2hfit6.RData")
hfit6 <- run(hburnin6, thin=8)
save(hfit6, hburnin6, dmi6, hpriors6, file = "data/modelling_data/e2hfit6.RData")
hgelman(hfit6)


## model7; a~I+Q & v~SS -------------------
model7 <- BuildModel(
  p.map     = list(a = c("I","Q"), v = c("SS"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), I = c("50", "400"),
                   Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar7 <- length(GetPNames(model7))

dat0 <- data.frame(d2)
dmi7 <- BuildDMI(dat0, model7)

p.prior7 <- BuildPrior(
  dists = rep("tnorm", npar7),
  p1    = c(2.5, 2.5, 2.5, 2.5, 4.01, 2.93, 4.92, .5, .19),
  p2    = rep(2, npar7),
  lower = c(rep(0, 4), rep(-5, 3), rep(0, 2)),
  upper = c(rep(8, 4), rep(10, 3),  3, 3))
mu.prior7 <- BuildPrior(
  dists = rep("tnorm", npar7),
  p1    = c(2.5, 2.5, 2.5, 2.5, 4.01, 2.93, 4.92, .5, .19),
  p2    = rep(2, npar7),
  lower = c(rep(0, 4), rep(-5,3), rep(0, 2)),
  upper = c(rep(8, 4), rep(10,3), 3, 3))
sigma.prior7 <- BuildPrior(
  dists = rep("beta", npar7),
  p1    = rep(1, npar7),
  p2    = rep(1, npar7),
  upper = rep(NA, npar7))
sigma.prior7 <- BuildPrior(
  dists = rep("unif", npar7),
  p1    = rep(0, npar7),
  p2    = rep(3, npar7),
  upper = rep(NA, npar7))

names(p.prior7) <- GetPNames(model7)
names(mu.prior7) <- GetPNames(model7)
names(sigma.prior7) <- GetPNames(model7)
hpriors7 <- list(pprior=p.prior7, location=mu.prior7, scale=sigma.prior7)

# load("data/modelling_data/e2hfit7.RData")
hburnin7 <- StartNewsamples(data=dmi7, prior=hpriors7, thin=4)
save(hburnin7, dmi7, hpriors7, file = "data/modelling_data/e2hfit7.RData")
hfit7 <- run(hburnin7, thin=8)
save(hfit7, hburnin7, dmi7, hpriors7, file = "data/modelling_data/e2hfit7.RData")
hgelman(hfit7)

## IQ model8 (a~I+Q + v~SS+Q) (V)-------------------
model8 <- BuildModel(
  p.map     = list(a = c("I","Q"), v = c("SS", "Q"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), I = c("50", "400"),
                   Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar8 <- length(GetPNames(model8))

dat0 <- data.frame(d2)
dmi8 <- BuildDMI(dat0, model8)

p.prior8 <- BuildPrior(
  dists = rep("tnorm", npar8),
  p1    = c(2.5, 2.5, 2.5, 2.5, 
            4.01, 2.93, 4.92, 
            4.01, 2.93, 4.92, 
            .5, .19),
  p2    = rep(2, npar8),
  lower = c(rep(0, 4), rep(-5, 6), rep(0, 2)),
  upper = c(rep(8, 4), rep(10, 6),  3, 3))
mu.prior8 <- BuildPrior(
  dists = rep("tnorm", npar8),
  p1    = c(2.5, 2.5, 2.5, 2.5, 
            4.01, 2.93, 4.92,
            4.01, 2.93, 4.92,
            .5, .19),
  p2    = rep(2, npar8),
  lower = c(rep(0, 4), rep(-5,6), rep(0, 2)),
  upper = c(rep(8, 4), rep(10,6), 3, 3))
sigma.prior8 <- BuildPrior(
  dists = rep("unif", npar8),
  p1    = rep(0, npar8),
  p2    = rep(5, npar8),
  upper = rep(NA, npar8))

names(p.prior8) <- GetPNames(model8)
names(mu.prior8) <- GetPNames(model8)
names(sigma.prior8) <- GetPNames(model8)
hpriors8 <- list(pprior=p.prior8, location=mu.prior8, scale=sigma.prior8)

hburnin8 <- StartNewsamples(data=dmi8, prior=hpriors8, thin=2)
save(hburnin8, e2, dmi8, hpriors8, file = "data/modelling_data/e2hfit8.RData")
hfit8 <- run(hburnin8, thin=2)
save(hfit8, hburnin8, e2, dmi8, hpriors8, file = "data/modelling_data/e2hfit8.RData")
hgelman(hfit8)

## model9; a~I+Q & v~SS+Q+I -------------------
model9 <- BuildModel(
  p.map     = list(a = c("I","Q"), v = c("SS", "I", "Q"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), I = c("50", "400"),
                   Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar9 <- length(GetPNames(model9))
## 18
dat0 <- data.frame(d3)
dmi9 <- BuildDMI(dat0, model9)

p.prior9 <- BuildPrior(
  dists = rep("tnorm", npar9),
  p1    = c(2.5, 2.5, 2.5, 2.5, 
            4.01, 2.93, 4.92, 
            4.92, 4.50, 3.98,
            4.01, 2.93, 4.92, 
            4.92, 4.50, 3.98,
            .5, .19),
  p2    = c(.5, .5, .5, .5, 
            .23, .18, .24,
            .24, .18, .13,
            .23, .18, .24,
            .24, .18, .13,
            07, .05),
  lower = c(rep(0, 4), rep(-5, 12), rep(0, 2)),
  upper = c(rep(8, 4), rep(10, 12),  3, 3))
mu.prior9 <- BuildPrior(
  dists = rep("tnorm", npar9),
  p1    = c(2.5, 2.5, 2.5, 2.5, 
            4.01, 2.93, 4.92,
            4.92, 4.50, 3.98,
            4.01, 2.93, 4.92,
            4.92, 4.50, 3.98,
            .5, .19),
  p2    = c(.5, .5, .5, .5,
            .23, .18, .24,
            .24, .18, .13,
            .23, .18, .24,
            .24, .18, .13,
            .07, .05),
  lower = c(rep(0, 4), rep(-5,12), rep(0, 2)),
  upper = c(rep(8, 4), rep(10,12), 3, 3))
sigma.prior9 <- BuildPrior(
  dists = rep("beta", npar9),
  p1    = rep(1, npar9),
  p2    = rep(1, npar9),
  upper = rep(NA, npar9))

names(p.prior9) <- GetPNames(model9)
names(mu.prior9) <- GetPNames(model9)
names(sigma.prior9) <- GetPNames(model9)
hpriors9 <- list(pprior=p.prior9, location=mu.prior9, scale=sigma.prior9)

## 24 mins
hburnin9 <- StartNewsamples(data=dmi9, prior=hpriors9)
save(hburnin9, e3, dmi9, hpriors9, file = "data/modelling_data/e3hfit9.RData")
## 122.9 mins
hfit9 <- run(hburnin9, thin=2)
save(hfit9, hburnin9, e3, dmi9, hpriors9, file = "data/modelling_data/e3hfit9.RData")
hgelman(hfit9)
## 244 mins
# hfit9 <- run(hfit9, thin=4)
# save(hfit9, hburnin9, e3, dmi9, hpriors9, file = "data/modelling_data/e3hfit9.RData")
load("data/modelling_data/e3hfit9.RData")
plot(hfit9, hyper=T)
plot(hfit9)

## model10 (a~SS) -------------
model10 <- BuildModel(
  p.map     = list(a = "SS", v = "1", z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar10 <- length(GetPNames(model10))
dmi10 <- BuildDMI(data.frame(d2), model10)

p.prior10 <- BuildPrior(
  dists = rep("tnorm", npar10),
  p1    = c(a.3 = 2, a.7=2, a.9=2,  v=3, z = .5, t0 = .2),
  p2    = rep(2, npar10),
  lower = c(0, 0, 0, rep(-5, 1), rep(0, 2)),
  upper = c(8, 8, 8, rep(10, 1),  3, 3))
mu.prior10 <- BuildPrior(
  dists = rep("tnorm", npar10),
  p1    = c(a.3 = 2, a.7=2, a.9=2,  v=3, z = .5, t0 = .2),
  p2    = rep(2, npar10),
  lower = c(0, 0, 0, rep(-5,1), rep(0, 2)),
  upper = c(8, 8, 8, rep(10,1), 3, 3))
sigma.prior10 <- BuildPrior(
  dists = rep("unif", npar10),
  p1    = rep(0, npar10),
  p2    = rep(3, npar10),
  upper = rep(NA, npar10))
names(sigma.prior10) <- GetPNames(model10)
hpriors10 <- list(pprior=p.prior10, location=mu.prior10, scale=sigma.prior10)

 hburnin10 <- StartNewsamples(data=dmi10, prior=hpriors10)
 hfit10 <- run(hburnin10)
 save(hfit10, hburnin10, dmi10, priors10, hpriors10, 
      file = "data/modelling_data/e2hfit10.RData")
rhat0 <- hgelman(hfit10)

## model11 (a~SS+Q) -------------
model11 <- BuildModel(
  p.map     = list(a = c("SS","Q"), v = "1", z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")

npar11 <- length(GetPNames(model11))
dmi11 <- BuildDMI(data.frame(d2), model11)

p.prior11 <- BuildPrior(
  dists = rep("tnorm", npar11),
  p1    = c(a.3.F = 2, a.7.F=2, a.9.F=2, a.3.V = 2, a.7.V=2, a.9.V=2, v=3, z = .5, t0 = .2),
  p2    = rep(2, npar11),
  lower = c(rep(0, 6), rep(-5, 1), rep(0, 2)),
  upper = c(rep(8, 6), rep(10, 1),  3, 3))
mu.prior11 <- BuildPrior(
  dists = rep("tnorm", npar11),
  p1    = c(a.3.F = 2, a.7.F=2, a.9.F=2, a.3.V = 2, a.7.V=2, a.9.V=2, v=3, z = .5, t0 = .2),
  p2    = rep(2, npar11),
  lower = c(rep(0, 6), rep(-5, 1), rep(0, 2)),
  upper = c(rep(8, 6), rep(10, 1),  3, 3))
sigma.prior11 <- BuildPrior(
  dists = rep("unif", npar11),
  p1    = rep(0, npar11),
  p2    = rep(3, npar11),
  upper = rep(NA, npar11))
names(sigma.prior11) <- GetPNames(model11)
hpriors11 <- list(pprior=p.prior11, location=mu.prior11, scale=sigma.prior11)

 hburnin11 <- StartNewsamples(data=dmi11, prior=hpriors11, thin=4)
 hfit11 <- run(hburnin11)
 save(hfit11, hburnin11, dmi11, hpriors11, 
      file = "data/modelling_data/e2hfit11.RData")
rhat0 <- hgelman(hfit11)

## model12; a~SS+Q+ISI -------------
model12 <- BuildModel(
  p.map     = list(a = c("SS","Q", "I"), v = "1", z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), I = c("50", "400"),
                   Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar12 <- length(GetPNames(model12))
dmi12 <- BuildDMI(data.frame(d2), model12)

p.prior12 <- BuildPrior(
  dists = rep("tnorm", npar12),
  p1    = c(a.3.F.50 = 2, a.7.F.50=2, a.9.F.50=2, a.3.V.50 = 2, a.7.V.50=2, a.9.V.50=2,
            a.3.F.400 = 2, a.7.F.400=2, a.9.F.400=2, a.3.V.400 = 2, a.7.V.400=2, a.9.V.400=2,
            v=3, z = .5, t0 = .2),
  p2    = rep(2, npar12),
  lower = c(rep(0, 12), rep(-5, 1), rep(0, 2)),
  upper = c(rep(8, 12), rep(10, 1),  3, 3))
mu.prior12 <- BuildPrior(
    dists = rep("tnorm", npar12),
      p1    = c(a.3.F.50 = 2, a.7.F.50=2, a.9.F.50=2, a.3.V.50 = 2, a.7.V.50=2, a.9.V.50=2,
            a.3.F.400 = 2, a.7.F.400=2, a.9.F.400=2, a.3.V.400 = 2, a.7.V.400=2, a.9.V.400=2,
            v=3, z = .5, t0 = .2),
  p2    = rep(2, npar12),
  lower = c(rep(0, 12), rep(-5, 1), rep(0, 2)),
  upper = c(rep(8, 12), rep(10, 1),  3, 3))
sigma.prior12 <- BuildPrior(
  dists = rep("unif", npar12),
  p1    = rep(0, npar12),
  p2    = rep(3, npar12),
  upper = rep(NA, npar12))
names(sigma.prior12) <- GetPNames(model12)
hpriors12 <- list(pprior=p.prior12, location=mu.prior12, scale=sigma.prior12)

 hburnin12 <- StartNewsamples(data=dmi12, prior=hpriors12, thin=4)
 hfit12 <- run(hburnin12)
 save(hfit12, hburnin12, dmi12, hpriors12, file = "data/modelling_data/e2hfit12.RData")
rhat0 <- hgelman(hfit12)

## model13; a~SS & v~Q -------------
model13 <- BuildModel(
  p.map     = list(a = "SS", v = "Q", z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar13 <- length(GetPNames(model13))
dmi13 <- BuildDMI(data.frame(d2), model13)

p.prior13 <- BuildPrior(
  dists = rep("tnorm", npar13),
  p1    = c(a.3 = 2, a.7=2, a.9=2, v.F=3, v.V=3, z = .5, t0 = .2),
  p2    = rep(2, npar13),
  lower = c(rep(0, 3), rep(-5, 2), rep(0, 2)),
  upper = c(rep(8, 3), rep(10, 2),  3, 3))
mu.prior13 <- BuildPrior(
    dists = rep("tnorm", npar13),
      p1    = c(a.3 = 2, a.7=2, a.9=2, v.F=3, v.V=3, z = .5, t0 = .2),
  p2    = rep(2, npar13),
  lower = c(rep(0, 3), rep(-5, 2), rep(0, 2)),
  upper = c(rep(8, 3), rep(10, 2),  3, 3))
sigma.prior13 <- BuildPrior(
  dists = rep("unif", npar13),
  p1    = rep(0, npar13),
  p2    = rep(3, npar13),
  upper = rep(NA, npar13))
names(sigma.prior13) <- GetPNames(model13)
hpriors13 <- list(pprior=p.prior13, location=mu.prior13, scale=sigma.prior13)

hburnin13 <- StartNewsamples(data=dmi13, prior=hpriors13, thin=4)
hfit13 <- run(hburnin13, thin=4)
 save(hfit13, hburnin13, dmi13, hpriors13, file = "data/modelling_data/e2hfit13.RData")
 rhat0 <- hgelman(hfit13)

## model14; a~SS & v~Q+ISI -------------
model14 <- BuildModel(
  p.map     = list(a = "SS", v = c("Q", "I"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), I = c("50", "400"),
                   Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar14 <- length(GetPNames(model14))
dmi14 <- BuildDMI(data.frame(d2), model14)

p.prior14 <- BuildPrior(
  dists = rep("tnorm", npar14),
  p1    = c(a.3=2, a.7=2, a.9=2, v.F.50=3, v.V.50=3, v.F.400=3, v.V.400=3, z = .5, t0 = .2),
  p2    = rep(2, npar14),
  lower = c(rep(0, 3), rep(-5, 4), rep(0, 2)),
  upper = c(rep(8, 3), rep(10, 4),  3, 3))
mu.prior14 <- BuildPrior(
    dists = rep("tnorm", npar14),
  p1    = c(a.3=2, a.7=2, a.9=2, v.F.50=3, v.V.50=3, v.F.400=3, v.V.400=3, z = .5, t0 = .2),
  p2    = rep(2, npar14),
  lower = c(rep(0, 3), rep(-5, 4), rep(0, 2)),
  upper = c(rep(8, 3), rep(10, 4),  3, 3))
sigma.prior14 <- BuildPrior(
  dists = rep("unif", npar14),
  p1    = rep(0, npar14),
  p2    = rep(3, npar14),
  upper = rep(NA, npar14))
names(sigma.prior14) <- GetPNames(model14)
hpriors14 <- list(pprior=p.prior14, location=mu.prior14, scale=sigma.prior14)

 hburnin14 <- StartNewsamples(data=dmi14, prior=hpriors14, thin=4)
 hfit14 <- run(hburnin14, thin=4)
 save(hfit14, hburnin14, dmi14, hpriors14, file = "data/modelling_data/e2hfit14.RData")
rhat0 <- hgelman(hfit14)

## model15; a~SS & v~SS+Q+ISI -------------
model15 <- BuildModel(
  p.map     = list(a = "SS", v = c("SS","Q", "I"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), I = c("50", "400"),
                   Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar15 <- length(GetPNames(model15))
dmi15 <- BuildDMI(data.frame(d2), model15)

p.prior15 <- BuildPrior(
  dists = rep("tnorm", npar15),
  p1    = c(2, 2, 2,
            4.01, 2.93, 4.92, 
            4.92, 4.50, 3.98,
            4.01, 2.93, 4.92, 
            4.92, 4.50, 3.98,
            z = .5, t0 = .2),
  p2    = rep(2, npar15),
  lower = c(rep(0, 3), rep(-5, 12), rep(0, 2)),
  upper = c(rep(8, 3), rep(10, 12),  3, 3))
mu.prior15 <- BuildPrior(
    dists = rep("tnorm", npar15),
      p1    = c(2, 2, 2,
            4.01, 2.93, 4.92, 
            4.92, 4.50, 3.98,
            4.01, 2.93, 4.92, 
            4.92, 4.50, 3.98,
            z = .5, t0 = .2),
  p2    = rep(2, npar15),
  lower = c(rep(0, 3), rep(-5, 12), rep(0, 2)),
  upper = c(rep(8, 3), rep(10, 12),  3, 3))
sigma.prior15 <- BuildPrior(
  dists = rep("unif", npar15),
  p1    = rep(0, npar15),
  p2    = rep(3, npar15),
  upper = rep(NA, npar15))
names(p.prior15) <- GetPNames(model15)
names(mu.prior15) <- GetPNames(model15)
names(sigma.prior15) <- GetPNames(model15)
hpriors15 <- list(pprior=p.prior15, location=mu.prior15, scale=sigma.prior15)

 hburnin15 <- StartNewsamples(data=dmi15, prior=hpriors15, thin=4)
 hfit15 <- run(hburnin15)
 save(hfit15, hburnin15, dmi15, hpriors15, file = "data/modelling_data/e2hfit15.RData")
rhat0 <- hgelman(hfit15)

## Compare models ----------
load("data/modelling_data/e2hfit0.RData")  ## (done)
load("data/modelling_data/e2hfit1.RData")  ## (done)
load("data/modelling_data/e2hfit2.RData")  ## (done)
load("data/modelling_data/e2hfit3.RData")  ## (done)
load("data/modelling_data/e2hfit4.RData")  ## (done)
load("data/modelling_data/e2hfit5.RData")  ## (done)
load("data/modelling_data/e2hfit6.RData")  ## (done)
load("data/modelling_data/e2hfit7.RData")  ## (done)
load("data/modelling_data/e2hfit8.RData")  ## (done)
load("data/modelling_data/e2hfit9.RData")  ## (not converge)
load("data/modelling_data/e2hfit10.RData") ## (done)
load("data/modelling_data/e2hfit11.RData") ## (done)
load("data/modelling_data/e2hfit12.RData") ## (not converge)
load("data/modelling_data/e2hfit13.RData") ## (done)
load("data/modelling_data/e2hfit14.RData") ## (done)
source("R/functions/predict_one.R")
hgelman(hfit0); hgelman(hfit1); hgelman(hfit2); 
hgelman(hfit3); hgelman(hfit4); hgelman(hfit5); 
hgelman(hfit6); hgelman(hfit7); hgelman(hfit8);
hgelman(hfit10); hgelman(hfit11); hgelman(hfit12); 
hgelman(hfit13); hgelman(hfit14);

dev0 <- DIC(hfit0, BPIC = TRUE)
dev1 <- DIC(hfit1, BPIC = TRUE)
dev2 <- DIC(hfit2, BPIC = TRUE)
dev3 <- DIC(hfit3, BPIC = TRUE)
dev4 <- DIC(hfit4, BPIC = TRUE)
dev5 <- DIC(hfit5, BPIC = TRUE)
dev6 <- DIC(hfit6, BPIC = TRUE)
dev7 <- DIC(hfit7, BPIC = TRUE)
dev8 <- DIC(hfit8, BPIC = TRUE)
dev10 <- DIC(hfit10, BPIC = TRUE)
dev11 <- DIC(hfit11, BPIC = TRUE)

dev13 <- DIC(hfit13, BPIC = TRUE)
dev14 <- DIC(hfit14, BPIC = TRUE)

res <- summary(hfit5, hyper = TRUE)  
round(res$quantiles, 2)
## Summed BPIC: -15192.87 ##         v~SS+Q+I  model0 M1 
## Summed BPIC: -15319.67 ##         v~SS+Q    model1 M2
## Summed BPIC: -14853.99 ##         v~SS      model2 M3
## Summed BPIC: -15644.45 ## a~Q   & v~SS      model3 M4
## Summed BPIC: -15633.07 ## a~Q   & v~SS+Q    model4 M5
## Summed BPIC: -14978.63 ## a~I   & v~SS      model5 M6 
## Summed BPIC: -15435.71 ## a~I   & v~SS+Q    model6 M7 
## Summed BPIC: -15746.85 ## a~I+Q & v~SS      model7 M8 
## Summed BPIC: -15745.77 ## a~I+Q & v~SS+Q    model8 M9
## Summed BPIC:           ## a~I+Q & v~SS+Q+I  model9 (not converge)

## Summed BPIC: -14121.81 ## a~SS              model10 
## Summed BPIC: -14891.9  ## a~SS+Q            model11 
##                        ## a~SS+Q+I          model12 (not converge)
## Summed BPIC: -14489.62 ## a~SS & v~Q        model13
##                        ## a~SS & v~Q+ISI    model14


ics <- c(sum(dev0), sum(dev1), sum(dev2), sum(dev3), sum(dev4), sum(dev5), 
         sum(dev6), sum(dev7), sum(dev8))
res0 <- ggdmc:::weightIC(ics)
##            M1     M2     M3     M4     M5     M6     M7   M8   M9
## IC-min 553.98 427.18 892.86 102.39 113.77 768.22 311.13 0.00 1.08
## w        0.00   0.00   0.00   0.00   0.00   0.00   0.00 0.63 0.37

## Use Word Model labelling
word_lab <- c('M6', 'M2', 'M0', 'M1', 'M3',
              'M4', 'M5', 'M7', 'M8')
eqn <- c('v~SS+Q+I', 'v~SS+Q', 'v~SS', 'a~Q & v~SS', 'a~Q & v~SS+Q',
         'a~I & v~SS', 'a~I & v~SS+Q', 'a~I+Q & v~SS', 'a~I+Q & v~SS+Q')
res1 <- round(ics - sum(dev2), 2)
dat <- data.frame(M=word_lab, IC=ics, rIC=res1, E=eqn)
dat[order(dat[,1]),]
#    M      IC              E
# 3 M0    0.00           v~SS
# 4 M1 -790.46     a~Q & v~SS
# 2 M2 -465.68         v~SS+Q
# 5 M3 -779.08   a~Q & v~SS+Q
# 6 M4 -124.64     a~I & v~SS
# 7 M5 -581.72   a~I & v~SS+Q
# 1 M6 -338.87       v~SS+Q+I
# 8 M7 -892.86   a~I+Q & v~SS
# 9 M8 -891.78 a~I+Q & v~SS+Q

# 3 M0    0.00           v~SS
# 6 M4 -124.64     a~I & v~SS

# 2 M2 -465.68         v~SS+Q
# 7 M5 -581.72   a~I & v~SS+Q

# M2 -465.68       v~SS+Q
# M6 -338.87       v~SS+Q+I

# 4 M1 -790.46     a~Q & v~SS
# 6 M4 -124.64     a~I & v~SS

# 5 M3 -779.08   a~Q & v~SS+Q
# 7 M5 -581.72   a~I & v~SS+Q
