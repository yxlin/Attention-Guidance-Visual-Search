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
load("data/visual_search.RData")
require(ggdmc) ## version 0.2.6.8

dplyr::tbl_df(e3)
e3$R <- factor( ifelse(e3$R == "L", "left", ifelse(e3$R == "R", "right", NA)))
d3 <- data.table::data.table(e3)
sapply(d3, levels)
d3[, .N, .(SS,Q,I,s)]
length(table(d3$s))

## model0; v~SS + I + Q -------------------
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
dmi0 <- BuildDMI(data.frame(d3), model0)

p.prior0 <- BuildPrior(
  dists = rep("tnorm", npar0),
  p1    = c(a = 2, v.3.50.F = 4,  v.7.50.F = 3.5,  v.9.50.F = 3,
                      v.3.400.F = 5, v.7.400.F = 4.5, v.9.400.F = 4,
                      v.3.50.V = 2,  v.7.50.V = 1.5,  v.9.50.V = 1,
                      v.3.400.V = 3, v.7.400.V = 2.5,  v.9.400.V = 2,
            z = .5, t0 = .2),
  p2    = rep(2, npar0),
  lower = c(0, rep(-5, 12), rep(0, 2)),
  upper = c(8, rep(10, 12),  3, 3))
mu.prior0 <- BuildPrior(
  dists = rep("tnorm", npar0),
  p1    = c(a = 2, v.3.50.F = 4,  v.7.50.F = 3.5,  v.9.50.F = 3,
                      v.3.400.F = 5, v.7.400.F = 4.5, v.9.400.F = 4,
                      v.3.50.V = 2,  v.7.50.V = 1.5,  v.9.50.V = 1,
                      v.3.400.V = 3, v.7.400.V = 2.5,  v.9.400.V = 2,
            z = .5, t0 = .2),
  p2    = rep(2, npar0),
  lower = c(0, rep(-5,12), rep(0, 2)),
  upper = c(8, rep(10,12), 3, 3))
sigma.prior0 <- BuildPrior(
  dists = rep("unif", npar0),
  p1    = rep(0, npar0),
  p2    = rep(3, npar0),
  upper = rep(NA, npar0))
names(sigma.prior0) <- GetPNames(model0)
hpriors0 <- list(pprior=p.prior0, location=mu.prior0, scale=sigma.prior0)

hburnin0 <- StartNewsamples(data=dmi0, prior=hpriors0, thin=4)
save(hburnin0, dmi0, hpriors0, file = "data/modelling_data/e3hfit0.RData")
hfit0 <- run(hburnin0)
save(hfit0, hburnin0, dmi0, hpriors0, file = "data/modelling_data/e3hfit0.RData")
load("data/e3hfit0.RData")
rhat0 <- hgelman(hfit0)

## model1; v~SS+Q ------------------
model1 <- BuildModel (
  p.map     = list(a = "1", v = c("SS", "Q"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar1 <- length(GetPNames(model1))
dmi1 <- BuildDMI(data.frame(d3), model1)

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

hburnin1 <- StartNewsamples(data=dmi1, prior=hpriors1, thin=4)
save(hburnin1, dmi1, hpriors1, file = "data/modelling_data/e3hfit1.RData")
hfit1 <- run(hburnin1, thin=8)
save(hfit1, hburnin1, dmi1, hpriors1, file = "data/modelling_data/e3hfit1.RData")
rhat0 <- hgelman(hfit1)

## model2; v~SS -------------
model2 <- BuildModel(
  p.map     = list(a = "1", v = c("SS"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar2 <- length(GetPNames(model2))
dmi2 <- BuildDMI(data.frame(d3), model2)

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
save(hburnin2, dmi2, hpriors2, file = "data/modelling_data/e3hfit2.RData")
hfit2 <- run(hburnin2, thin=8)
save(hfit2, hburnin2, dmi2, hpriors2, file = "data/modelling_data/e3hfit2.RData")
rhat0 <- hgelman(hfit2)

## a~Q model3 -------------------
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
dmi3 <- BuildDMI(data.frame(d3), model3)

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
hburnin3 <- StartNewsamples(data=dmi3, prior=hpriors3, thin=4)
save(hburnin3, dmi3, hpriors3, file = "data/modelling_data/e3hfit3.RData")
hfit3 <- run(hburnin3, thin=8)
save(hfit3, hburnin3, dmi3, hpriors3, file = "data/modelling_data/e3hfit3.RData")
hgelman(hfit3)

## model4; a~Q & v~SS+Q ------------- 
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
dmi4 <- BuildDMI(data.frame(d3), model4)

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
save(hburnin4, dmi4, hpriors4, file = "data/modelling_data/e3hfit4.RData")
hfit4 <- run(hburnin4, thin=8)
save(hfit4, hburnin4, dmi4, hpriors4, file = "data/modelling_data/e3hfit4.RData")
hgelman(hfit4)

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
dmi5 <- BuildDMI(data.frame(d3), model5)

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

hburnin5 <- StartNewsamples(data=dmi5, prior=hpriors5, thin=4)
save(hburnin5, dmi5, hpriors5, file = "data/modelling_data/e3hfit5.RData")
hfit5 <- run(hburnin5, thin=8)
save(hfit5, hburnin5, dmi5, hpriors5, file = "data/modelling_data/e3hfit5.RData")
hgelman(hfit5)

## model6; a~I & v~SS+Q ---------- 
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
dmi6 <- BuildDMI(data.frame(d3), model6)


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
save(hburnin6, dmi6, hpriors6, file = "data/modelling_data/e3hfit6.RData")
hfit6 <- run(hburnin6, thin=4)
save(hfit6, hburnin6, dmi6, hpriors6, file = "data/modelling_data/e3hfit6.RData")
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
dmi7 <- BuildDMI(data.frame(d3), model7)

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
  dists = rep("unif", npar7),
  p1    = rep(0, npar7),
  p2    = rep(3, npar7),
  upper = rep(NA, npar7))

names(p.prior7) <- GetPNames(model7)
names(mu.prior7) <- GetPNames(model7)
names(sigma.prior7) <- GetPNames(model7)
hpriors7 <- list(pprior=p.prior7, location=mu.prior7, scale=sigma.prior7)

hburnin7 <- StartNewsamples(data=dmi7, prior=hpriors7, thin=8)
save(hburnin7, dmi7, hpriors7, file = "data/modelling_data/e3hfit7.RData")
hfit7 <- run(hburnin7, thin=4)
save(hfit7, hburnin7, dmi7, hpriors7, file = "data/modelling_data/e3hfit7.RData")
hgelman(hfit7)

## model8; a~I+Q & v~SS+Q -------------------
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
dmi8 <- BuildDMI(data.frame(d3), model8)

p.prior8 <- BuildPrior(
  dists = rep("tnorm", npar8),
  p1    = c(2.5, 2.5, 2.5, 2.5, 4.01, 2.93, 4.92, 4.92, 4.50, 3.98, .5, .19),
  p2    = rep(2, npar8),
  lower = c(rep(0, 4), rep(-5, 6), rep(0, 2)),
  upper = c(rep(8, 4), rep(10, 6),  3, 3))
mu.prior8 <- BuildPrior(
  dists = rep("tnorm", npar8),
  p1    = c(2.5, 2.5, 2.5, 2.5, 4.01, 2.93, 4.92, 4.92, 4.50, 3.98, .5, .19),
  p2    = rep(2, npar8),
  lower = c(rep(0, 4), rep(-5,6), rep(0, 2)),
  upper = c(rep(8, 4), rep(10,6), 3, 3))
sigma.prior8 <- BuildPrior(
  dists = rep("unif", npar8),
  p1    = rep(0, npar8),
  p2    = rep(3, npar8),
  upper = rep(NA, npar8))

names(p.prior8) <- GetPNames(model8)
names(mu.prior8) <- GetPNames(model8)
names(sigma.prior8) <- GetPNames(model8)
hpriors8 <- list(pprior=p.prior8, location=mu.prior8, scale=sigma.prior8)

hburnin8 <- StartNewsamples(data=dmi8, prior=hpriors8, thin=8)
save(hburnin8, dmi8, hpriors8, file = "data/modelling_data/e3hfit8.RData")
hfit8 <- run(hburnin8, thin=4)
save(hfit8, hburnin8, dmi8, hpriors8, file = "data/modelling_data/e3hfit8.RData")
hgelman(hfit8)


## IQI model9 (a~I+Q + v~SS+Q+I) -------------------
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
hburnin9 <- StartNewsamples(data=dmi9, prior=hpriors9, nmc=10)
save(hburnin9, e3, dmi9, hpriors9, file = "data/modelling_data/e3hfit9.RData")
## 122.9 mins
hfit9 <- run(hburnin9, thin=2)
save(hfit9, hburnin9, e3, dmi9, hpriors9, file = "data/modelling_data/e3hfit9.RData")
hgelman(hfit9)
## 244 mins
# hfit9 <- run(hfit9, thin=4)
# save(hfit9, hburnin9, e3, dmi9, hpriors9, file = "data/modelling_data/e3hfit9.RData")




## Compare models ----------
load("data/modelling_data/e3hfit0.RData") ## done
load("data/modelling_data/e3hfit1.RData") ## done
load("data/modelling_data/e3hfit2.RData") ## done
load("data/modelling_data/e3hfit3.RData") ## done
load("data/modelling_data/e3hfit4.RData") ## done
load("data/modelling_data/e3hfit5.RData") ## done
load("data/modelling_data/e3hfit6.RData") ## done
load("data/modelling_data/e3hfit7.RData") ## done
load("data/modelling_data/e3hfit8.RData") ## done
source("R/functions/predict_one.R")
hgelman(hfit0);
hgelman(hfit1);
hgelman(hfit2); 
hgelman(hfit3);
hgelman(hfit4);
hgelman(hfit5); 
hgelman(hfit6);
hgelman(hfit7);
hgelman(hfit8)

plot(hfit8, hyper=T, pll=F, den=T)
res <- summary(hfit8, hyper = TRUE)  
round(res$quantiles, 2)

dev0 <- DIC(hfit0, BPIC = TRUE)
dev1 <- DIC(hfit1, BPIC = TRUE)
dev2 <- DIC(hfit2, BPIC = TRUE)
dev3 <- DIC(hfit3, BPIC = TRUE)
dev4 <- DIC(hfit4, BPIC = TRUE)
dev5 <- DIC(hfit5, BPIC = TRUE)
dev6 <- DIC(hfit6, BPIC = TRUE)
dev7 <- DIC(hfit7, BPIC = TRUE)
dev8 <- DIC(hfit8, BPIC = TRUE)
## Summed BPIC: -48107.87 ## v~SS+I+Q       model0
## Summed BPIC: -48186.91 ## v~SS+Q         model1
## Summed BPIC: -47204.69 ## v~SS           model2
## Summed BPIC: -48796.57 ## a~Q & v~SS     model3
## Summed BPIC: -49033.04 ## a~Q & v~SS+Q   model4
## Summed BPIC: -47666.43 ## a~I & v~SS     model5
## Summed BPIC: -48656.59 ## a~I & v~SS+Q   model6
## Summed BPIC: -49228.14 ## a~I+Q & v~SS   model7
## Summed BPIC: -49480.46 ## a~I+Q & v~SS+Q model8 

ics <- c(sum(dev0), sum(dev1), sum(dev2), sum(dev3), sum(dev4), sum(dev5), 
         sum(dev6), sum(dev7), sum(dev8))
res0 <- ggdmc:::weightIC(ics)

##             M1      M2      M3     M4     M5      M6     M7     M8 M9
## IC-min 1372.59 1293.55 2275.77 683.89 447.42 1814.03 823.88 252.32  0
## w         0.00    0.00    0.00   0.00   0.00    0.00   0.00   0.00  1

## Use Word Model labelling
word_lab <- c('M6', 'M2', 'M0', 'M1', 'M3',
              'M4', 'M5', 'M7', 'M8')
eqn <- c('v~SS+Q+I', 'v~SS+Q', 'v~SS', 'a~Q & v~SS', 'a~Q & v~SS+Q',
         'a~I & v~SS', 'a~I & v~SS+Q', 'a~I+Q & v~SS', 'a~I+Q & v~SS+Q')
res1 <- round(ics - sum(dev2), 2)
dat <- data.frame(M=word_lab, IC=ics, rIC=res1, E=eqn)
dat[order(dat[,1]),]
##    M        IC      rIC              E
## 3 M0 -47204.69     0.00           v~SS
## 4 M1 -48796.57 -1591.88     a~Q & v~SS
## 2 M2 -48186.91  -982.22         v~SS+Q
## 5 M3 -49033.04 -1828.35   a~Q & v~SS+Q
## 6 M4 -47666.43  -461.74     a~I & v~SS
## 7 M5 -48656.59 -1451.89   a~I & v~SS+Q
## 1 M6 -48107.87  -903.18       v~SS+Q+I
## 8 M7 -49228.14 -2023.45   a~I+Q & v~SS
## 9 M8 -49480.46 -2275.77 a~I+Q & v~SS+Q

