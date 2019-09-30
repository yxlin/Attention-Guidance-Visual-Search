#########################################################################0
## Fixed cue vs. varied cue
## Version: 0.0.3
## Authors: Yi-Shin Lin (yishinlin001@gmail.com)
## Date: 25 Sept, 2019 -- Draft one
## License: GPL 2
## Description: 1. 
rm( list = ls() )
setwd("~/Documents/Attention-Guidance-Visual-Search/")
require(ggdmc) ## version 0.2.6.8
load("data/visual_search.RData")

dplyr::tbl_df(e1)
e1$R <- factor( ifelse(e1$R == "L", "left", ifelse(e1$R == "R", "right", NA)))
d1 <- data.table::data.table(e1)
sapply(d1, levels)
d1[, .N, .(SS,Q,I,s)]
length(table(d1$s))
dat0 <- data.frame(d1)

## Note this data set did not test ISI factor, so we only compare the SS model
## with the priming model.

## Priming model ------------------
## v-c(SS, Q) 
## The I factor plays no role in influencing the drift rate, because the 
## target-repetition procedure results in pre-trial activations of the 
## attentional template, which does not operate in working memory.   
# load("data/e1hfit1.RData")
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
pop.mean1  <- c(a = 2,   v.3.F=3, v.5.F=3.5, v.7.F=4, v.9.F=5, 
                         v.3.V=2, v.5.V=2.5, v.7.V=3, v.9.V=4, z = .5, t0 = .2)
pop.scale1 <- c(a = .05, v.3.F =.15, v.5.F=.15, v.7.F = .15, v.9.F=.15,
                         v.3.V =.15, v.5.V=.15, v.7.V =.15,  v.9.V=.15, 
                z = .1, t0 = .05)
p.prior1 <- BuildPrior(
  dists = rep("tnorm", npar1),
  p1    = pop.mean1,
  p2    = pop.scale1*5,
  lower = c(0, rep(-5, 8), rep(0, 2)),
  upper = c(8, rep(10, 8),  3, 3))
mu.prior1 <- BuildPrior(
  dists = rep("tnorm", npar1),
  p1    = pop.mean1,
  p2    = pop.scale1*5,
  lower = c(0, rep(-5,8), rep(0, 2)),
  upper = c(8, rep(10,8), 3, 3))
sigma.prior1 <- BuildPrior(
  dists = rep("beta", npar1),
  p1    = rep(1, npar1),
  p2    = rep(1, npar1),
  upper = rep(NA, npar1))
names(sigma.prior1) <- GetPNames(model1)
hpriors1 <- list(pprior=p.prior1, location=mu.prior1, scale=sigma.prior1)

## 10 mins
# hburnin1 <- StartNewsamples(data=dmi1, prior=hpriors1, thin=2)
# hfit1 <- run(hburnin1, thin=2)
# save(hfit1, hburnin1, e1, dmi1, hpriors1, file = "data/modelling_data/e1hfit1.RData")
# rhat0 <- hgelman(hfit1)
# hyper    18     5    15    11     2    16     9    12    14     1     8     6     
# 1.05  1.03  1.04  1.04  1.04  1.04  1.05  1.05  1.05  1.05  1.05  1.06  1.06   
#    7   10    13    17    20    19     3     4 
# 1.06 1.07  1.07  1.07  1.08  1.09  1.09  1.19 


## SS Model -------------
## v-c(SS)
## The model assumes the only factor affecting the drift rate is the classic
## display size effect. It posits that neither Q nor I factor plays any role in
## influencing the drift rate. That is, the target-repetition procedure
## does not alter how one represents the attentional template.
# load("data/e1hfit2.RData")
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
pop.mean2  <- c(a = 2,   v.3=3,  v.5=3.5,  v.7=4, v.9=5, z = .5, t0 = .2)
pop.scale2 <- c(a = .05, v.3 =.15, v.5=.15, v.7 = .15, v.9=.15, z = .1, t0 = .05)
p.prior2 <- BuildPrior(
  dists = rep("tnorm", npar2),
  p1    = pop.mean2,
  p2    = pop.scale2*5,
  lower = c(0, rep(-5, 4), rep(0, 2)),
  upper = c(8, rep(10, 4),  3, 3))
mu.prior2 <- BuildPrior(
  dists = rep("tnorm", npar2),
  p1    = pop.mean2,
  p2    = pop.scale2*5,
  lower = c(0, rep(-5,4), rep(0, 2)),
  upper = c(8, rep(10,4), 3, 3))
sigma.prior2 <- BuildPrior(
  dists = rep("beta", npar2),
  p1    = rep(1, npar2),
  p2    = rep(1, npar2),
  upper = rep(NA, npar2))
names(sigma.prior2) <- GetPNames(model2)
hpriors2 <- list(pprior=p.prior2, location=mu.prior2, scale=sigma.prior2)

# hburnin2 <- StartNewsamples(data=dmi2, prior=hpriors2, thin=2)
# hfit2 <- run(hburnin2, thin=2)
# save(hfit2, hburnin2, e1, dmi2, hpriors2, file = "data/modelling_data/e1hfit2.RData")
# rhat0 <- hgelman(hfit2)
# hyper    11     9    10     1    18    16    12     8    13    15     5     3     
# 1.05   1.02  1.02  1.02  1.02  1.03  1.03  1.03  1.03  1.04  1.04  1.04  1.04   
# 6      14    17    20     2    19     7     4 
# 1.05 1.05  1.06  1.06  1.06  1.10  1.11  1.21 

## Model comparison ------------------------
load("data/modelling_data/e1hfit1.RData")
load("data/modelling_data/e1hfit2.RData")
plot(hfit1, hyper=TRUE)
plot(hfit1)
plot(hfit2, hyper = TRUE)
plot(hfit2)
dev1 <- DIC(hfit1, BPIC=TRUE)
dev2 <- DIC(hfit2, BPIC=TRUE)

