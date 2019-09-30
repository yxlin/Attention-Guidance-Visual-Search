#########################################################################0
## Fixed cue vs. varied cue
## Version: 0.0.3
## Authors: Yi-Shin Lin (yishinlin001@gmail.com)
## Date: 25 Sept, 2019 -- Draft one
## License: GPL 2
## Description: 1. 
rm( list = ls() )
setwd("~/Documents/Fixed-Cue-vs-Varied-Cue/")
require(ggdmc) ## version 0.2.6.8
load("data/alldata.RData")

dplyr::tbl_df(e2)
e2$R <- factor( ifelse(e2$R == "L", "left", ifelse(e2$R == "R", "right", NA)))
d2 <- data.table::data.table(e2)
sapply(d2, levels)
d2[, .N, .(SS,Q,I,s)]
length(table(d2$s))

## Offloading model -------------------
## v-c(SS, I, Q) 
## The I factor plays a role in influencing the drift rate, because the 
## attentional template must operate in working memory. 
load("data/e2hfit0.RData")

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
  p2    = c(.05, .23, .20, .18, .24, .18, .13, .15, .23, .19, .19, .20,
            .17, .07, .05),
  lower = c(0, rep(-5, 12), rep(0, 2)),
  upper = c(8, rep(10, 12),  3, 3))
mu.prior0 <- BuildPrior(
  dists = rep("tnorm", npar0),
  p1    = c(2, 4.01, 3.53, 2.93, 4.92, 4.50, 3.98, 2.08, 1.48, 1.03, 2.98,
            2.49, 2.04, .5, .19),
  p2    = c(.05, .23, .20, .18, .24, .18, .13, .15, .23, .19, .19, .20,
            .17, .07, .05),
  lower = c(0, rep(-5,12), rep(0, 2)),
  upper = c(8, rep(10,12), 3, 3))
sigma.prior0 <- BuildPrior(
  dists = rep("beta", npar0),
  p1    = rep(1, npar0),
  p2    = rep(1, npar0),
  upper = rep(NA, npar0))

names(p.prior0) <- GetPNames(model0)
names(mu.prior0) <- GetPNames(model0)
names(sigma.prior0) <- GetPNames(model0)
hpriors0 <- list(pprior=p.prior0, location=mu.prior0, scale=sigma.prior0)

# hburnin0 <- StartNewsamples(data=dmi0, prior=hpriors0)
# hfit0 <- run(hburnin0)
# hfit0 <- run(hfit0, thin=2)
# Start sampling: 100 200 300 400 500 
# save(hfit0, hburnin0, e2, dmi0, hpriors0, file = "data/e2hfit0.RData")

plot(hfit0, hyper = TRUE)
plot(hfit0)
hgelman(hfit0)
# hyper   10     3     18    20    14     7    21    13    16    12    19   
# 1.21  1.04  1.04   1.05  1.05  1.05  1.06  1.06  1.06  1.06  1.06  1.07
#    8   17     4     9    11    23    15 
# 1.07 1.07  1.08  1.08  1.09  1.09  1.10

## Priming model ------------------
## v-c(SS, Q) 
## The I factor plays no role in influencing the drift rate, because the 
## target-repetition procedure results in pre-trial activations of the 
## attentional template, which does not operate in working memory.   
load("data/e2hfit1.RData")

model1 <- BuildModel(
  p.map     = list(a = "1", v = c("SS", "Q"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9"), Q = c("F", "V")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar1 <- length(GetPNames(model1))
dmi1 <- BuildDMI(dat0, model1)

## We assume the following drift rates F > V and 3 > 7 > 9.
pop.mean1  <- c(a = 2,   v.3.F=3,  v.7.F=4, v.9.F=5, v.3.V=2,  v.7.V = 3,
               v.9.V=4, z = .5, t0 = .2)
pop.scale1 <- c(a = .05, v.3.F =.15, v.7.F = .15, v.9.F=.15, v.3.V =.15,
               v.7.V =.15, v.9.V=.15, z = .1, t0 = .05)
p.prior1 <- BuildPrior(
  dists = rep("tnorm", npar1),
  p1    = pop.mean1,
  p2    = pop.scale1*5,
  lower = c(0, rep(-5, 6), rep(0, 2)),
  upper = c(8, rep(10, 6),  3, 3))
mu.prior1 <- BuildPrior(
  dists = rep("tnorm", npar1),
  p1    = pop.mean1,
  p2    = pop.scale1*5,
  lower = c(0, rep(-5,6), rep(0, 2)),
  upper = c(8, rep(10,6), 3, 3))
sigma.prior1 <- BuildPrior(
  dists = rep("beta", npar1),
  p1    = rep(1, npar1),
  p2    = rep(1, npar1),
  upper = rep(NA, npar1))
names(sigma.prior1) <- GetPNames(model1)
hpriors1 <- list(pprior=p.prior1, location=mu.prior1, scale=sigma.prior1)

# hburnin1 <- StartNewsamples(data=dmi1, prior=hpriors1)
# hfit1 <- run(hburnin1)
# save(hfit1, hburnin1, fit1, burnin1, e2, dmi1, priors1, hpriors1,
#      file = "data/e2hfit1.RData")
rhat0 <- hgelman(hfit1)
# hyper   18    19    16    20    12     7    14     5    13    15    10    21      
# 1.09  1.04  1.06  1.07  1.07  1.07  1.08  1.08  1.08  1.08  1.09  1.09  1.09   
# 8      17     3     9    23    11     4 
# 1.09 1.10  1.10  1.10  1.12  1.14  1.15 
plot(hfit1, hyper=TRUE)
plot(hfit1)

## SS Model -------------
## v-c(SS)
## The model assumes the only factor affecting the drift rate is the classic
## display size effect. It posits that neither Q nor I factor plays any role in
## influencing the drift rate. That is, the target-repetition procedure
## does not alter how one represents the attentional template.
load("data/e2hfit2.RData")
model2 <- BuildModel(
  p.map     = list(a = "1", v = c("SS"), z = "1", d = "1", sz = "1",
                   sv = "1", t0 = "1", st0 = "1"),
  match.map = list(M = list(L = "left", R = "right")),
  factors   = list(S = c("L", "R"), SS =c("3","7","9")),
  constants = c(st0 = 0, sz = 0, sv = 0, d = 0),
  responses = c("left", "right"),
  type      = "rd")
npar2 <- length(GetPNames(model2))
dmi2 <- BuildDMI(dat0, model2)

## We assume the following drift rates 3 > 7 > 9.
pop.mean2  <- c(a = 2,   v.3=3,  v.7=4, v.9=5, z = .5, t0 = .2)
pop.scale2 <- c(a = .05, v.3 =.15, v.7 = .15, v.9=.15, z = .1, t0 = .05)
p.prior2 <- BuildPrior(
  dists = rep("tnorm", npar2),
  p1    = pop.mean2,
  p2    = pop.scale2*5,
  lower = c(0, rep(-5, 3), rep(0, 2)),
  upper = c(8, rep(10, 3),  3, 3))
mu.prior2 <- BuildPrior(
  dists = rep("tnorm", npar2),
  p1    = pop.mean2,
  p2    = pop.scale2*5,
  lower = c(0, rep(-5,3), rep(0, 2)),
  upper = c(8, rep(10,3), 3, 3))
sigma.prior2 <- BuildPrior(
  dists = rep("beta", npar2),
  p1    = rep(1, npar2),
  p2    = rep(1, npar2),
  upper = rep(NA, npar2))
names(sigma.prior2) <- GetPNames(model2)
hpriors2 <- list(pprior=p.prior2, location=mu.prior2, scale=sigma.prior2)
 
# hburnin2 <- StartNewsamples(data=dmi2, prior=hpriors2)
# hfit2 <- run(hburnin2)
# save(hfit2, hburnin2, fit2, burnin2, e2, dmi2, priors2, hpriors2,
#      file = "data/e2hfit2.RData")
rhat0 <- hgelman(hfit2)
# hyper   10    16    13    18    20    19    15    14     3     9    17    21      
# 1.06  1.03  1.04  1.04  1.04  1.04  1.05  1.05  1.06  1.06  1.06  1.06  1.06   
#    7    5    12     8     4    11    23 
# 1.07 1.07  1.08  1.08  1.09  1.09  1.11 
plot(hfit2, hyper = TRUE)
plot(hfit2)




## Compare models ----------
load("data/e2hfit0.RData")
load("data/e2hfit1.RData")
load("data/e2hfit2.RData")

dev0 <- DIC(hfit0, BPIC = TRUE)
dev1 <- DIC(hfit1, BPIC = TRUE)
dev2 <- DIC(hfit2, BPIC = TRUE)
