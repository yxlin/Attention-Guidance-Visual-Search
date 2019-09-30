#########################################################################0
## Fixed cue vs. varied cue
## Version: 0.0.3
## Authors: Yi-Shin Lin (yishinlin001@gmail.com)
## Date: 25 Sept, 2019 -- Draft one
## License: GPL 2
## Description: 1. 
rm( list = ls() )
## setwd("C:/Users/yslin/Documents/Fixed-Cue-vs-Varied-Cue/")
setwd("~/Documents/Fixed-Cue-vs-Varied-Cue/")
load("data/alldata.RData")
require(ggdmc) ## version 0.2.6.8

dplyr::tbl_df(e3)
e3$R <- factor( ifelse(e3$R == "L", "left", ifelse(e3$R == "R", "right", NA)))
d3 <- data.table::data.table(e3)
sapply(d3, levels)
d3[, .N, .(SS,Q,I,s)]
length(table(d3$s))

## Offloading model -------------------
## v-c(SS, I, Q) 
## The I factor plays a role in influencing the drift rate, because the 
## attentional template must operate in working memory. 
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
dat0 <- data.frame(d3)
dmi0 <- BuildDMI(dat0, model0)

## We assume the following drift rates 400.F > 50.F > 400.V > 50.V, and
## SS3 > SS7 > SS9
pop.mean0  <- c(a = 2, v.3.50.F = 4,  v.7.50.F = 3.5,  v.9.50.F = 3,
                      v.3.400.F = 5, v.7.400.F = 4.5, v.9.400.F = 4,
                      v.3.50.V = 2,  v.7.50.V = 1.5,  v.9.50.V = 1,
                      v.3.400.V = 3, v.7.400.V = 2.5,  v.9.400.V = 2,
               z = .5, t0 = .2)
## We assume small but homogeneous variabilities
pop.scale0 <- c(a = .05, v.3.50.F = .15,  v.7.50.F = .15,  v.9.50.F = .15,
                        v.3.400.F = .15, v.7.400.F = .15, v.9.400.F = .15,
                        v.3.50.V = .15,  v.7.50.V = .15,  v.9.50.V = .15,
                        v.3.400.V = .15, v.7.400.V = .15,  v.9.400.V = .15,
               z = .1, t0 = .05)

p.prior0 <- BuildPrior(
  dists = rep("tnorm", npar0),
  p1    = pop.mean0,
  p2    = pop.scale0*5,
  lower = c(0, rep(-5, 12), rep(0, 2)),
  upper = c(8, rep(10, 12),  3, 3))
mu.prior0 <- BuildPrior(
  dists = rep("tnorm", npar0),
  p1    = pop.mean0,
  p2    = pop.scale0*5,
  lower = c(0, rep(-5,12), rep(0, 2)),
  upper = c(8, rep(10,12), 3, 3))
sigma.prior0 <- BuildPrior(
  dists = rep("beta", npar0),
  p1    = rep(1, npar0),
  p2    = rep(1, npar0),
  upper = rep(NA, npar0))
names(sigma.prior0) <- GetPNames(model0)
hpriors0 <- list(pprior=p.prior0, location=mu.prior0, scale=sigma.prior0)

# hburnin0 <- StartNewsamples(data=dmi0, prior=hpriors0)
# hfit0 <- run(hburnin0)
# save(hfit0, hburnin0, fit0, burnin0, e2, dmi0, priors0, hpriors0,
#      file = "data/e3hfit0.RData")
load("data/e3hfit0.RData")
rhat0 <- hgelman(hfit0)
# hyper   26    30    36    27    32    40    41    39    31    43    37    24    
# 1.06  1.04  1.04  1.05  1.05  1.05  1.05  1.06  1.06  1.06  1.06  1.06  1.07  
#   33   49    34    38    45    42    46    47 
# 1.07 1.07  1.08  1.09  1.09  1.11  1.14  1.16 
plot(hfit0, hyper=TRUE)
plot(hfit0)

## Priming model ------------------
## v-c(SS, Q) 
## The I factor plays no role in influencing the drift rate, because the 
## target-repetition procedure results in pre-trial activations of the 
## attentional template, which does not operate in working memory.   
model1 <- BuildModel (
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
# save(hfit1, hburnin1, fit1, burnin1, e3, dmi1, priors1, hpriors1,
#      file = "data/e3hfit1.RData")

load("data/e3hfit1.RData")
rhat0 <- hgelman(hfit1)
# hyper    39    30    27    36    31    26    32    24    33    37    49    40    
# 1.04  1.02  1.03  1.03  1.03  1.04  1.04  1.04  1.04  1.04  1.04  1.05  1.05   
# 41     46    45    43    34    42    47    38 
# 1.05 1.06  1.06  1.06  1.06  1.08  1.08  1.25 
plot(hfit1, hyper=TRUE)
plot(hfit1)

## SS Model -------------
## v-c(SS) 
## The model assumes the only factor affecting the drift rate is the classic 
## display size effect. It posits that neither Q nor I factor plays any role in
## influencing the drift rate. That is, the target-repetition procedure 
## does not alter how one represents the attentional template.   
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
# save(hfit2, hburnin2, e3, dmi2, hpriors2, file = "data/e3hfit2.RData")


load("data/e3hfit2.RData")
rhat0 <- hgelman(hfit2)
# hyper   27    45    49    36    40    41    37    32    30    39    38    24    
# 1.05  1.04  1.04  1.05  1.05  1.06  1.06  1.06  1.06  1.06  1.07  1.07  1.08  
# 31     33    26    46    34    42    43    47 
# 1.08 1.08  1.09  1.11  1.13  1.14  1.17  1.21 

plot(hfit2, hyper=TRUE)
plot(hfit2)
plot(hfit2, hyper=TRUE, pll=FALSE, den=TRUE)


## Compare models ----------
load("data/e3hfit0.RData")
load("data/e3hfit1.RData")
load("data/e3hfit2.RData")
dev0 <- DIC(hfit0, BPIC = TRUE)
dev1 <- DIC(hfit1, BPIC = TRUE)
dev2 <- DIC(hfit2, BPIC = TRUE)

