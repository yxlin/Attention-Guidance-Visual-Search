#########################################################################0
## Fixed cue vs. varied cue
## Version: 0.0.3
## Authors: Yi-Shin Lin (yishinlin001@gmail.com)
## Date: 07 May, 2019 -- Draft one
## License: GPL 2
## Description: 1. 
#########################################################################0
rm(list = ls())
setwd("~/Documents/Attention-Guidance-Visual-Search/")
## setwd("C:/Users/user/Documents/Attention-Guidance-Visual-Search/")
load("data/visual_search.RData")
loadedPackages <-c("plyr", "fBasics", "plotrix", "car", "afex",
                   "reshape2", "ggplot2", "grid", "data.table")
sapply(loadedPackages, require, character.only=TRUE)
source("R/functions/summarise.R")
source('R/functions/ez2_2008.R')

## Load RT data ---------------------------------------
tibble::as_tibble(e1)
length(levels(e1$s))

avg0 <- GetEZData(e1)
avg1 <- GetEZData(e2)
avg2 <- GetEZData(e3)
fit0 <- run(avg0, e1)
fit1 <- run(avg1, e2)
fit2 <- run(avg2, e3)

## Anova on EZ-2 diffusion
tibble::as_tibble(fit0)
tibble::as_tibble(fit1)
tibble::as_tibble(fit2)

cor.test(as.numeric(as.character(fit0$SS)), fit0$v)
cor.test(as.numeric(as.character(fit0$SS)), fit0$a)

cor.test(as.numeric(as.character(fit1$SS)), fit1$v)
cor.test(as.numeric(as.character(fit1$SS)), fit1$a)

cor.test(as.numeric(as.character(fit2$SS)), fit2$v)
cor.test(as.numeric(as.character(fit2$SS)), fit2$a)

cor.test(as.numeric(as.character(fit0$SS)), fit0$t0)
cor.test(as.numeric(as.character(fit1$SS)), fit1$t0)
cor.test(as.numeric(as.character(fit2$SS)), fit2$t0)

a0  <- aov_ez("s", "a",  fit0, within = c("Q", "SS"))
v0  <- aov_ez("s", "v",  fit0, within = c("Q", "SS"))
t00 <- aov_ez("s", "t0", fit0, within = c("Q", "SS"))

a1  <- aov_ez("s", "a",  fit1, within = c('I', "Q", "SS"))
v1  <- aov_ez("s", "v",  fit1, within = c('I', "Q", "SS"))
t01 <- aov_ez("s", "t0", fit1, within = c('I', "Q", "SS"))

a2  <- aov_ez("s", "a",  fit2, within = c('I', "Q", "SS"))
v2  <- aov_ez("s", "v",  fit2, within = c('I', "Q", "SS"))
t02 <- aov_ez("s", "t0", fit2, within = c('I', "Q", "SS"))

## Experiment 2 and 3, 50 ms
v1.1  <- aov_ez("s", "v",  fit1[fit1$I == '50', ], within = c("Q", "SS"))
v2.1  <- aov_ez("s", "v",  fit2[fit2$I == '50', ], within = c("Q", "SS"))
a1.1  <- aov_ez("s", "a",  fit1[fit1$I == '50', ], within = c("Q", "SS"))
a2.1  <- aov_ez("s", "a",  fit2[fit2$I == '50', ], within = c("Q", "SS"))
t01.1  <- aov_ez("s", "t0",  fit1[fit1$I == '50', ], within = c("Q", "SS"))
t02.1  <- aov_ez("s", "t0",  fit2[fit2$I == '50', ], within = c("Q", "SS"))

## Experiment 2 and 3, 400 ms
v1.2  <- aov_ez("s", "v",  fit1[fit1$I == '400', ], within = c("Q", "SS"))
v2.2  <- aov_ez("s", "v",  fit2[fit2$I == '400', ], within = c("Q", "SS"))
a1.2  <- aov_ez("s", "a",  fit1[fit1$I == '400', ], within = c("Q", "SS"))
a2.2  <- aov_ez("s", "a",  fit2[fit2$I == '400', ], within = c("Q", "SS"))
t01.2 <- aov_ez("s", "t0",  fit1[fit1$I == '400', ], within = c("Q", "SS"))
t02.2 <- aov_ez("s", "t0",  fit2[fit2$I == '400', ], within = c("Q", "SS"))

nice(v0,  correction = "GG", es = "pes")
nice(a0,  correction = "GG", es = "pes")
nice(t00,  correction = "GG", es = "pes")

nice(v2,  correction = "GG", es = "pes")
# Response: v, Exp 1
#  Effect          df  MSE          F   pes p.value       # Response: v
#       Q       1, 19 0.00      13.77   .42    .001 **
#      SS 2.29, 43.59 0.00     163.16   .90  <.0001 ***
#    Q:SS 2.32, 44.10 0.00       0.33   .02     .75
#       Q       1, 19 0.00       3.16   .14     .09 +     # Response: a
#      SS 1.84, 34.99 0.00     109.19   .85  <.0001 ***
#    Q:SS 2.59, 49.12 0.00       0.70   .04     .54
#       Q       1, 19 0.00       0.15  .008     .70       # Response: t0
#      SS 1.93, 36.66 0.00      11.19   .37   .0002 ***
#    Q:SS 2.30, 43.75 0.00       1.64   .08     .20

nice(v1,  correction = "GG", es = "pes")
nice(a1,  correction = "GG", es = "pes")
nice(t01, correction = "GG", es = "pes")
# Response: v, Exp 2
#  Effect          df  MSE          F     pes p.value   # Response: v
#       I       1, 18 0.00       0.00   .0002     .96
#       Q       1, 18 0.00       0.17    .010     .68
#      SS 1.13, 20.40 0.01     109.48     .86  <.0001 ***

#     I:Q       1, 18 0.00       2.25     .11     .15
#    I:SS 1.76, 31.61 0.00       4.25     .19     .03 *
#    Q:SS 1.49, 26.90 0.00       0.95     .05     .37
#  I:Q:SS 1.36, 24.50 0.00       2.42     .12     .12
# ------------------------------------------------------ #
#       I       1, 18 0.00       0.04   .002     .84    # Response: a
#       Q       1, 18 0.00       8.28    .32     .01 *
#      SS 1.67, 30.15 0.00      99.90    .85  <.0001 ***

#     I:Q       1, 18 0.00       4.50    .20     .05 *
#    I:SS 1.71, 30.71 0.00       0.08   .005     .90
#    Q:SS 1.77, 31.84 0.00       0.05   .003     .93
#  I:Q:SS 1.45, 26.10 0.00       0.08   .005     .86
# -------------------------------------------------------#
#       I       1, 18 0.00      10.47    .37    .005 **  # Response: t0
#       Q       1, 18 0.01      11.69    .39    .003 **
#      SS 1.70, 30.54 0.00      14.20    .44  <.0001 ***

#     I:Q       1, 18 0.00       3.29    .15     .09 +
#    I:SS 1.55, 27.94 0.00       1.48    .08     .24

#    Q:SS 1.47, 26.54 0.00       5.41    .23     .02 *  
#  I:Q:SS 1.62, 29.18 0.00       0.87    .05     .41


nice(v2,  correction = "GG", es = "pes")
nice(a2,  correction = "GG", es = "pes")
nice(t02, correction = "GG", es = "pes")
# Response: v, Exp 3
#  Effect          df  MSE          F   pes p.value  # Response: v
#       I       1, 19 0.00       2.23   .11     .15
#       Q       1, 19 0.00       2.46   .11     .13
#      SS 1.20, 22.80 0.01     199.94   .91  <.0001 ***

#     I:Q       1, 19 0.00       2.88   .13     .11

#    I:SS 1.70, 32.39 0.00       0.18  .009     .80

#    Q:SS 1.60, 30.45 0.00       0.04  .002     .93
#  I:Q:SS 1.52, 28.89 0.00       2.25   .11     .13

#       I       1, 19 0.00       4.12   .18     .06 + # Response: a
#       Q       1, 19 0.00       0.32   .02     .58
#      SS 1.40, 26.53 0.00      92.95   .83  <.0001 ***
#     I:Q       1, 19 0.00       0.15  .008     .70
#    I:SS 1.87, 35.50 0.00       0.58   .03     .55
#    Q:SS 1.68, 31.83 0.00       0.19   .01     .79
#  I:Q:SS 1.87, 35.44 0.00       0.14  .007     .85

#       I       1, 19 0.00      23.85   .56   .0001 *** # Response: t0
#       Q       1, 19 0.00       3.87   .17     .06 +
#      SS 1.57, 29.75 0.00       7.55   .28    .004 **
#     I:Q       1, 19 0.00       0.01 .0004     .94
#    I:SS 1.58, 30.06 0.00       0.65   .03     .50
#    Q:SS 1.95, 37.00 0.00       4.19   .18     .02 *
#  I:Q:SS 1.97, 37.37 0.00       0.27   .01     .76

nice(v1.1,  correction = "GG", es = "pes")
nice(v2.1,  correction = "GG", es = "pes")
nice(v1.2,  correction = "GG", es = "pes")
nice(v2.2,  correction = "GG", es = "pes")
# Response: v, Exp 2 50 ms  
# Effect          df  MSE           F  pes p.value
# 1      Q       1, 18 0.00      2.24  .11     .15
# 2     SS 1.32, 23.70 0.01     80.73  .82  <.0001 ***
# 3   Q:SS 1.74, 31.34 0.00      3.34  .16     .05 +

## v, Exp 3, 50 ms
#       Q       1, 19 0.00      3.39   .15     .08 +
#      SS 1.52, 28.97 0.00    146.07   .88  <.0001 ***
#    Q:SS 1.69, 32.15 0.00      0.99   .05     .37

# Response: v, Exp 2 400 ms
# 1      Q       1, 18 0.00      0.14  .008     .72
# 2     SS 1.29, 23.15 0.01     99.43   .85  <.0001 ***
# 3   Q:SS 1.47, 26.43 0.00      0.11  .006     .83

# Response: v, Exp 3 400 ms
#       Q       1, 19 0.00       1.04   .05     .32 # Response: v, Exp 3 400 ms
#      SS 1.18, 22.45 0.00     163.66   .90  <.0001 ***
#    Q:SS 1.56, 29.65 0.00       1.03   .05     .35

nice(a1.1,  correction = "GG", es = "pes")
nice(a2.1,  correction = "GG", es = "pes")
# Response: a, Exp 2, 50 ms
# Effect          df  MSE          F  pes p.value
#       Q       1, 18 0.00     12.68   .41    .002 **
#      SS 1.93, 34.73 0.00     82.92   .82  <.0001 ***
#    Q:SS 1.54, 27.74 0.00      0.03  .002     .94
#--------------------------#
# Response: a, Exp 3, 50 ms
#       Q       1, 19 0.00      0.65  .03     .43 
#      SS 1.67, 31.65 0.00     86.40  .82  <.0001 ***
#    Q:SS 1.93, 36.58 0.00      0.30  .02     .74
nice(a1.2,  correction = "GG", es = "pes")
nice(a2.2,  correction = "GG", es = "pes")
# Response: a, Exp 2 400 ms
# Effect          df  MSE           F   pes p.value
# 1      Q       1, 18 0.00      0.12  .007     .73
# 2     SS 1.81, 32.65 0.00     59.66   .77  <.0001 ***
# 3   Q:SS 1.61, 29.02 0.00      0.15  .008     .82
# Response: a, Exp 3 400 ms
# 1      Q       1, 19 0.00      0.08   .004     .78
# 2     SS 1.62, 30.70 0.00     61.65    .76  <.0001 ***
# 3   Q:SS 1.99, 37.82 0.00      0.04   .002     .96


nice(t01.1,  correction = "GG", es = "pes")
nice(t02.1,  correction = "GG", es = "pes")
nice(t01.2,  correction = "GG", es = "pes")
nice(t02.2,  correction = "GG", es = "pes")

# Response: Exp2, 50 ms, t0
#  Effect           df  MSE       F   pes p.value
#       Q       1, 18 0.00     8.02   .31     .01 *
#      SS 1.90, 34.17 0.00    10.83   .38   .0003 ***
#    Q:SS 1.62, 29.08 0.00     4.26   .19     .03 *

#       Q       1, 19 0.00     4.85   .20     .04 *  # Response: Exp3, 50, t0
#      SS 1.30, 24.74 0.00     5.72   .23     .02 *
#    Q:SS 1.43, 27.25 0.00     3.94   .17     .04 *

#       Q       1, 18 0.01    12.06   .40    .003 **  # Exp 2, 400 ms, t0
#      SS 1.82, 32.83 0.00    11.33   .39   .0003 ***
#    Q:SS 1.40, 25.23 0.00     3.90   .18     .05 *

#       Q       1, 19 0.00     1.96   .09     .18  # Response: t0, 400 ms Exp 3
#      SS 1.89, 35.99 0.00     4.95   .21     .01 *
#    Q:SS 1.85, 35.10 0.00     2.03   .10     .15
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
(ss.effect <- summarySEwithin(dplyr::tbl_df(fit0), 
                            measurevar = 'v', 
                            withinvar   =c('SS'), 
                            idvar='s', 
                            na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(ss.effect <- summarySEwithin(dplyr::tbl_df(fit0), 
                              measurevar = 'a', 
                              withinvar   =c('SS'), 
                              idvar='s', 
                              na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(ss.effect <- summarySEwithin(dplyr::tbl_df(fit0), 
                              measurevar = c('t0'), 
                              withinvar   =c('SS'), 
                              idvar='s', 
                              na.rm = FALSE, conf.interval = .95, .drop=TRUE))

## Exp 2
(ss.effect <- summarySEwithin(dplyr::tbl_df(fit1), 
                              measurevar = 'v', 
                              withinvar   =c('SS'), 
                              idvar='s', 
                              na.rm = FALSE, conf.interval = .95, .drop=TRUE))



(ss.effect <- summarySEwithin(dplyr::tbl_df(fit1), 
                              measurevar = 'a', 
                              withinvar   =c('Q', 'I'), 
                              idvar='s', 
                              na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(ss.effect <- summarySEwithin(dplyr::tbl_df(fit1), 
                              measurevar = c('t0'), 
                              withinvar   =c('SS'), 
                              idvar='s', 
                              na.rm = FALSE, conf.interval = .95, .drop=TRUE))

## Exp 3
(ss.effect <- summarySEwithin(dplyr::tbl_df(fit2), 
                              measurevar = c('t0'), 
                              withinvar   =c('SS'), 
                              idvar='s', 
                              na.rm = FALSE, conf.interval = .95, .drop=TRUE))


## I effect
(q50 <- summarySEwithin(dplyr::tbl_df(fit0), 
                        measurevar = 'v', 
                        withinvar   =c('Q'), 
                        idvar='s', 
                        na.rm = FALSE, conf.interval = .95, .drop=TRUE))


(qeffect <- summarySEwithin(dplyr::tbl_df(fit1), 
                        measurevar = 'v', 
                        withinvar   =c('Q'), 
                        idvar='s', 
                        na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(q50 <- summarySEwithin(dplyr::tbl_df(fit1[fit1$I =='50',]), 
                        measurevar = 'v', 
                        withinvar   =c('Q'), 
                        idvar='s', 
                        na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(q400 <- summarySEwithin(dplyr::tbl_df(fit1[fit1$I =='400',]), 
                         measurevar = 'v', 
                         withinvar   =c('Q'), 
                         idvar='s', 
                         na.rm = FALSE, conf.interval = .95, .drop=TRUE))

# Q  N         v        sd         se         ci
# 1 F 57 0.3225781 0.1109745 0.01469893 0.02944549
# 2 V 57 0.3125146 0.1341069 0.01776289 0.03558335
# Q  N         v        sd         se         ci
# 1 F 57 0.3159968 0.1384461 0.01833763 0.03673469
# 2 V 57 0.3198486 0.1355722 0.01795698 0.03597215
(q50 <- summarySEwithin(dplyr::tbl_df(fit2[fit2$I =='50',]), 
                        measurevar = 'v', 
                        withinvar   =c('Q'), 
                        idvar='s', 
                        na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(q400 <- summarySEwithin(dplyr::tbl_df(fit2[fit2$I =='400',]), 
                         measurevar = 'v', 
                         withinvar   =c('Q'), 
                         idvar='s', 
                         na.rm = FALSE, conf.interval = .95, .drop=TRUE))


(q50 <- summarySEwithin(dplyr::tbl_df(fit1[fit1$I =='50',]), 
                        measurevar = 'v', 
                        withinvar   =c('SS', 'Q'), 
                        idvar='s', 
                        na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(q400 <- summarySEwithin(dplyr::tbl_df(fit1[fit1$I =='400',]), 
                         measurevar = 'v', 
                         withinvar   =c('SS', 'Q'), 
                         idvar='s', 
                         na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(q50 <- summarySEwithin(dplyr::tbl_df(fit2[fit2$I =='50',]), 
                        measurevar = 'v', 
                        withinvar   =c('SS', 'Q'), 
                        idvar='s', 
                        na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(q400 <- summarySEwithin(dplyr::tbl_df(fit2[fit2$I =='400',]), 
                         measurevar = 'v', 
                         withinvar   =c('SS', 'Q'), 
                         idvar='s', 
                         na.rm = FALSE, conf.interval = .95, .drop=TRUE))

## The Q effect on the rate is driven by the short ISI.  When we conducted 
## simple effect on 50 and 400 ISI, we can see E2 and E3 simple Anova replicate
## E1's Anova result.  In contract, the simple ANOVA in E2 and E3 in 400 ISI 
## show the diminish effect of Q on the drift rate, supporting the argument
## that ISI plays a role in modulating the influence of Q factor on drift rates.

nice(a2,  correction = "GG", es = "pes")

a1.1  <- aov_ez("s", "a",  fit1[fit1$I == '50', ], within = c("Q", "SS"))
a2.1  <- aov_ez("s", "a",  fit2[fit2$I == '50', ], within = c("Q", "SS"))

a1.2  <- aov_ez("s", "a",  fit1[fit1$I == '400', ], within = c("Q", "SS"))
a2.2  <- aov_ez("s", "a",  fit2[fit2$I == '400', ], within = c("Q", "SS"))

nice(a1.1,  correction = "GG", es = "ges")
nice(a2.1,  correction = "GG", es = "ges")

nice(a1.2,  correction = "GG", es = "ges")
nice(a2.2,  correction = "GG", es = "ges")

## Q factor under 50 ms exerts stronger effect on boundary than on the rate
## The SS factor argue that our three replication studies demonstrate typical
## guidied search results.

nice(t00,  correction = "GG", es = "ges")
nice(t01,  correction = "GG", es = "ges")
nice(t02,  correction = "GG", es = "ges")



## Marginal means
d0 <- data.table::data.table(fit0)
d1 <- data.table::data.table(fit1)
d2 <- data.table::data.table(fit2)

d0[, .(mean_v  = round(mean(v), 2),
      mean_a  = round(mean(a), 2),
      mean_t0 = round(mean(t0),2) ), .(Q, SS)]

d1[, .(mean_v  = round(mean(v), 2),
       mean_a  = round(mean(a), 2),
       mean_t0 = round(mean(t0),2) ), .(I, Q, SS)]

d2[, .(mean_v  = round(mean(v), 2),
       mean_a  = round(mean(a), 2),
       mean_t0 = round(mean(t0),2) ), .(I, Q, SS)]

