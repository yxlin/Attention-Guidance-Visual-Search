## Disclaimer ---------------------------------------------------------------##
## Author:      Yi-Shin Lin
## Date:        7 May, 2019
## Description: From EZ2Run4.R and anovaDrift.R
rm(list = ls())
setwd("C:/Users/user/Documents/Fixed-Cue-vs-Varied-Cue/")
load('data/visual_search_2015.RData')
require(afex)
require(ggplot2)

# afex_options(emmeans_model = "multivariate") # use multivariate model for all follow-up tests.
rm( list=setdiff(ls(), c("feat1", "conj1", "two51")) )

# Load RT data ---------------------------------------
tibble::as_tibble(feat1)
length(levels(feat1$s))
## 19 * 2 * 4 * 2
# A tibble: 15,100 x 7
# s     S       SS    R       C2       RT C    
# <fct> <fct>   <fct> <fct>   <fct> <dbl> <lgl>
# d1    present 3     present hit   0.369 TRUE 
# d1    present 18    present hit   0.422 TRUE 
# d1    absent  3     absent  cr    0.354 TRUE 
# d1    present 6     present hit   0.508 TRUE 
# d1    absent  18    absent  cr    0.415 TRUE 
# d1    absent  12    absent  cr    0.496 TRUE 
# d1    present 12    present hit   0.484 TRUE 
# d1    absent  6     absent  cr    0.441 TRUE 
# d1    absent  3     absent  cr    0.592 TRUE 
# d1    present 3     present hit   0.335 TRUE 
# with 15,090 more rows

## Get correct MRT, VRT and Pc
## col <- c('s', 'S', 'C', 'SS')
## Pc; CJ and EACHI preserve the 0 counts
source('R/functions/ez2_2008.R')

avg0 <- GetEZData2015(feat1, fac = c('S', 'SS'))
avg1 <- GetEZData2015(conj1, fac = c('S', 'SS'))
avg2 <- GetEZData2015(two51, fac = c('S', 'SS'))
fit0 <- run2015(avg0, feat1)
fit1 <- run2015(avg1, conj1)
fit2 <- run2015(avg2, two51)

rm( list=setdiff(ls(), c("fit0", "fit1", "fit2", 'feat1', 'conj1', 'two51')) )

## Separate Anova ------------
source("R/functions/summarise.R")
fit0$SS <- factor(fit0$SS, levels=c('3','6','12', '18'),
                  labels=c('3','6','12', '18'))
fit1$SS <- factor(fit1$SS, levels=c('3','6','12', '18'),
                  labels=c('3','6','12', '18'))
fit2$SS <- factor(fit2$SS, levels=c('3','6','12', '18'),
                  labels=c('3','6','12', '18'))

sim0 <- sim.effect2015(fit0)
sim1 <- sim.effect2015(fit1)
sim2 <- sim.effect2015(fit2)
#         S SS  N     v    sd     a    sd    t0    sd
# 1  absent  3 19 0.393 0.064 0.091 0.012 0.310 0.022
# 2  absent  6 19 0.416 0.055 0.092 0.012 0.299 0.016
# 3  absent 12 19 0.438 0.050 0.094 0.012 0.296 0.016
# 4  absent 18 19 0.430 0.049 0.097 0.007 0.293 0.021
# 5 present  3 19 0.369 0.053 0.094 0.012 0.278 0.018
# 6 present  6 19 0.381 0.052 0.086 0.010 0.302 0.019
# 7 present 12 19 0.380 0.045 0.086 0.011 0.313 0.022
# 8 present 18 19 0.371 0.054 0.084 0.013 0.319 0.027

# Conj
#         S SS  N     v    sd     a    sd    t0    sd
# 1  absent  3 19 0.356 0.069 0.093 0.017 0.402 0.025
# 2  absent  6 19 0.360 0.050 0.109 0.017 0.408 0.035
# 3  absent 12 19 0.307 0.045 0.131 0.018 0.427 0.036
# 4  absent 18 19 0.263 0.073 0.144 0.020 0.454 0.073
# 5 present  3 19 0.335 0.044 0.096 0.015 0.357 0.033
# 6 present  6 19 0.302 0.038 0.102 0.014 0.378 0.037
# 7 present 12 19 0.247 0.053 0.111 0.016 0.399 0.037
# 8 present 18 19 0.220 0.073 0.105 0.014 0.442 0.037

# Two5
#         S SS  N     v    sd     a    sd    t0    sd
# 1  absent  3 20 0.292 0.035 0.135 0.026 0.469 0.055
# 2  absent  6 20 0.250 0.038 0.162 0.029 0.565 0.090
# 3  absent 12 20 0.214 0.043 0.201 0.021 0.661 0.083
# 4  absent 18 20 0.188 0.037 0.207 0.020 0.630 0.098
# 5 present  3 20 0.267 0.036 0.125 0.026 0.437 0.065
# 6 present  6 20 0.202 0.040 0.142 0.015 0.451 0.046
# 7 present 12 20 0.137 0.031 0.146 0.018 0.523 0.074
# 8 present 18 20 0.098 0.043 0.146 0.022 0.602 0.072

a  <- aov_ez("s", "a",  fit0, within = c("S", "SS"))
v  <- aov_ez("s", "v",  fit0, within = c("S", "SS"))
t0 <- aov_ez("s", "t0", fit0, within = c("S", "SS"))
nice(v,  correction = "GG", es = "pes")
nice(a,  correction = "GG", es = "pes")
nice(t0, correction = "GG", es = "pes")
# Anova Table (Type 3 tests)
#  Effect          df  MSE        F    pes p.value
#       S       1, 18 0.00     9.45    .34    .007 ** a
#       S       1, 18 0.00    15.36    .46    .001 ** v
#       S       1, 18 0.00     0.27    .01    .61    t0

#      SS 2.77, 49.81 0.00     0.52    .03     .65    a
#      SS 2.03, 36.59 0.00     1.78    .09     .18    v
#      SS 2.49, 44.78 0.00     5.66    .24    .004 ** t0 

#    S:SS 2.49, 44.80 0.00     3.96    .18     .02 *    v
#    S:SS 2.65, 47.63 0.00     1.44    .07     .25      a
#    S:SS 2.68, 48.27 0.00    27.98    .61  <.0001 *** t0
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1

# Wolfe et al's data 
# 
#   Effect          df   MSE      F  pes    p.value
# v      S        1, 8  0.01   2.03  .20     .19
# a      S        1, 8  0.00  14.62  .65    .005 **
# t0     S        1, 8  0.00   0.65  .07     .44

# v     SS 2.27, 18.19  0.00   0.34  .04     .74
# a     SS 2.69, 21.54  0.00   4.18  .34     .02 *
# t0    SS 1.90, 15.17  0.00   1.09  .12     .36

# v   S:SS 2.47, 19.75  0.00   4.60  .37     .02 *
# a   S:SS 1.89, 15.15  0.00   6.27 .44     .01 *
# t0  S:SS 2.14, 17.14  0.00    6.76  .46    .006 **

tibble::as_tibble(fit0)


(ss.effect <- summarySEwithin(dplyr::tbl_df(fit0), 
                              measurevar = 'v', 
                              withinvar   =c('SS', 'S'), 
                              idvar='s', 
                              na.rm = FALSE, conf.interval = .95, .drop=TRUE))

a  <- aov_ez("s", "a",  fit1, within = c("S", "SS"))
v  <- aov_ez("s", "v",  fit1, within = c("S", "SS"))
t0 <- aov_ez("s", "t0", fit1, within = c("S", "SS"))
nice(a,  correction = "GG", es = "pes")
nice(v,  correction = "GG", es = "pes")
nice(t0, correction = "GG", es = "pes")
# Anova Table (Type 3 tests)
# 
# Response: v
#   Effect          df  MSE         F pes p.value
# 1      S       1, 18 0.01   10.32  .36    .005 **
# 2     SS 2.04, 36.80 0.00   35.94  .67  <.0001 ***
# 3   S:SS 1.97, 35.52 0.00    1.14  .06     .33
# Response: a
# a     S       1, 18 0.00   23.66  .57   .0001 ***
# a     SS 2.21, 39.78 0.00   22.54  .56  <.0001 ***
# a   S:SS 2.71, 48.82 0.00   16.77  .48  <.0001 ***
# Response: t0
# 1      S       1, 18 0.01    5.87 .25     .03 *
# 2     SS 1.71, 30.79 0.00   27.68 .61  <.0001 ***
# 3   S:SS 2.22, 39.92 0.00    1.59 .08     .21
# ---

# Wolfe et al's data
# Anova Table (Type 3 tests)
# 
# Response: a
# Effect            df  MSE         F pes p.value
# a      S        1, 8 0.00 41.53 *** .84   .0002
# v      S        1, 8 0.00      1.19 .13     .31
# t0     S        1, 8 0.01    7.21 * .47     .03

# a     SS 1.54, 12.29 0.00 40.63 *** .84  <.0001
# v     SS 1.91, 15.29 0.00 85.97 *** .91  <.0001
# t0    SS 1.44, 11.50 0.00    6.52 * .45     .02

# a   S:SS 2.27, 18.13 0.00 16.71 *** .68  <.0001
# v   S:SS 2.04, 16.36 0.00      1.53 .16     .25
# t0  S:SS 1.81, 14.51 0.00      0.47 .06     .61


(ss.effect <- summarySEwithin(dplyr::tbl_df(fit1), 
                              measurevar = 'v', 
                              withinvar   =c('SS', 'S'), 
                              idvar='s', 
                              na.rm = FALSE, conf.interval = .95, .drop=TRUE))
#  SS       S  N         v         sd          se         ci
#   3  absent 19 0.3563091 0.06860865 0.015739905 0.03306831
#   3 present 19 0.3349219 0.04403786 0.010102979 0.02122557
#   6  absent 19 0.3596374 0.04961276 0.011381948 0.02391258
#   6 present 19 0.3022433 0.03823490 0.008771687 0.01842863
#  12  absent 19 0.3073646 0.04544749 0.010426369 0.02190499
#  12 present 19 0.2472561 0.05331094 0.012230369 0.02569505
#  18  absent 19 0.2628305 0.07307745 0.016765116 0.03522220
#  18 present 19 0.2198633 0.07300855 0.016749310 0.03518899

a  <- aov_ez("s", "a",  fit2, within = c("S", "SS"))
v  <- aov_ez("s", "v",  fit2, within = c("S", "SS"))
t0 <- aov_ez("s", "t0", fit2, within = c("S", "SS"))
nice(a,  correction = "GG", es = "pes")
nice(v,  correction = "GG", es = "pes")
nice(t0, correction = "GG", es = "pes")
#    Effect          df  MSE         F  pes p.value
# a       S       1, 19 0.00     64.05  .77  <.0001 ***
# v       S       1, 19 0.00     30.28  .61  <.0001 ***
# t0      S       1, 19 0.01     23.43  .55   .0001 ***
# a      SS 2.32, 44.14 0.00     36.86  .66  <.0001 ***
# v      SS 1.90, 36.19 0.00    131.84  .87  <.0001 ***
# t0     SS 2.49, 47.32 0.01     47.62  .71  <.0001 ***
# a    S:SS 2.44, 46.39 0.00     15.54  .45  <.0001 ***
# v    S:SS 1.87, 35.61 0.00     12.19  .39   .0001 ***
# t0   S:SS 1.91, 36.26 0.01      6.78  .26    .004 **

# Wolfe et al's data
# 
#   Effect      df   MSE         F   pes p.value
# a      S    1, 8  0.00     36.43   .82   .0003 ***
# v      S    1, 8  0.00      3.39   .30     .10
# t0     S    1, 8  0.02     11.16   .58     .01 *

# a     SS 1.53, 12.20 0.00  12.82 ** .62    .002
# v     SS 2.08, 16.63 0.00 153.74 *** .95  <.0001
# t0    SS 1.96, 15.68 0.00 40.78 *** .84  <.0001

# a   S:SS 2.16, 17.30 0.00 11.55 *** .59   .0005
# v   S:SS 1.76, 14.10 0.00  18.42 *** .70   .0002
# t0  S:SS 1.73, 13.82 0.01    4.70 * .37     .03


## Three-task Anova -------
fit0$TK <- 'F'
fit1$TK <- 'C'
fit2$TK <- 'S'
fit <- rbind(fit0, fit1, fit2)
fit$SS <- factor(fit$SS, levels=c('3','6','12', '18'),
                  labels=c('3','6','12', '18'))
fit$news <- paste(fit$s, fit$TK, sep='-')


a  <- aov_ez(id = "news", "a",  fit, within = c("S", "SS"), between = 'TK')
v  <- aov_ez(id = "news", "v",  fit, within = c("S", "SS"), between = 'TK')
t0 <- aov_ez(id = "news", "t0", fit, within = c("S", "SS"), between = 'TK')
nice(v,  correction = "GG", es = "pes")
nice(a,  correction = "GG", es = "pes")
nice(t0, correction = "GG", es = "pes")
# Anova Table (Type 3 tests)
# 
# Response: v
# Effect           df  MSE         F pes p.value
# 1      TK        2, 55 0.03 54.86 *** .67  <.0001
# 2       S        1, 55 0.01 50.48 *** .48  <.0001
# 3    TK:S        2, 55 0.01      0.56 .02     .58
# 4      SS 2.55, 140.39 0.00 58.09 *** .51  <.0001
# 5   TK:SS 5.10, 140.39 0.00 25.44 *** .48  <.0001
# 6    S:SS 2.28, 125.35 0.00   6.22 ** .10    .002
# 7 TK:S:SS 4.56, 125.35 0.00      0.90 .03     .47
# Response: a
# Effect           df  MSE         F pes p.value
# 1      TK        2, 55 0.00 48.49 *** .64  <.0001
# 2       S        1, 55 0.00 93.95 *** .63  <.0001
# 3    TK:S        2, 55 0.00 20.25 *** .42  <.0001
# 4      SS 2.52, 138.34 0.00 48.27 *** .47  <.0001
# 5   TK:SS 5.03, 138.34 0.00 15.79 *** .36  <.0001
# 6    S:SS 2.84, 156.15 0.00 32.43 *** .37  <.0001
# 7 TK:S:SS 5.68, 156.15 0.00   4.07 ** .13    .001
# ---
# Response: t0
# Effect           df  MSE          F pes p.value
# 1      TK        2, 55 0.02 113.33 *** .80  <.0001
# 2       S        1, 55 0.01  23.36 *** .30  <.0001
# 3    TK:S        2, 55 0.01  11.18 *** .29  <.0001
# 4      SS 2.47, 135.69 0.00  72.30 *** .57  <.0001
# 5   TK:SS 4.93, 135.69 0.00  24.48 *** .47  <.0001
# 6    S:SS 2.04, 112.38 0.00    6.00 ** .10    .003
# 7 TK:S:SS 4.09, 112.38 0.00   6.65 *** .19  <.0001
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
# 
# Sphericity correction method: GG 

## Wolfe et al's data
# Anova Table (Type 3 tests)
# 
# Response: v
# Effect          df  MSE          F pes p.value
# 1      TK       2, 24 0.03  31.62 *** .72  <.0001
# 2       S       1, 24 0.00     4.83 * .17     .04
# 3    TK:S       2, 24 0.00       0.62 .05     .54
# 4      SS 2.79, 66.85 0.00 123.15 *** .84  <.0001
# 5   TK:SS 5.57, 66.85 0.00  32.91 *** .73  <.0001
# 6    S:SS 2.40, 57.48 0.00  15.01 *** .38  <.0001
# 7 TK:S:SS 4.79, 57.48 0.00     2.89 * .19     .02
# ---
# Response: a
# Effect          df  MSE         F pes p.value
# 1      TK       2, 24 0.01 24.40 *** .67  <.0001
# 2       S       1, 24 0.00 88.06 *** .79  <.0001
# 3    TK:S       2, 24 0.00 10.12 *** .46   .0007
# 4      SS 1.59, 38.22 0.00 37.11 *** .61  <.0001
# 5   TK:SS 3.19, 38.22 0.00  8.78 *** .42   .0001
# 6    S:SS 2.58, 61.81 0.00 31.95 *** .57  <.0001
# 7 TK:S:SS 5.15, 61.81 0.00   3.69 ** .24    .005
# ---
# Response: t0
# Effect          df  MSE         F pes p.value
# 1      TK       2, 24 0.02 51.94 *** .81  <.0001
# 2       S       1, 24 0.01 18.88 *** .44   .0002
# 3    TK:S       2, 24 0.01    4.81 * .29     .02
# 4      SS 2.05, 49.20 0.00 43.74 *** .65  <.0001
# 5   TK:SS 4.10, 49.20 0.00 26.42 *** .69  <.0001
# 6    S:SS 2.08, 49.86 0.00    3.00 + .11     .06
# 7 TK:S:SS 4.15, 49.86 0.00   3.86 ** .24    .008
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
# 
# Sphericity correction method: GG 

