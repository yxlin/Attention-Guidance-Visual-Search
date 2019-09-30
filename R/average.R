#########################################################################0
## Fixed cue vs. varied cue
## Version: 0.0.3
## Authors: Yi-Shin Lin (yishinlin001@gmail.com)
## Date: 01 Oct, 2018 -- Draft one
## License: GPL 2
## Description: 1. 
rm(list = ls())
setwd("~/Documents/Attention-Guidance-Visual-Search/")
## setwd("C:/Users/user/Documents/Attention-Guidance-Visual-Search/")
loadedPackages <-c("plyr", "fBasics", "plotrix", "car", "afex",
                   "reshape2", "ggplot2", "grid", "data.table")
sapply(loadedPackages, require, character.only=TRUE)
source("R/functions/summarise.R")
load("data/visual_search.RData")

## N --------------
d <- data.table::data.table(allexp)
d[, .N, .(E, I, Q, SS, s)]
d[, .N, .(s)]

## E1, per-condition trial:
## ns = 20; n trial = 130
d[E == "E1", .N, .(I, Q, SS, s)]
ns(d, "E1")

## E2, per-condition trial:
## ns = 19; n trial = 100
d[E == "E2", .N, .(I, Q, SS, s)]
ns(d, "E2")

## E3, per-condition trial:
## ns = 20; n trial = 200
d[E == "E3", .N, .(I, Q, SS, s)]
ns(d, "E3")

sapply(e1, levels)
sapply(e2, levels)
sapply(e3, levels)
dplyr::tbl_df(allexp)

## Search slope ------------
res <- d[, .(value = get.slope(RT, ss)), .(E, C, I, Q, s)]
res <- res[!is.na(value)]
crs <- res[C==T]
ers <- res[C==F]
slopea <- aov_ez("s", "value", crs[I=="50"], between = "E", within = c("Q"))
slope1 <- aov_ez("s", "value", crs[E=="E1"], within = c("Q"))
slope2 <- aov_ez("s", "value", crs[E=="E2"], within = c("Q", "I"))
slope3 <- aov_ez("s", "value", crs[E=="E3"], within = c("Q", "I"))
nice(slopea, correction = "GG", es = "pes")
nice(slope1, correction = "GG", es = "pes")
nice(slope2, correction = "GG", es = "pes")
nice(slope3, correction = "GG", es = "pes")
# Response: all three experiments
#   Effect    df  MSE       F  pes p.value
# 1      E 2, 56 0.00    5.35  .16    .008 **
# 2      Q 1, 56 0.00    0.74  .01     .39
# 3    E:Q 2, 56 0.00    5.32  .16    .008 **
# ----------------------------------------- Response: E1 slope
# 1      Q 1, 19 0.00   7.55    .28     .01 *
# ----------------------------------------- Response: E2 slope 
# 1      Q 1, 18 0.00    9.12   .34    .007 **
# 2      I 1, 18 0.00    1.04   .05     .32
# 3    Q:I 1, 18 0.00    0.57   .03     .46
# ----------------------------------------- Response: E3 slope
# 1      Q 1, 19 0.00   1.07    .05     .31
# 2      I 1, 19 0.00   1.43    .07     .25
# 3    Q:I 1, 19 0.00   3.33    .15     .08 +
# ---

(slope.effect <- summarySEwithin(dplyr::tbl_df(crs[I=='50' & E =='E1']), 
                            measurevar = 'value', 
                            withinvar   =c('Q'), 
                            idvar='s', 
                            na.rm = FALSE, conf.interval = .95, .drop=TRUE))
#   Q  N      value          sd          se          ci
# 1 F 20 0.03240150 0.005300561 0.001185242 0.002480739
# 2 V 20 0.03700806 0.005300561 0.001185242 0.002480739

slope2.tmp <- aov_ez("s", "value", crs[E=="E2" & I == '50'], within = c('Q'))
nice(slope2.tmp, correction = "GG", es = "pes")

(slope.effect <- summarySEwithin(dplyr::tbl_df(crs[E =='E2']), 
                                 measurevar = 'value', 
                                 withinvar   =c('Q', 'I'), 
                                 idvar='s', 
                                 na.rm = FALSE, conf.interval = .95, .drop=TRUE))
#   Q   I  N      value          sd          se          ci
# 1 F  50 19 0.04887328 0.006983073 0.001602027 0.003365733
# 2 F 400 19 0.05075708 0.005337257 0.001224451 0.002572476
# 3 V  50 19 0.04405887 0.007247227 0.001662628 0.003493051
# 4 V 400 19 0.04408143 0.004762098 0.001092500 0.002295258
(slope.effect <- summarySEwithin(dplyr::tbl_df(crs[E =='E2']), 
                                 measurevar = 'value', 
                                 withinvar   =c('Q'), 
                                 idvar='s', 
                                 na.rm = FALSE, conf.interval = .95, .drop=TRUE))


(slope.effect <- summarySEwithin(dplyr::tbl_df(crs[E =='E3']), 
                                 measurevar = 'value', 
                                 withinvar   =c('Q', 'I'), 
                                 idvar='s', 
                                 na.rm = FALSE, conf.interval = .95, .drop=TRUE))
# Q   I  N      value          sd           se          ci
# 1 F  50 20 0.03507989 0.006332511 0.0014159925 0.002963706
# 2 F 400 20 0.03322775 0.003595636 0.0008040086 0.001682809
# 3 V  50 20 0.03206182 0.004923777 0.0011009899 0.002304398
# 4 V 400 20 0.03242046 0.005807034 0.0012984922 0.002717775

## Avg across ----------
mrt  <- d[, .(value = median(RT)), .(C, E, I, Q, SS, s)]
q1rt <- d[, .(value = quantile(RT, .1)), .(C, E, I, Q, SS, s)]
q9rt <- d[, .(value = quantile(RT, .9)), .(C, E, I, Q, SS, s)]
crt <- mrt[C == TRUE]
ert <- mrt[C != TRUE]
cq1 <- q1rt[C == TRUE]
eq1 <- q1rt[C != TRUE]
cq9 <- q9rt[C == TRUE]
eq9 <- q9rt[C != TRUE]

crt[E == "E1"]
cq1[E == "E1"]
cq9[E == "E1"]
crt[E == "E2"]
cq1[E == "E2"]
cq9[E == "E2"]
crt[E == "E3"]
cq1[E == "E3"]
cq9[E == "E3"]

# 20*4*2
# 19*3*2*2
# 20*3*2*2

pro <- d[, .N, .(C, E, I, Q, SS, s)]
pro[, NN := sum(N), .(E, I, Q, SS, s)]
pro[, value := N/NN]
cp <- pro[C == TRUE] ## correct percentage
ep <- pro[C != TRUE] ## error percentage

dplyr::tbl_df(crt)
dplyr::tbl_df(ert)


(cvg <- summarySEwithin(dplyr::tbl_df(crt), 
                        measurevar = 'value', 
                        withinvar   =c('I', 'Q','SS'), 
                        betweenvars = 'E',
                        idvar='s', 
                        na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(c1 <- summarySEwithin(dplyr::tbl_df(cq1), 
                        measurevar = 'value', 
                        withinvar   =c('I', 'Q','SS'), 
                        betweenvars = 'E',
                        idvar='s', 
                        na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(c9 <- summarySEwithin(dplyr::tbl_df(cq9), 
                        measurevar = 'value', 
                        withinvar   =c('I', 'Q','SS'), 
                        betweenvars = 'E',
                        idvar='s', 
                        na.rm = FALSE, conf.interval = .95, .drop=TRUE))

(evg <- summarySEwithin(dplyr::tbl_df(ert), 
                        measurevar = 'value', 
                        withinvar  = c('I', 'Q','SS'), 
                        betweenvar = 'E', 
                        idvar      = 's', 
                        na.rm      = FALSE, conf.interval=.95, .drop=TRUE))

(arg <- summarySEwithin(dplyr::tbl_df(cp), 
                        measurevar = 'value', 
                        withinvar  = c('I', 'Q','SS'), 
                        betweenvar = 'E', 
                        idvar      = 's', 
                        na.rm      = FALSE, conf.interval=.95, .drop=TRUE))

(erg <- summarySEwithin(dplyr::tbl_df(ep), 
                        measurevar = 'value', 
                        withinvar  = c('I', 'Q','SS'), 
                        betweenvar = 'E', 
                        idvar      = 's', 
                        na.rm      = FALSE, conf.interval=.95, .drop=TRUE))

dplyr::tbl_df(cvg)
dplyr::tbl_df(arg)
cvg$P <- "50% RT"
c1$P <- "10% RT"
c9$P <- "90% RT"
cvg$PN <- "Correct median RT (s)"
c1$PN <- "Correct median RT (s)"
c9$PN <- "Correct median RT (s)"
arg$PN <- "Accuracy" 
arg$P <- "Accuracy" 
gvg <- rbind(cvg, c1, c9, arg)

textdf <- data.frame(x=c(4, 4, 4), y=c(.9, .58, .4), 
                     lab = c("90%", "50%", "10%"),
                PN = rep("Correct median RT (s)", 3),
                QI = c("F5010% RT", "F5050% RT", "F5090% RT"),
                EI = rep("E1-50", 3) )

gvg$EI <- factor(paste(gvg$E, gvg$I, sep = "-"), 
                 levels = c('E1-50', 'E2-50', 'E3-50', 'E2-400', 'E3-400'))
gvg$QI <- paste0(gvg$Q, gvg$I, gvg$P)
levels(gvg$EI)

p <- ggplot(gvg, aes(x = SS, y = value, group = QI)) +
  geom_ribbon(aes(ymin = value-se, ymax = value+se, 
                  fill = Q, colour = Q), alpha=.2 ) + 
  geom_line(aes(colour = Q, linetype = Q), size=1.5) + 
  geom_point(aes(shape = Q, colour = Q), size=4) +
  facet_grid(PN ~ EI, scales="free_y", switch = "y") + 
  geom_text(aes(x, y, label = lab), size=5.5, data = textdf) +
  theme_bw() +
  scale_x_discrete(name = "Display Size")+
  scale_colour_grey(start=.2, end=.7) +
  scale_fill_grey(start=.2, end=.7) +
  scale_shape_manual(values=c(16,17,2)) +
  theme(axis.title.x = element_text(size=26), 
        axis.text.x  = element_text(size=26,colour="grey30"), 
        axis.title.y = element_blank(), 
        axis.text.y  = element_text(size=26,colour="grey30"),
        strip.text.x = element_text(size=26, angle=0),  
        strip.text.y = element_text(size=26, angle=90),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position=  c(.08, .93),   
        legend.title = element_blank(), 
        legend.text = element_text(size=26),
        legend.key.size = unit(2.0, "lines"),
        legend.key.width = unit(1.8, "cm"))


# png(filename = "figs/beh.png", width = 1024, height = 768)
print(p)
# dev.off()

## Anova E1, E2 & E3 CRT----
dplyr::tbl_df(crt)
dplyr::tbl_df(cp)
levels(crt$E)

crt1 <- aov_ez("s", "value", crt[E == "E1"], within = c("Q", "SS"))
crt2 <- aov_ez("s", "value", crt[E == "E2"], within = c("I", "Q", "SS"))
crt3 <- aov_ez("s", "value", crt[E == "E3"], within = c("I", "Q", "SS"))
acc1 <- aov_ez("s", "value", cp[E == "E1"], within = c("Q", "SS"))
acc2 <- aov_ez("s", "value", cp[E == "E2"], within = c("I", "Q", "SS"))
acc3 <- aov_ez("s", "value", cp[E == "E3"], within = c("I", "Q", "SS"))

nice(crt1, correction = "GG", es = "pes")
nice(acc1, correction = "GG", es = "pes")

# E1 
# Response: Correct median RTS and correct rates
# Effect          df  MSE          F  pes p.value
#      Q       1, 19 0.00       7.10  .27     .02 *
#     SS 1.15, 21.88 0.00     158.86  .89  <.0001 ***
#   Q:SS 2.15, 40.80 0.00       3.30  .15     .04 *
## -------------------------------------------------##
#      Q       1, 19 0.00       3.56  .16     .07 +
#     SS 2.33, 44.34 0.00       5.55  .23    .005 **
#   Q:SS 2.30, 43.76 0.00       0.40  .02     .70
(qeffect <- summarySEwithin(dplyr::tbl_df(crt), 
                            measurevar = 'value', 
                            withinvar   =c('Q'), 
                            betweenvars = 'E',
                            idvar='s', 
                            na.rm = FALSE, conf.interval = .95, .drop=TRUE))
# E  Q   N value    sd         se         ci
# E1 F  80  0.54 0.099 0.01106133 0.02201703
# E1 V  80  0.56 0.109 0.01217248 0.02422871
# E2 F 114  0.57 0.191 0.01784366 0.03535151
# E2 V 114  0.52 0.158 0.01477369 0.02926934
# E3 F 120  0.55 0.127 0.01162537 0.02301940
# E3 V 120  0.54 0.116 0.01059044 0.02097012

round(qeffect$value, 2)
diff(qeffect$value[1:2]) ## E1
diff(qeffect$value[3:4]) ## E2
diff(qeffect$value[5:6])
(.560 - .537) * 1000
(.574 - .523) * 1000

(sseffect <- summarySEwithin(dplyr::tbl_df(crt), 
                             measurevar = 'value', 
                             withinvar   =c('SS'), 
                             betweenvars = 'E',
                             idvar='s', 
                             na.rm = FALSE, conf.interval = .95, .drop=TRUE))
round(sseffect$value, 2)

(qandss <- summarySEwithin(dplyr::tbl_df(crt), 
                            measurevar = 'value', 
                            withinvar   =c('Q', 'SS'), 
                            betweenvars = 'E',
                            idvar='s', 
                            na.rm = FALSE, conf.interval = .95, .drop=TRUE))

dqss <- data.table::data.table(qandss)

dqss[E == 'E1' & Q == 'F', 'value'] -
dqss[E == 'E1' & Q == 'V', 'value']
#       value
# 1: -0.008575
# 2: -0.023475
# 3: -0.033300
# 4: -0.029525


t.test(value~Q, data = cp[E == "E1" & SS == '7'], paired = TRUE)
t.test(value~Q, data = cp[E == "E1" & SS == '9'], paired = TRUE)
(qandss <- summarySEwithin(dplyr::tbl_df(cp[E == "E1" & SS == '9']), 
                           measurevar = 'value', 
                           withinvar   =c('Q', 'SS'), 
                           betweenvars = 'E',
                           idvar='s', 
                           na.rm = FALSE, conf.interval = .95, .drop=TRUE))
# Paired t-test
# 
# data:  value by Q
# t = 2.2335, df = 19, p-value = 0.03774
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.000460842 0.014192025
# sample estimates:
#   mean of the differences 
# 0.007326433 

## Experiment 2  ## 
nice(crt2, correction = "GG", es = "pes")
nice(acc2, correction = "GG", es = "pes")
# Response: correct median RT and accuracy 
#   Effect          df  MSE          F   pes p.value
# 1      I       1, 18 0.00   10.23 **   .36    .005
# 2      Q       1, 18 0.01   12.22 **   .40    .003
# 3     SS 1.06, 19.06 0.02 113.48 ***   .86  <.0001
# 4    I:Q       1, 18 0.00       1.75   .09     .20
# 5   I:SS 1.64, 29.51 0.00       1.53   .08     .23
# 6   Q:SS 1.40, 25.28 0.00    9.20 **   .34    .003
# 7 I:Q:SS 1.40, 25.22 0.00       0.81   .04     .42
# Response: value
#   Effect          df  MSE        F pes p.value
# 1      I       1, 18 0.00     1.07 .06     .31
# 2      Q       1, 18 0.00   7.71 * .30     .01
# 3     SS 1.41, 25.43 0.00   6.09 * .25     .01
# 4    I:Q       1, 18 0.00 10.09 ** .36    .005
# 5   I:SS 1.60, 28.73 0.00   3.65 * .17     .05
# 6   Q:SS 1.85, 33.30 0.00     1.01 .05     .37
# 7 I:Q:SS 1.50, 26.91 0.00     2.22 .11     .14
# ---
(qeffect <- summarySEwithin(dplyr::tbl_df(crt), 
                            measurevar = 'value', 
                            withinvar   =c('Q'), 
                            betweenvars = 'E',
                            idvar='s', 
                            na.rm = FALSE, conf.interval = .95, .drop=TRUE))


## Experiment 3 ## 
nice(crt3, correction = "GG", es = "pes")
nice(acc3, correction = "GG", es = "pes")
# E3 (Type 3 ANOVA tests)
# 
# Response: Correct mean RTs and accuracy
#  Effect          df  MSE          F pes p.value
#      I       1, 19 0.00       6.08 .24     .02 *
#      Q       1, 19 0.01       0.84 .04     .37
#     SS 1.08, 20.50 0.01     113.23 .86  <.0001 ***
#    I:Q       1, 19 0.00       1.75 .08     .20
#   I:SS 1.68, 31.90 0.00       0.21 .01     .77
#   Q:SS 1.43, 27.16 0.00       1.97 .09     .17
# I:Q:SS 1.45, 27.61 0.00       1.91 .09     .17
# ------------------------------------------------#
#     I       1, 19  0.00      1.22  .06     .28
#     Q       1, 19  0.00      4.30  .18     .05 +
#    SS  1.29, 24.47 0.00     13.66  .42   .0005 ***
#   I:Q       1, 19  0.00      3.07  .14     .10 +
#  I:SS  1.67, 31.70 0.00      1.26  .06     .29
#  Q:SS  1.70, 32.32 0.00      0.62  .03     .52
# I:Q:SS 1.94, 36.77 0.00      2.89  .13     .07 +

# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '+' 0.1 ' ' 1
# Sphericity correction method: GG 

## Next ez2.R 