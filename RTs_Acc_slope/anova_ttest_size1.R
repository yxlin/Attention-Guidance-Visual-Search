#-------------------------------------------------------
# Author: Yishin Lin
# Date: 19 April, 2014
# Description: This script conducted ANOVA and post-hoc t tests
# at the display size 1.

# Load package, data and function----------------------------------
loadedPackages <-c("plyr", "car") 
suppressPackageStartupMessages(lapply(loadedPackages, require, 
                                      character.only=TRUE));
rm(list=ls()); 
load("./data/bu_td_nm_rt.RData")
source("./functions/summarise.R")
source("./functions/rm.anova.R")
source("./functions/eta2_cond.R")
source("./functions/massive.t.test.R")
avg_size1$cond <- factor(avg_size1$cond, levels=c('nm','td','bu'))

# Anova display size 1--------------------------------------------
rtfits1 <- rm.anova(avg_size1, measurevar='median', idvar='subj', 
                    withinvars=c('cond'))
accfits1 <- rm.anova(avg_size1, measurevar='cc', idvar='subj', 
                     withinvars=c('cond'))
eta2_cond(rtfits1) 
eta2_cond(accfits1)

#        SS     num Df  Error SS den Df        F    Pr(>F)    
# RT     26845      2      39600     18   6.1012  0.009486 ** 
# Acc    10         2     22.268     18   3.8878    0.0395 *  
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# partial eta^2
# RT     Accuracy 
# 0.302  0.404 

# T test at Median CRT --------------------------------
massive.t.test(avg_size1, "median")
massive.t.test(avg_size1, "cc")

# RT
# var1 var2 t.value df p.value
# nm   td    -3.115  9   0.012
# nm   bu    -3.427  9   0.008
# td   bu     0.949  9   0.367
# Accurary
# var1 var2 t.value df p.value
# nm   td     1.177  9   0.269
# nm   bu     2.577  9   0.030
# td   bu     1.593  9   0.146
# 
#           nm        td         bu 
# RT    341.25    413.00     390.00 
# Acc 98.92857  98.57143   97.58929 