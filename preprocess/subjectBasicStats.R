#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        19 April, 2014
# Description: Calculate participants' demongraphic information
rm(list=ls())
bg <- read.csv("./data/subjBkInfo.csv", header=TRUE)

# how many participants took part in the study?----------
length(unique(bg$subj)) # 10

# sex and handedness ratio; mean age
# Age Mean = 19.4 
# sex   h    
# f:8   l:1  
# m:2   r:9  
summary(bg); 

# standard deviation of age 
sd(bg$age) #0.5163978
