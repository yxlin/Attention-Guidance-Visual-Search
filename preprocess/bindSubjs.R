#-------------------------------------------------------
# Author:      Yishin Lin
# Date:        19 April, 2014
# Description: Read E-Prime data files and aggregate all data files
# as one R image 

# Load Package  --------------------------------------------------
loadedPackages <-c("plyr", "car", "ggplot2", "grid") 
suppressPackageStartupMessages(lapply(loadedPackages, require, 
                                      character.only=TRUE));
rm(list=ls()); 
source("./functions/cleanData2.R")

# Testing Sequence ------------------------------------------------
# d43 321 
# d44 231
# d48 231
# d49 132
# d53 123
# d57 123
# d58 213
# d59 312
# d62 132
# d60 213

# Set-up Data Path-------------------------------------------------
v1files <- paste0("./data/rawData/d", c(43,44,48,49,53,57,58,59,62,60),"/visit1/", "d", 
                  c(43,44,48,49,53,57,58,59,62,60), "_1.txt")
v2files <- paste0("./data//rawData/d", c(43,44,48,49,53,57,58,59,62,60),"/visit2/", "d",
                  c(43,44,48,49,53,57,58,59,62,60), "_2.txt")
v3files <- paste0("./data//rawData/d", c(43,44,48,49,53,57,58,59,62,60),"/visit3/", "d",
                  c(43,44,48,49,53,57,58,59,62,60), "_3.txt")
n <- length(v1files)

testSeq <- matrix(c("bu","td","nm", "td","bu","nm", "td","bu","nm",
                    "nm","bu","td", "nm","td","bu", "nm","td","bu",
                    "td","nm","bu", "bu","nm","td", "nm","bu","td",
                    "td", "nm", "bu"), ncol=3,byrow=T)

# Load Data ---------------------------------------------------------
x0 <- NULL
for(i in 1:n){
  df1 <- read.table(as.character(v1files[i]), skip=1, 
                    fileEncoding="UCS-2LE", 
                    sep="\t", header=T)
  df2 <- read.table(as.character(v2files[i]), skip=1, 
                    fileEncoding="UCS-2LE", 
                    sep="\t", header=T)
  df3 <- read.table(as.character(v3files[i]), skip=1, 
                    fileEncoding="UCS-2LE", 
                    sep="\t", header=T)
 
  v1 <- cleanData(df1); v2 <- cleanData(df2); v3 <- cleanData(df3)
  v1$cond <- testSeq[i,1]; v2$cond <- testSeq[i,2]; v3$cond <- testSeq[i,3]
  
  df <- rbind(v1,v2,v3)
  x0 <- rbind(x0, df)
}

# Check Trial Numbers 1----------------------------------------------
# Each participant should contribute 2016 trials in total
ddply(x0, .(subj), summarise,
      N = length(rt))

# Clearing-up 1------------------------------------------------------
rm(list=setdiff( ls(), c("x0")))

# Averaging Trials --------------------------------------------------
x1 <- subset(x0, rt >= 200 & rt <= 2000 & acc == 1); nrow(x1)/nrow(x0)
avg <- ddply(x0, .(size,cond,subj,acc), .drop=FALSE, summarize, 
                   N = length(rt), 
                   mean = mean(rt), median = median(rt))
xCC <- subset(avg, acc== 1,  select=N)
xEE <- subset(avg, acc== 0,  select=N)
cc <- (xCC/(xCC+xEE))*100
ee <- (xEE/(xCC+xEE))*100

avgX <- avg[avg$acc !=0,]
avg0 <- cbind(avgX, ee,cc)
avg1 <- avg0[,-c(4)]
names(avg1) <- c("size",  "cond",  "subj", "N", "mean", 
                  "median","ee", "cc")

# Check Trial Numbers 2------------------------------------------
ddply(x0, .(subj, size, cond), summarise, N = length(rt))
avg1[avg1$cc < 70,]  # Check The Condition with Accuracy less then 70%

# Clearing-up 2 and Save Data----------------------------------------
rm(list=setdiff( ls(), c("x1","x0","avg1")))
#save(x0, x1, avg1, file='./data/bu_td_nm_rt.RData')