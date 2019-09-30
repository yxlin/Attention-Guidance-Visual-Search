rm(list=ls())
datc <- read.table("ranTab3c", header=F)
datv <- read.table("ranTab3v", header=F)
head(datc,1)
head(datv,1)

# cueImg
unique(datc[,5])
unique(datv[,5])

# target img
unique(datc[,39])
unique(datv[,39])

all(datc[,5] == datc[,39])

# congruence
apply(datc[,c(2:8)], 2, unique)
apply(datv[,c(2:8)], 2, unique)
