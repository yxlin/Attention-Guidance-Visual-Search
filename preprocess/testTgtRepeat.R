#-------------------------------------------------------
# Author: Yishin Lin
# Date: 19 April, 2014
# Description: Calculate the how the percentage of 
# repeated trials

loadedPackages <-c("plyr", "car") 
suppressPackageStartupMessages(lapply(loadedPackages, require, 
                                      character.only=TRUE));
rm(list=ls()); 
load("./data/bu_td_nm_rt.RData")

subj.seq <- unique(x0$subj)
block.seq <- unique(x0$block)
session.seq <- unique(x0$session)
tmpholder <- numeric(nrow(x0)/3/10/14-1)
df <- NULL

for(i in seq(along=subj.seq)){
  for(j in seq(along=block.seq)){
    for(k in seq(along=session.seq)){
      subdata <- subset(x0, subj==subj.seq[i] & 
                      block==block.seq[j] & 
                      session==session.seq[k])
      
      for(l in 2:nrow(subdata)){
        tmpholder[l-1] <- subdata$tgt[l] == subdata$tgt[l-1]
      }
      repeatRate <- sum(tmpholder)/length(tmpholder)
      tmp <- c(repeatRate, subj.seq[i], block.seq[j], 
                     session.seq[k])
      df <- rbind(df, tmp)
    }
  }
}
  

colnames(df) <- c("rate", "subj", "block", "session")
rownames(df) <- 1:nrow(df)
df2 <- as.data.frame(df)
plot(df2$rate)
summary(df2)

