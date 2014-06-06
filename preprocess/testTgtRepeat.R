#-------------------------------------------------------
# Author: Yishin Lin
# Date: 19 April, 2014
# Description: Calculate the percentage of repeated trials

# Load packages and data  -----------------------------------------
loadedPackages <-c("plyr", "car", "ggplot2", "grid") 
suppressPackageStartupMessages(lapply(loadedPackages, require, 
                                      character.only=TRUE));
rm(list=ls()); 
load("./data/bu_td_nm_rt.RData")

# Set up the for loop parameters -------------------------------
subj.seq <- unique(x0$subj)
block.seq <- unique(x0$block)
cond.seq <- unique(x0$cond)
tmpholder <- numeric(nrow(x0)/3/10/14-1)
df <- NULL

# Start for loop --------------------------------------------------
for(i in seq(along=subj.seq)){
  for(j in seq(along=block.seq)){
    for(k in seq(along=cond.seq)){
      subdata <- subset(x0, subj==subj.seq[i] & 
                      block==block.seq[j] & 
                      cond==cond.seq[k])
      
      # Test if trial N-1 has the same target letter as trial N       
      for(l in 2:nrow(subdata)){
        tmpholder[l-1] <- subdata$tgt[l] == subdata$tgt[l-1]
      }
      
      # Calculate the percentrage
      repeatRate <- signif(sum(tmpholder)/length(tmpholder),4)
      tmp <- c(repeatRate, subj.seq[i], block.seq[j], 
                     cond.seq[k])
      df <- rbind(df, tmp)
    }
  }
}
  

# Give meaningful row and column names ----------------------------
colnames(df) <- c("rate", "subj", "block", "cond")
rownames(df) <- 1:nrow(df)
df2 <- as.data.frame(df)
df2$block <- factor(df2$block, levels=1:14)
summary(df2)

# Visualise data --------------------------------------------------
p <- ggplot(df2, aes(x=block, y=rate)) + 
  geom_line(aes(group=cond)) +
  geom_point(size=4.5, fill="white") +
  facet_grid(subj~cond)

# jpeg(filename = "./figures/primingTest.jpeg",
#      width = 1280, height = 1024, units = "px", pointsize = 8,
#      quality = 95 ,bg = "white")
print(p)
dev.off()

