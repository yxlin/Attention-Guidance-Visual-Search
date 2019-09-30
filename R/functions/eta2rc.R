eta2rc <- function(fit){
  aovtab <- summary(fit, multivariate=F)[4]$univariate.tests[-1,]
  eta2 <- numeric(nrow(aovtab))
  for(i in 1:nrow(aovtab)){
    eta2[i] <- aovtab[i,1]/(aovtab[i,1]+aovtab[i,3])
  }
  aovtab2 <- data.frame(aovtab,eta2);
  aovtab2$signif <- ifelse(aovtab2$P > .1, "ns",
  ifelse(aovtab2$P <= .1 & aovtab2$P > .05, ".",
  ifelse(aovtab2$P <= .05 & aovtab2$P > .01, "*",
  ifelse(aovtab2$P <= .01 & aovtab2$P > .005, "**",
  ifelse(aovtab2$P <= .005 & aovtab2$P > .001, "***","****")))))

  options(digits=3)
  colnames(aovtab2) <- c("SS","num.df","Error.SS","den.df","F","P","eta2","signif")
  
  GG <- summary(fit, multivariate=F)[5]$pval.adjustments
  out <- list(aovtab2,GG[,])
  names(out) <- c("rm.anova", "GG-HF")
  return(out)
}

