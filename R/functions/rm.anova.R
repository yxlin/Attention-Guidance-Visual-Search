rm.anova <- function(dat=NULL, measurevar, idvar, withinvars=NULL, betweenvars=NULL ) {
  library(car); library(reshape2)
  
  #----------------------------------------------------------
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- sapply(dat[, c(withinvars), drop=FALSE], FUN=is.factor)
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    dat[nonfactorvars] <- lapply(dat[nonfactorvars], factor)
  }
  
  tmp <- as.formula( paste(idvar, "~", paste(withinvars, collapse="+")))
  datWide <- dcast(dat, tmp, value.var = measurevar)
  
  #--
  nameWithin <- names(datWide)[-1]  # take off subj column
  numberOfWithinFactor <- length(withinvars)
  
  # Each row represents one combination of conditions; each column is one factor
  wf <- matrix( numeric(numberOfWithinFactor*length(nameWithin)), ncol=numberOfWithinFactor)
  
  #-- construct idata
  for(i in 1:length(nameWithin))  {
    t1 <- unlist( strsplit(nameWithin[i], "_", fixed=TRUE) )
    for (j in 1:length(t1)) {
      wf[i,j] <-  t1[j]  
    }
  }
  
  idata <- data.frame(wf);  colnames(idata) <- withinvars
  
  #-- construct idesign
  if( length(names(idata)) == 1) {
    ides <- as.formula( paste("~", names(idata) ) )
  } else {
    ides <- as.formula( paste("~", paste(names(idata), collapse="*")))
  }
  
  #-- extract names of between factor;
  # this part has not been fully tested.
  if( is.null(betweenvars) ) {
    nameBetween <- 1
  } else {
    #nameBetween <- names(datWide)[c(2,3)]
  }
  
  y <- as.matrix(datWide[,2:ncol(datWide)])
  fittedFormula <- as.formula(paste('y ~ ', paste(nameBetween, collapse='*') ) )
  
  #-- print rm anova summary
  mod <- lm(  fittedFormula, data=dat)
  av.mod <- Anova(mod, idata=idata, idesign=ides, type="III")
  
  return(av.mod)
}
