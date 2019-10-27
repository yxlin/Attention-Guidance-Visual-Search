predict_one <- function(object, npost = 100, rand = TRUE, factors = NA,
                        xlim = NA, seed = NULL)
{
  model <- attributes(object$data)$model
  facs <- names(attr(model, "factors"))
  class(object$data) <- c("data.frame", "list")
  
  if (!is.null(factors))
  {
    if (any(is.na(factors))) factors <- facs
    if (!all(factors %in% facs))
      stop(paste("Factors argument must contain one or more of:",
                 paste(facs, collapse=",")))
  }
  
  resp <- names(attr(model, "responses"))
  ns   <- table(object$data[,facs], dnn = facs)
  npar   <- object$n.pars
  nchain <- object$n.chains
  nmc    <- object$nmc
  ntsample <- nchain * nmc
  pnames   <- object$p.names
  thetas <- matrix(aperm(object$theta, c(3,2,1)), ncol = npar)
  
  colnames(thetas) <- pnames
  
  if (is.na(npost)) {
    use <- 1:ntsample
  } else {
    if (rand) {
      use <- sample(1:ntsample, npost, replace = F)
    } else {
      use <- round(seq(1, ntsample, length.out = npost))
    }
  }
  
  npost  <- length(use)
  posts   <- thetas[use, ]
  nttrial <- sum(ns) ## number of total trials
  
  v <- lapply(1:npost, function(i) {
    ggdmc:::simulate_one(model, n = ns, ps = posts[i,], seed = seed)
  })
  out <- data.table::rbindlist(v)
  reps <- rep(1:npost, each = nttrial)
  out <- cbind(reps, out)
  
  if (!any(is.na(xlim)))
  {
    out <- out[RT > xlim[1] & RT < xlim[2]]
  }
  
  attr(out, "data") <- object$data
  return(out)
}

shade <- function(d, bin, col = "black", npos=1e2, median=FALSE) {
  
  grayscale <- gray(0:8 / 8, alpha=1/2)
  
  x0 <- matrix(NA, nrow=100, ncol=length(bin)-1)  
  cat("Use only correct responses\n")
  for(i in 1:npos) {
    tmp <- hist( d[reps==i & C==TRUE]$RT, breaks=bin, plot=FALSE) 
    x0[i,] <- tmp$counts
  }
  
  loc <- matrixStats::colQuantiles(x0, probs=c(.1, .5, .9))
  
  if (median) {
    lines(tmp$mids, loc[,2], lwd=2, lty="solid", col=rethinking::col.alpha(col, .45))
  } else {
    xx <- c(tmp$mids, rev(tmp$mids)) ## all mids are the same, bc bin controls it.
    yy <- c(loc[,1], rev(loc[,3]))
    polygon(xx, yy, col=rethinking::col.alpha(col, .15), border=col)
  }
  
}
