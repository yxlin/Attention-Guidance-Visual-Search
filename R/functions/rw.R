pub_size <- function(loc=c(.08, .93), basesize=24) {
  theme(axis.title.x = element_text(size=basesize), 
        axis.text.x  = element_text(size=basesize-2,colour="grey30"), 
        axis.title.y = element_text(size=basesize), 
        axis.text.y  = element_text(size=basesize-2,colour="grey30"),
        strip.text.x = element_text(size=basesize, angle=0),  
        strip.text.y = element_text(size=basesize, angle=180),
        strip.background = element_blank(),
        strip.placement = "outside",
        legend.position=  loc,   
        legend.title = element_blank(), 
        legend.text = element_text(size=basesize-4),
        legend.key.size = unit(0.5, "lines"),
        legend.key.width = unit(.6, "cm"))
}

random_walk <- function(n, a, z, u = 1e2, s = 50, lower = 0, dt = .001, 
                        t0=.03, verbose = FALSE, maxsize=1e4) {
  ## Define the bounds
  upper <- a;   # upper bound (correct answer)
  ## lower <- 0;    # lower bound (incorrect answer)
  
  ## Define the Gaussian distribution of the drift rate
  
  # s <- 50;    # standard deviation of drift per unit time (y unit / time unit)
  # u <- 100;   # mean drift rate per unit time (y unit / time unit )
  # dt <- .001;  # step size for simulations (time unit)
  v <- u*dt;   ## drift (y unit)
  sv <- sqrt(dt)*s;
  if (verbose) cat("mean and standard deviation of the drift rate:\n", c(v, sv),'\n')
  nobs <- n;   # observations 
  
  ## Initalize containers:
  y <- matrix(NA, nrow=maxsize, ncol=nobs)
  y[1,] <- z       # set z constant half of the bound 
  
  R <- rep(0, nobs);     # to be filled with 0 or 1 for incorrect and correct
  RT <- rep(0, nobs);    # to be filled with RT values in seconds
  
  for(i in 1:nobs)
  {
    alive <- TRUE  # index vector for walks that haven't terminated
    tStep <- 0;
    while (alive) # loop until all walks are terminated
    {
      if(tStep >= 1e4) break
      tStep <- tStep+1;
      
      # for continuous step sizes, use this line:
      # dy = p.u*p.dt + p.s*sqrt(p.dt)*randn(1,sum(alive));
      dy0 <- rnorm(1, mean = v, sd = sv);
      
      # increment the 'living' walks
      y[tStep+1, i] <- y[tStep, i] + dy0;
      # find the walker(s) that have reached a correct decision:  the 'a' boundary
      # find the walks that reached an incorrect decision: the '-b' boundary
      
      aboveA <- y[tStep+1,i] >= upper
      belowB <- y[tStep+1,i] <= lower
      
      if (any(aboveA))
      {
        R[i] = 1;           # correct response
        alive = FALSE;   # 'kill' the walk
        RT[i] = tStep*dt; # record the RT
        # if (verbose) cat("Walker(s) ", i, "has suppassed the positive threshold\n")
      }
      
      if (any(belowB))
      {
        R[i] = 0;           # incorrect response
        alive = FALSE;   #'kill' the walk
        RT[i] = tStep*dt; # record the RT
        # if (verbose) cat("Walker(s) ", i, "has suppassed the negative threshold\n")
      }
    }
  }
  return(list(R=R, RT=RT+t0, y=y))
}


make_df <- function(obj) {
  value <- c(obj$RT[obj$R==0], obj$RT[obj$R==1])
  C0 <- rep(FALSE, sum(obj$R==0))
  C1 <- rep(TRUE, sum(obj$R==1))
  out <- data.frame(value = value, C = c(C0, C1))
  return(out)
}
