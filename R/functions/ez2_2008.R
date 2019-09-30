## fdexgun.R  --------------------------------------------------------
## The disfit routines; 
## revised includes parameter p: mix proportion, mixture of Exg & Fixed Unif.
erf <- function(x) { 2 * pnorm(x * sqrt(2)) - 1 }

# exg
exgf <- function(tau,xm,s,t) {
  t1 = 1.0/tau
  t4 = s^2
  t5 = tau^2
  tmp=(xm-t)*t1+t4/t5/2.0
  t10 = exp(tmp)
  t17 = sqrt(2.0)
  t21 = erf((tau*t-tau*xm-t4)/s*t1*t17/2.0)
  t1*t10*(t21+1.0)/2.0 
  
}
# deriv m
exgfdm <-function(tau,xm,s,t) {
  t1 = tau*t
  t3 = tau*xm
  t5 = s^2
  t7 = tau^2
  t8 = 1.0/t7
  tmp=-(2.0*t1-2.0*t3-t5)*t8/2.0
  t11 = exp(tmp)
  t12 = sqrt(pi)
  t13 = t12*s
  t14 = t1-t3-t5
  t15 = 1.0/s
  t18 = sqrt(2.0)
  t22 = erf(t14*t15/tau*t18/2.0)
  t24 = t14^2
  tmp=(-t24/t5*t8/2.0)
  t29 = exp(tmp)
  -t11*(-t13*t22-t13+t29*t18*tau)*t8/t12*t15/2.0
}
# deriv s
exgfds<-function(tau,xm,s,t) {
  t1 = tau*t
  t3 = tau*xm
  t5 = s^2
  t7 = tau^2
  t8 = 1.0/t7
  tmp=(-(2.0*t1-2.0*t3-t5)*t8/2.0)
  t11 = exp(tmp)
  t13 = sqrt(pi)
  t14 = t5*s*t13
  t15 = t1-t3-t5
  t19 = sqrt(2.0)
  t23 = erf(t15/s/tau*t19/2.0)
  t25 = t15^2
  t26 = 1.0/t5
  tmp=(-t25*t26*t8/2.0)
  t30 = exp(tmp)
  t31 = t30*t19
  -t11*(-t14*t23-t14+t31*tau*t5+t31*t7*t-t31*t7*xm)/t7/tau/t13*t26/2.0
}
# deriv t
exgfdt<-function(tau,xm,s,t) {
  t1 = tau*t
  t3 = tau*xm
  t5 = s^2
  t7 = tau^2
  t8 = 1.0/t7
  tmp = (-(2.0*t1-2.0*t3-t5)*t8/2.0)
  t11 = exp(tmp)
  t12 = sqrt(pi)
  t13 = t7*t12
  t14 = t1-t3-t5
  t18 = sqrt(2.0)
  t22 = erf(t14/s/tau*t18/2.0)
  t24 = t12*tau
  t31 = t12*t5
  t33 = t14^2
  tmp = (-t33/t5*t8/2.0)
  t38 = exp(tmp)
  t44 = t7^2
  t11*(-t13*t22-t13+t24*t*t22+t24*t-t24*xm*t22-t24*xm-t31*t22-t31+t38*t18*s*tau)/t44/t12/2.0
}

# exgauss dens
exgauss<-function(xm,s,tau,t) {exgf(tau,xm,s,t)}

# uniform density
unif<-function(a,b,t) {(t-t+1)/(b-a)} # t-t+1 = 1 nonsense. but useful when t is a vector

# mixture
# log likelihood
loglexgm<-function(par,x,y)
{
  m=par[1]
  s=par[2]
  t=par[3]
  p1=par[4]
  p2=1-p1
  a=y[1]
  b=y[2]
  L=-sum(log(exgauss(m,s,t,x)*p1+unif(a,b,x)*p2))
  print(L)
  L
}


postp<-function(par,x,y)
{
  m=par[1]
  s=par[2]
  t=par[3]
  a=y[1]
  b=y[2]
  p1=par[4]
  p2=1-p1
  xtmp=exgauss(m,s,t,x)*p1+unif(a,b,x)*p2
  xtmp1=exgauss(m,s,t,x)
  xtmp2=unif(a,b,x)
  cbind(p1*xtmp1/xtmp,p2*xtmp2/xtmp)
}



# mixture
# derivatives
loglexggm<-function(par,x,y)
{
  g=c(0,0,0,0)
  m=par[1]
  s=par[2]
  t=par[3]
  a=y[1]
  b=y[2]
  p1=par[4]
  p2=1-p1
  xtmp=exgauss(m,s,t,x)*p1+unif(a,b,x)*p2
  xtmp1=exgauss(m,s,t,x)
  xtmp2=unif(a,b,x)
  g[1]=-sum(p1*exgfdm(t,m,s,x)/xtmp)       
  g[2]=-sum(p1*exgfds(t,m,s,x)/xtmp) 
  g[3]=-sum(p1*exgfdt(t,m,s,x)/xtmp)
  g[4]=-sum((xtmp1-xtmp2)/xtmp)
  g
}

# mixture
loglexg2m<-function(par,x,y)
{
  m=par[1]
  s=par[2]
  t=par[3]
  a=y[1]
  b=y[2]
  p1=par[4]
  p2=1-p1
  L=-sum( log(exgauss(m,s,t,x)*p1+unif(a,b,x)*p2))
  g=loglexgg(par,x)
  attr(L,"gradient")=g
  L
}

# only exg
# log likelihood
loglexg<-function(par,x)
{
  m=par[1]
  s=par[2]
  t=par[3]
  -sum(log(exgauss(m,s,t,x)))
}
# only exg
# derivatives
loglexgg<-function(par,x)
{
  g=c(0,0,0)
  m=par[1]
  s=par[2]
  t=par[3]
  xtmp=exgf(t,m,s,x)
  g[1]=-sum(exgfdm(t,m,s,x)/xtmp)       
  g[2]=-sum(exgfds(t,m,s,x)/xtmp) 
  g[3]=-sum(exgfdt(t,m,s,x)/xtmp)
  g
}

# only exg
loglexg2<-function(par,x)
{
  m=par[1]
  s=par[2]
  t=par[3]
  L=-sum(log(exgauss(m,s,t,x)))
  g=loglexgg(par,x)
  attr(L,"gradient")=g
  L
}


## EZ MAIN --------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
RobustEZ.from.File = function(file.name)
{
  #==========================================================================# 
  # Read in data from file.name
  #==========================================================================#
  
  d    = scan(file = file.name)
  N    = d[1]
  rt   = d[2:length(d)]
  ac   = length(rt)/N
  
  pars = Get.Robust.vaTer(rt, ac, min_U=min(rt), max_U=max(rt),start_p_EG=.95)
  
  return(pars)
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
Get.Robust.vaTer = function(dat, Pc, min_U=min(dat), max_U=max(dat), start_p_EG=.95, s=.1)
{
  #==========================================================================#
  # Get cleaned-up values for EZ pars
  #==========================================================================#
  meanvarp = Get.Robust.MV(dat, min_U, max_U, start_p_EG)
  MRT      = meanvarp[1]
  VRT      = meanvarp[2]
  p_EG	   = meanvarp[3]
  # MRT & VRT cleaned up, now plug these in to EZ equations
  s2 = s^2
  # The default value for the scaling parameter s equals .1
  if (Pc == 0)
    cat("Oops, Pc == 0!\n")
  if (Pc == 0.5)
    cat("Oops, Pc == .5!\n")
  if (Pc == 1)
    cat("Oops, Pc == 1!\n")
  # If Pc equals 0, .5, or 1, the method will not work, and
  # an edge-correction is required.
  L = qlogis(Pc)
  # The function "qlogis" calculates the logit.
  x = L*(L*Pc^2 - L*Pc + Pc - 0.5)/VRT
  v = sign(Pc-0.5)*s*x^(1/4)
  # This gives drift rate.
  a = s2*qlogis(Pc)/v
  # This gives boundary separation.
  y = -v*a/s2
  MDT = (a/(2*v))*(1-exp(y))/(1+exp(y))
  Ter = MRT-MDT
  # This gives nondecision time.
  return(c(v, a, Ter, p_EG))
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
Get.Robust.MV = function(dat, min_U, max_U, start_p_EG)
{
  #==========================================================================# 
  # Get cleaned-up values for RT mean and RT variance
  #==========================================================================#
  pars     = Est.ExGaussUnif(dat, min_U, max_U, start_p_EG)
  EG_mean  = pars[1] + pars[3]
  EG_var   = pars[2]^2 + pars[3]^2
  meanvarp = c(EG_mean, EG_var, pars[4])
  return(meanvarp)
}
# to check:
# mu=.4; sigma=.035; tau=.065; p_EG=.8; min_U=0.1; max_U=1.2
# dat  = Gen.ExGaussUnif(1000, mu, sigma, tau, p_EG, min_U, max_U)
# Est.ExGaussUnif(dat,.1,1.2,.9)
# Get.Robust.MV(dat, .1,1.2,.9)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
Est.ExGaussUnif = function(dat, min_U, max_U, start_p_EG)
{
  #==========================================================================# 
  # Get Estimates for Ex-Gauss/Unif RT distribution
  #==========================================================================#
  start.pars = c(MMest.ExGauss(dat), start_p_EG)
  
  lo   = c(0.1, .010, .010, 0.60)      # lower bounds; note bound on mix proportion 
  up   = c(1,   .300, .300, 0.9999999) # upper bounds
  y    = c(min_U,max_U) # fixed unif pars
  
  # quasi newton
  
  res  = optim(start.pars, loglexgm, loglexggm, method="L-BFGS-B",
               lower=lo, upper=up, hessian=T, x=dat, y=y)
  
  pars = res$par[1:4]
  return(pars)
}
# to check:
# mu=.4; sigma=.035; tau=.065; p_EG=.8; min_U=.1; max_U=1.2
# dat = Gen.ExGaussUnif(1000, mu, sigma, tau, p_EG, min_U, max_U)
# Est.ExGaussUnif(dat,.1,1.2,.9)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
MMest.ExGauss = function(dat)
{
  #==========================================================================# 
  # Get Method of Moments Estimates for ExGauss distribution
  # cf. Heathcote (1996)
  #==========================================================================#
  M1    = mean(dat)
  M2    = var(dat) #division by n-1
  M3    = (length(dat)/(length(dat)-1)) * mean ( (dat-M1)^3 ) #effectively division by n-1
  
  tau   = (0.5*M3)^(1/3)
  if (is.nan(tau)) 
  {
    message('tau is NAN')
    tau <- 0
  }
  mu    = M1 - tau
  if ( (M2 - tau^2)>0 )
  {
    sigma = sqrt(M2 - tau^2) 
  }
  if ( ((M2 - tau^2)<=0) || (sigma<=0) || (tau<=0) ) #in case of weird values
  {
    tau   = 0.8 * sd(dat)
    mu    = M1 - tau
    #sigma = sqrt(M2 - tau^2) # this is the same as:
    sigma = 0.6 * sd(dat)	    # (correcting a small mistake in Heathcote 1996)
  }
  pars = c(mu, sigma, tau)
  return(pars)
}
# to check:
# mu=.4; sigma=.035; tau=.065
# dat = Gen.ExGauss(1000, mu, sigma, tau)
# MMest.ExGauss(dat)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
Gen.ExGaussUnif = function(N, mu, sigma, tau, p_EG, min_U, max_U)
{
  #==========================================================================# 
  # Generate data from ExGauss distribution Contaminated with a Uniform 
  #==========================================================================#
  EG = rnorm(round(p_EG*N), mean=mu, sd=sigma) +	rexp(round(p_EG*N), rate=1/tau)	
  Un = runif(N-round(p_EG*N), min=min_U, max=max_U)
  dat = c(EG,Un)
  
  return(dat)
}
# to check:
# mu=.4; sigma=.035; tau=.065; p_EG=.8; min_U=.1; max_U=1.2
# dat = Gen.ExGaussUnif(1000, mu, sigma, tau, p_EG, min_U, max_U)
# plot(dat)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
Gen.ExGauss = function(N, mu, sigma, tau)
{
  #==========================================================================# 
  # Generate data from ExGauss distribution
  #==========================================================================#
  dat = rnorm(N, mean=mu, sd=sigma) +	rexp(N, rate=1/tau)	
  return(dat)
}
# to check:
# mu=.4; sigma=.035; tau=.065
# dat = Gen.ExGauss(1000, mu, sigma, tau)
# mu+tau # theoretical mean
# mean(dat)
# sigma^2 + tau^2 # theoretical variance
# var(dat)
# 2*tau^3 # theoretical 3rd moment
# (length(dat)/(length(dat)-1)) * mean ( (dat-mean(dat))^3 )
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## Wrapper functions -----------------------------------
GetEZData2015 <- function(df, s = 's', fac = 'S', C = 'C', dep = 'RT')
{
  # df <- feat1
  # s = 's'
  # fac = c('S', 'SS')
  # C = 'C'
  # dep = 'RT'
  # avg0 <- GetEZData(feat1, fac = c('S', 'SS'))
  eqn0 <- paste(dep, paste(paste(c(s, C, fac)), collapse = '+'), sep='~')
  eqn1 <- paste(dep, paste(paste(c(s,    fac)), collapse = '+'), sep='~')
  for0 <- formula(eqn0)
  for1 <- formula(eqn1)
  
  dfN <- aggregate(for0, data = df, FUN = 'length', drop = FALSE)
  dfM <- aggregate(for0, data = df, FUN = 'mean',   drop = FALSE)
  dfV <- aggregate(for0, data = df, FUN = 'var',    drop = FALSE)
  dfP <- aggregate(for1, data = df, FUN = 'length', drop = FALSE)

  
  ## 100% correct will return NA count. Here corrects it to 0 count
  dfN[[dep]][ is.na(dfN[[dep]]) ] <- 0
  
  dfN$MRT <- dfM[[dep]]  ## MRT
  dfN$VRT <- dfV[[dep]]  ## VRT
  
  ## RT column is replaced with count
  dfO <- dfN[dfN[[C]] == TRUE, ]
  dfX <- dfN[dfN[[C]] == FALSE,]
  
  dfO$Pc <- dfO[[dep]] / dfP[[dep]]


  ## Edge correction
  ## Here we chose to apply one of the standard edge-correction methods, 
  ## replacing Pc = 1 with a value that corresponds to one half of an 
  ## error,
  if (any(dfO$Pc == 1)) message('Edge correction')
  dfO$Pc <- ifelse(dfO$Pc == 1, 1 - (1/(2*dfO[[dep]])), dfO$Pc)


  selected <- c(s, fac, dep, 'MRT', 'VRT', 'Pc')
  out <- dfO[, selected]
  
  names(out) <- c(s, fac, 'N', 'MRT', 'VRT', 'Pc')
  return(out)
}


run2015 <- function(DT, df)
{
  require(data.table)
  DT <- data.table::data.table(DT)
  d0 <- data.table::data.table(df)
  setkey(d0, s, S, C, SS)
  
  subjects <- levels(DT$s)
  stimuli  <- levels(DT$S)
  setsizes <- levels(DT$SS)
  ns  <- length(subjects)
  nS  <- length(stimuli)
  nSS <- length(setsizes)
  
  nest <- ns * nS * nSS
  
  npar <- 4
  x0 <- NULL
  for(i in 1:ns)
  {
    for(j in 1:nS)
    {
      for (k in 1:nSS)
      {
        tmp0 <- d0[s==subjects[i] & S == stimuli[j] & SS == setsizes[k] & C == TRUE]
        tmp1 <- DT[s==subjects[i] & S == stimuli[j] & SS == setsizes[k]]
        rt <- tmp0$RT
        ac <- tmp1$Pc
        pars <- Get.Robust.vaTer(rt, ac, min_U=min(rt), max_U=max(rt),start_p_EG=.95)
        x0 <- rbind(x0, c(subjects[i], stimuli[j], setsizes[k], pars))
        
      }
    }
  }
  
  out <- data.frame(x0)
  out[,4] <- as.double(as.character(out[,4]))
  out[,5] <- as.double(as.character(out[,5]))
  out[,6] <- as.double(as.character(out[,6]))
  out[,7] <- as.double(as.character(out[,7]))
  
  names(out) <- c('s', 'S', 'SS', 'v', 'a', 't0', 'pr')
  return(out)
}

GetEZData <- function(df)
{
  require(data.table)
  ## Get correct MRT, VRT and Pc
  ## col <- c('s', 'S', 'C', 'SS')
  ## Pc; CJ and EACHI preserve the 0 counts
  
  d0 <- data.table::data.table(df)
  setkey(d0, s, I, Q, C, SS)
  avg <- d0[CJ(s, I, Q, C, SS, unique = TRUE), 
            .(N   = .N,
              MRT = mean(RT),
              VRT = var(RT)), by = .EACHI]
  
  pro <- d0[CJ(s, I, Q, C, SS, unique = TRUE), .N, by = .EACHI]
  pro[, NN := sum(N), .(I, Q, SS, s)]
  pro[, Pc := N/NN]
  
  if(!all( avg[C == TRUE]$s == pro[C == TRUE]$s )) stop("avg != pro")
  if(!all( avg[C == TRUE]$I == pro[C == TRUE]$I )) stop("avg != pro")
  if(!all( avg[C == TRUE]$Q == pro[C == TRUE]$Q )) stop("avg != pro")
  if(!all( avg[C == TRUE]$SS == pro[C == TRUE]$SS )) stop("avg != pro")
  
  avg$NN <- pro$NN
  avg$Pc <- pro$Pc
  
  ## Edge correction
  ## Here we chose to apply one of the standard edge-correction methods, 
  ## replacing Pc = 1 with a value that corresponds to one half of an 
  ## error,
  if (any(avg$Pc == 1)) message('Edge correction')
  avg$Pc <- ifelse(avg$Pc == 1, 1 - (1/(2*avg$N)), avg$Pc)
  
  
  selected <- c('s', 'I', 'Q', 'SS', 'Pc', 'VRT', 'MRT')
  return(avg[C == TRUE, ..selected])
}

run <- function(DT, df)
{
  # DT <- avg0
  # df <- e1
  require(data.table)
  d0 <- data.table::data.table(df)
  setkey(d0, s, I, Q, C, SS)
  
  subjects <- levels(DT$s)
  intervals<- levels(DT$I)
  stimuli  <- levels(DT$Q)
  setsizes <- levels(DT$SS)
  ns  <- length(subjects)
  nI  <- length(intervals)
  nQ  <- length(stimuli)
  nSS <- length(setsizes)
  
  nest <- ns * nI * nQ * nSS
  
  npar <- 4
  x0 <- NULL
  # i <- j <- k <- l <- 1
  for(i in 1:ns)
  {
    for(j in 1:nI)
    {
      for(k in 1:nQ)
      {
        for (l in 1:nSS)
        {
          tmp0 <- d0[s==subjects[i] & I == intervals[j] & Q == stimuli[k] & 
                       SS == setsizes[l] & C == TRUE]
          tmp1 <- DT[s==subjects[i] & I == intervals[j] & Q == stimuli[k] & 
                       SS == setsizes[l]]
          rt <- tmp0$RT
          ac <- tmp1$Pc
          pars <- Get.Robust.vaTer(rt, ac, min_U=min(rt), max_U=max(rt),start_p_EG=.95)
          x0 <- rbind(x0, c(subjects[i], intervals[j], stimuli[k], setsizes[l], pars))
        }
      }
      
    }
  }
  
  out <- data.frame(x0)
  
  out[,5] <- as.double(as.character(out[,5]))
  out[,6] <- as.double(as.character(out[,6]))
  out[,7] <- as.double(as.character(out[,7]))
  out[,8] <- as.double(as.character(out[,8]))
  
  names(out) <- c('s', 'I', 'Q', 'SS', 'v', 'a', 't0', 'pr')
  return(out)
}
