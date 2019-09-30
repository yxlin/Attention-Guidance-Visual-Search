
# revised includes parameter p: mix proportion, mixture of Exg & Fixed Unif.

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
