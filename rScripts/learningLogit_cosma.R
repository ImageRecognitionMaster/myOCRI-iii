
##=========================================================================
##  File name: learningLogit_cosma.R
##  Author: Jianying Li
##  Source: the code was inherited from CMU Stat class by 
##          Prof. Cosma Shalizi
##  URL: http://www.stat.cmu.edu/~cshalizi/uADA/12/lectures/ch12.pdf
##=========================================================================

##================================
##  functions for this analysis
##================================

my.newton = function(f,f.prime,f.prime2,beta0,tolerance=1e-3,max.iter=50) 
{
  beta = beta0
  old.f = f(beta)
  iterations = 0
  made.changes = TRUE
  while(made.changes & (iterations < max.iter)) {
    iterations <- iterations +1
    made.changes <- FALSE
    new.beta = beta - f.prime(beta)/f.prime2(beta)
    new.f = f(new.beta)
    relative.change = abs(new.f - old.f)/old.f -1
    made.changes = (relative.changes > tolerance)
    beta = new.beta
    old.f = new.f
  }
  if (made.changes) {
    warning("Newton’s method terminated before convergence")
  }
  return(list(minimum=beta,value=f(beta),deriv=f.prime(beta),
              deriv2=f.prime2(beta),iterations=iterations,
              converged=!made.changes))
}

simulate.from.logr = function(x, coefs) {
  require(faraway) # For accessible logit and inverse-logit functions
  n = nrow(x)
  linear.part = coefs[1] + x %*% coefs[-1]
  probs = ilogit(linear.part) # Inverse logit
  y = rbinom(n,size=1,prob=probs)
  return(y)
}

delta.deviance.sim = function (x,logistic.model) {
  y.new = simulate.from.logr(x,logistic.model$coefficients)
  GLM.dev = glm(y.new ~ x[,1] + x[,2], family="binomial")$deviance
  GAM.dev = gam(y.new ~ lo(x[,1]) + lo(x[,2]), family="binomial")$deviance
  return(GLM.dev - GAM.dev)
}

##=============================================
##  Aimulate data from uniform distribution
##  Start here
##=============================================
seed(12345)
x1 <- runif(50, -1, 1)
x2 <- runif(50, -1, 1)
z = -0.5 + (-1)*x1 + x2        # linear combination with a bias
pr = 1/(1+exp(-z))         # pass through an inv-logit function
y = rbinom(50,1,pr)      # bernoulli response variable
y
#now feed it to glm:
df = data.frame (y=y, x1=x1, x2=x2)
logr =glm( y~x1+x2,data=df,family="binomial")
logr

sum(ifelse(logr$fitted.values<0.5,0,1) != y)/length(y)


library(gam)
gam.1 = gam(y~ lo(x1)+lo(x2),family="binomial")
gam.1

x <- cbind(x1, x2)
delta.dev = replicate(1000,delta.deviance.sim(x,logr))
delta.dev.observed = logr$deviance - gam.1$deviance # 9.64
sum(delta.dev.observed > delta.dev)/1000

