## ----eval=FALSE---------------------------------------------------------------
#  GMSMOOTH <- function(y, x, h) {
#    n <- length(y)
#    s <- c(-Inf, 0.5 * (x[-n] + x[-1]), Inf)
#    s.hat <- rep(0, n)
#    for (i in 1:n) {
#      fx.hat <- function(z, h, x) {
#        dnorm((x - z)/h)/h
#      }
#      a <- y[i] * integrate(fx.hat, s[i], s[i + 1], h =
#                              h, x = x[i])$value
#      s.hat[i] <- sum(a)
#    }
#    return(s.hat)
#  }

## -----------------------------------------------------------------------------
NWSMOOTH <- function(h,y,x) {
  n <- length(y) 
  s.hat <- rep(0, n) 
  for (i in 1:n) {
    a <- fx.hat(x[i], h) 
    s.hat[i] <- sum(y * a/sum(a))
  } 
  return(s.hat)
} 

## ----eval=TRUE----------------------------------------------------------------
library(SC19076)
x <- seq(-1, 1, length = 40)
y <- 5 * x * cos(5 * pi * x)
h <- 0.055 
fx.hat <- function(z, h) { 
  dnorm((z - x)/h)/h
}
NWsmooth.val <- NWSMOOTH(h, y, x)
GMsmooth.val <- GMSMOOTH(y, x, h)
plot(x, y, xlab = "Predictor", ylab = "Response", col = 1) 
f <- function(x) 5 * x * cos(5 * pi * x) 
curve(f, -1, 1, ylim = c(-15.5, 15.5), lty = 1, add = T, col = 1)
lines(x, NWsmooth.val, lty = 2, col = 2)
lines(x, GMsmooth.val, lty = 3, col = 3)
letters <- c("orignal model", "NW method", "GM method") 
legend("bottomright", legend = letters, lty = 1:3, col = 1:3, cex = 0.5)



## -----------------------------------------------------------------------------
x <- seq(-1, 1, length = 40) 
y <- 5 * x * cos(5 * pi * x) 
fx.hat <- function(z, h) { 
  dnorm((z - x)/h)/h
}
NWSMOOTH <- function(h, y, x) { 
  n <- length(y) 
  s.hat <- rep(0, n)
  for (i in 1:n) { 
    a <- fx.hat(x[i], h) 
    s.hat[i] <- sum(y * a/sum(a))
  } 
  return(s.hat)
}
h <- 0.025 
NWsmooth.val0 <- NWSMOOTH(h, y, x) 
h <- 0.05 
NWsmooth.val1 <- NWSMOOTH(h, y, x) 
h <- 0.1 
NWsmooth.val2 <- NWSMOOTH(h, y, x)
h <- 0.2 
NWsmooth.val3 <- NWSMOOTH(h, y, x) 
h <- 0.3
NWsmooth.val4 <- NWSMOOTH(h, y, x)
plot(x, y, xlab = "Predictor", ylab = "Response", col = 1) 
f <- function(x) 5 * x * cos(5 * pi * x) 
curve(f, -1, 1, ylim = c(-15.5, 15.5), lty = 1, add = T, col = 1) 
lines(x, NWsmooth.val0, lty = 2, col = 2) 
lines(x, NWsmooth.val1, lty = 3, col = 3) 
lines(x, NWsmooth.val2, lty = 4, col = 4)
lines(x, NWsmooth.val3, lty = 5, col = 5)
lines(x, NWsmooth.val4, lty = 6, col = 6)
letters <- c("orignal model", "h=0.025", "h=0.05",
"h=0.1", "h=0.2", "h=0.3") 
legend("bottom", legend = letters, lty = 1:6, col = 1:6, cex = 0.5)

## -----------------------------------------------------------------------------
data(occupationalStatus)
head(occupationalStatus)
summary(occupationalStatus)

## ---- echo=FALSE--------------------------------------------------------------
set.seed(120)
x=rnorm(150,mean=60,sd=10)
hist(x,breaks = 28)

## ----fig.width=8,fig.height=4-------------------------------------------------
set.seed(123)
n<-1000
u<-runif(n)

### several choices of σ
sigma=c(0.3,0.5,0.7,0.9)
for (i in sigma){
x=sqrt(-2*(i)^2*logb(1-u))

### Generate Rayleigh samples 
hist(x,prob=TRUE,
     main=expression(f(x)==(x/sigma^2)*e^(-x^2/2*sigma^2)))
y <- seq(0, 10, .1)

### check that the mode of the generated samples is close to the theoretical mode
lines(y, (y/i^2)*exp((-y^2)/(2*(i)^2)))
}

## -----------------------------------------------------------------------------
set.seed(123)
n <- 1000
x1 <- rnorm(n, 0, 1) 
x2 <- rnorm(n, 3, 1)

###Generate a random sample of size 1000 
s <- x1 + x2
u <- runif(n)

### Graph the histogram when p1 =0.75
k <- as.integer(u > 0.75) #vector of 0’s and 1’s
x<-k*x1+ (1-k) * x2  #the mixture
par(mar=c(1,1,1,1))#two graphs per page 
hist(s, freq = F, breaks = 30) 
lines(density(s, bw=.5), col="red", lwd=2)

## -----------------------------------------------------------------------------
n <- 1000
mean_s <- c(0, 3)

### Repeat with different values for p1 
y<- sample(c("head", "tail"), size = n, replace = TRUE, prob = c(0.6, 0.4))
x <- rnorm(n = 1000, mean = mean_s[1])
tails <- y %in% c("tail")
x[tails] <- rnorm(sum(tails), mean = mean_s[2])
require(lattice)
densityplot(~x, par.settings = list(plot.symbol = list(col = factor(y))))
y<- sample(c("head", "tail"), size = n, replace = TRUE, prob = c(0.5, 0.5))
x <- rnorm(n = 1000, mean = mean_s[1])
tails <- y %in% c("tail")
x[tails] <- rnorm(sum(tails), mean = mean_s[2])
require(lattice)
densityplot(~x, par.settings = list(plot.symbol = list(col = factor(y))))
y<- sample(c("head", "tail"), size = n, replace = TRUE, prob = c(0.25, 0.75))
x <- rnorm(n = 1000, mean = mean_s[1])
tails <- y %in% c("tail")
x[tails] <- rnorm(sum(tails), mean = mean_s[2])
require(lattice)
densityplot(~x, par.settings = list(plot.symbol = list(col = factor(y))))
y<- sample(c("head", "tail"), size = n, replace = TRUE, prob = c(0.1, 0.9))
x <- rnorm(n = 1000, mean = mean_s[1])
tails <- y %in% c("tail")
x[tails] <- rnorm(sum(tails), mean = mean_s[2])
require(lattice)
densityplot(~x, par.settings = list(plot.symbol = list(col = factor(y))))

## -----------------------------------------------------------------------------
require(graphics)
### generate a funtion
runif.wis<-function(sigma,n){
  p<-ncol(sigma)
  l<-chol(sigma)
  A<-matrix(ncol=p,nrow=p)
  for(i in 1:p){
  for(j in 1:p){
    A[i,j]=rnorm(1,0.1)
  }
  }
  A[lower.tri(A)]=0
  c<-numeric(p)
  for (i in 1:p){
    c[i]=rchisq(1,n-i+1)
    A[i,i]=c[i]
  }
  s<-l%*%A%*%t(A)%*%t(l)
  return(s)
}

## -----------------------------------------------------------------------------
### generate  random sample from Wishart
m<-matrix(c(5,1,1,2),2,2)
k<-2*diag(8)
sigma1<-m
sigma2<-k
n<-18
runif.wis(sigma1,n)
runif.wis(sigma2,n)

## -----------------------------------------------------------------------------
set.seed(0)
m<-10000
t<-runif(m,min=0,max=pi/3)

### Compute a Monte Carlo estimate
theta.hat<-mean(sin(t))*(pi/3)
print(theta.hat)

### compare your estimate with the exact value of the integral
print((cos(0)-cos(pi/3))-theta.hat)

## -----------------------------------------------------------------------------
set.seed(123)

## antithetic variables to estimate
MC.Phi <- function(x, R = 10000, antithetic = TRUE) {
u <- runif(R/2) 
if (!antithetic)
v <- runif(R/2) else
v<-1- u
u <- c(u, v)
theta.hat <- numeric(length(x))
for (i in 0:length(x)) { 
g <- mean(exp(-u*x)/(1+(u*x)^2)) 
} 
g
}

## -----------------------------------------------------------------------------
set.seed(123)
MC1 <- MC.Phi(1, anti = FALSE)
print(MC1)
set.seed(123) 
MC2 <- MC.Phi(1)
print(MC2)

## -----------------------------------------------------------------------------
set.seed(123)
m <- 10000
MC1 <- MC2 <- numeric(m) 
x <- 1
for (i in 1:m){
   MC1[i] <- MC.Phi(x, R = 1000, anti = FALSE)
   MC2[i] <- MC.Phi(x, R = 1000)
}
 print(sd(MC1))
 print(sd(MC2))
 
 ## approximate reduction in variance
 print((var(MC1) - var(MC2))/var(MC1))

## -----------------------------------------------------------------------------

m<-10000   # number of replicates
set.seed(0) 
k<-5      # number of strata
r<-m/k    
N<-50     # number of times to repeat the estimation
T2<-numeric(k)
estimates<-matrix(0,N,2)
g<-function(x) {
  exp(-x)/(1+x^2)*(x>0)*(x<1)
}
## stratified importance sampling
for (j in 1:N) {
for (i in 1:k) {

    u<-c(0,0.2,0.4,0.6,0.8,1)
    q<--log(1-u*(1-exp(-1)))   
    u1<-runif(m/5)
    y<--log(exp(-q[i])-u1/5*(1-exp(-1)))
    f1<-g(y)/(5*exp(-y)/(1-exp(-1)))
    T2[i]<-mean(f1)
}
 estimates[j,2]<-sum(T2)
 
##inverse transformated method

  u2<-runif(m)
  x<--log(1-u2*(1-exp(-1)))    
  f2<-g(x)/(exp(-x)/(1-exp(-1)))
  estimates[j,1]<-mean(f2)
} 



print(estimates)

apply(estimates,2,mean)
apply(estimates,2,var)
print((var(estimates[,1])-var(estimates[,2]))/var(estimates[,1]))

## -----------------------------------------------------------------------------
set.seed(0)
n<-20 
alpha <- .05
# sample mean of x : mean(x)
# standard deviation of x : sd(x)
# the 97.5th percentile of the t-distribution with n degrees of freedom : qt(0.975, n)

inORnot <- function(alpha, n, mu) {

   x <- rchisq(n, df = 2) 

  L = mean(x) - qt(1 - alpha / 2, n - 1)*sd(x)/sqrt(n)
  U = mean(x) + qt(1 - alpha / 2, n - 1)*sd(x)/sqrt(n)

  return(L <= mu & U >= mu)

}

covPro <- function(alpha, S, n) {

  cum = 0
  for (i in 1:S) cum = cum + inORnot(alpha, n, mu = 2)
  return(cum / S)

}

 covPro(alpha = 0.05, S = 1000, n = 20)

## -----------------------------------------------------------------------------
set.seed(0)
n<-20
alpha <- .05
UCL <- replicate(1000, expr = { 
  x <- rchisq(n, df=2)
  (n-1) * var(x) / qchisq(alpha, df = n-1) 
  })
#count the number of intervals that contain sigma^2=4 
sum(UCL > 4) 
#or compute the mean to get the confidence level 
mean(UCL > 4)

# from the answer we can know that The t-interval(92.9%) is more robust to departures from normality than the interval for variance which is 80.6%.

## -----------------------------------------------------------------------------
n=1000
set.seed(0)
sk<- function(x) {
  #computes the sample skewness coeff
  xbar <- mean(x)
  m3 <- mean((x - xbar)^3) 
  m2 <- mean((x - xbar)^2) 
  return( m3 / m2^1.5 )
}
skew<-numeric(50)
for (i in 1:50){
  x<-rnorm(1000)
  skew[i]<-sk(x)
}
quantile(skew,c(0.025,0.05,0.95,0.975))

## -----------------------------------------------------------------------------
varq<-function(n,q,xq){
    return (q*(1-q)/(n*dnorm(xq,0,1)^2))
}
sqrt(varq(1000,c(0.025,0.05,0.95,0.975),quantile(skew,c(0.025,0.05,0.95,0.975))))

## -----------------------------------------------------------------------------
n<-1000
c2 <- qnorm(c(0.025,0.05,0.95,0.975), 0, sqrt(6/n))
c1<-unname(quantile(skew,c(0.025,0.05,0.95,0.975)))
c<-cbind(c1,c2)
print(c)


## -----------------------------------------------------------------------------
library(energy)

sk <- function(x) { 
#computes the sample skewness coeff. 
xbar <- mean(x) 
m3 <- mean((x - xbar)^3)
m2 <- mean((x - xbar)^2)
return( m3 / m2^1.5 )
}
alpha<-0.05
set.seed(0)
n<-30
m<-3300
para<-1:10
N<-length(para)
pwr<-numeric(N)
# critical value for the skewness test
cv<-qnorm(1-alpha/2,0,sqrt(6*(n-2)/((n+1)*(n+3))))

par(mar=c(1,1,1,1))
for (j in 1:N) {
  p<-para[j]
  sktests<-numeric(m)
  for (i in 1:m) {
    x<-rbeta(n,p,p)
    sktests[i] <- as.integer(
   shapiro.test(x)$p.value <= alpha)
  }
  pwr[j] <- mean(sktests)
}

#plot power vs parameter
plot(para,pwr,type='b',col="green")



for (j in 1:N) {
  p<-para[j]
  sktests<-numeric(m)
  for (i in 1:m) {
    y<-rt(n,p)
    sktests[i]<-as.integer(abs(sk(y))>=cv)
  }
  pwr[j]<-mean(sktests)
}

plot(para,pwr,type='b',col="red")

## -----------------------------------------------------------------------------
set.seed(0)
n<-50
alpha <- .05 
mu0<-1
m <- 10000
#number of replicates
p <- numeric(m) 
#storage for p-values 
for (j in 1:m) { 
  x <- rchisq(n, df=1)
  ttest <- t.test(x, alternative = "two.sided", mu = mu0) 
  p[j] <- ttest$p.value
  }
p.hat <- mean(p < alpha)
print(p.hat)

## -----------------------------------------------------------------------------
n<-50
alpha <- .05 
a<-1
mu0<-1
m <- 10000
#number of replicates
p <- numeric(m) 
#storage for p-values 
for (j in 1:m) { 
  x <- runif(n, min=0,max=2)
  ttest <- t.test(x, alternative = "two.sided", mu = mu0) 
  p[j] <- ttest$p.value
  }
p.hat <- mean(p < alpha)
print(p.hat)

## -----------------------------------------------------------------------------
n<-50
alpha <- .05 
a<-1
mu0<-1
m <- 10000
#number of replicates
p <- numeric(m) 
#storage for p-values 
for (j in 1:m) { 
  x <- rexp(n, 1)
  ttest <- t.test(x, alternative = "two.sided", mu = mu0) 
  p[j] <- ttest$p.value
  }
p.hat <- mean(p < alpha)
print(p.hat)

## -----------------------------------------------------------------------------
library(boot)
data(scor,package="bootstrap")
pairs(~mec+vec+alg+ana+sta,data=scor,main="The scatter plot of each pair of test scores")

## -----------------------------------------------------------------------------
library(corrplot)
res<-cor(scor, method = c("pearson", "kendall", "spearman"))
round(res,3)
corrplot(res, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

## -----------------------------------------------------------------------------
#set up the bootstrap 

#number of replicates
set.seed(0)
B <- 200
 
n <- nrow(scor)
#storage for replicates
R <- numeric(B) 

#bootstrap estimate of standard error of R 
for (b in 1:B) { 
  #randomly select the indices 
  i <- sample(1:n, size = n, replace = TRUE)
  mec <- scor$mec[i]
  vec <- scor$vec[i]
  R[b] <- cor(mec, vec)
}
#output print(
se.R12 <- sd(R) 
se.R12
hist(R, prob = TRUE)

## -----------------------------------------------------------------------------
#set up the bootstrap 

#number of replicates
B <- 200
 
n <- nrow(scor)
#storage for replicates
R <- numeric(B) 

#bootstrap estimate of standard error of R 
for (b in 1:B) { 
  #randomly select the indices 
  i <- sample(1:n, size = n, replace = TRUE)
  alg <- scor$alg[i]
  ana <- scor$ana[i]
  R[b] <- cor(alg, ana)
}
#output print(
se.R34 <- sd(R) 
se.R34
hist(R, prob = TRUE)

## -----------------------------------------------------------------------------
#set up the bootstrap 

#number of replicates
B <- 200
 
n <- nrow(scor)
#storage for replicates
R <- numeric(B) 

#bootstrap estimate of standard error of R 
for (b in 1:B) { 
  #randomly select the indices 
  i <- sample(1:n, size = n, replace = TRUE)
  alg <- scor$alg[i]
  sta <- scor$sta[i]
  R[b] <- cor(alg, sta)
}
#output print(
se.R35 <- sd(R) 
se.R35
hist(R, prob = TRUE)

## -----------------------------------------------------------------------------
#set up the bootstrap 

#number of replicates
B <- 200
 
n <- nrow(scor)
#storage for replicates
R <- numeric(B) 

#bootstrap estimate of standard error of R 
for (b in 1:B) { 
  #randomly select the indices 
  i <- sample(1:n, size = n, replace = TRUE)
  ana<- scor$ana[i]
  sta <- scor$sta[i]
  R[b] <- cor(ana, sta)
}
#output print(
se.R45 <- sd(R) 
se.R45
hist(R, prob = TRUE)

## -----------------------------------------------------------------------------
library(knitr)
library(kableExtra)
knitr::kable(cbind(se.R12,se.R34,se.R35,se.R45),formate = "html",col.names = c("sd of r12","sd of r34","sd of r35","sd of r45")) %>% kable_styling(position = "center")

## -----------------------------------------------------------------------------
n<-1e4
set.seed(1)
sk<- function(x,i) {
  #computes the sample skewness coeff
  xbar <- mean(x[i])
  m3 <- mean((x[i] - xbar)^3) 
  m2 <- mean((x [i]- xbar)^2) 
  return( m3 / m2^1.5 )
}

mu<-0;b<-1;m<-1e2;
library(boot)

ci.norm<-ci.basic<-ci.perc<-matrix(NA,m,2)
for(i in 1:m){ 
 U<-rnorm(m,0,1) 
  de <- boot(data=U,statistic=sk, R = 2000) 
  ci <- boot.ci(de ,type=c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3];ci.basic[i,]<-ci$basic[4:5] 
  ci.perc[i,]<-ci$percent[4:5]
}
cat('norm =',round(mean(ci.norm[,1]<=mu & ci.norm[,2]>=mu),4), 
    'basic =',mean(ci.basic[,1]<=mu & ci.basic[,2]>=mu), 'perc =',mean(ci.perc[,1]<=mu & ci.perc[,2]>=mu))

## -----------------------------------------------------------------------------
n<-1e4
set.seed(1)
sk<- function(x,i) {
  #computes the sample skewness coeff
  xbar <- mean(x[i])
  m3 <- mean((x[i] - xbar)^3) 
  m2 <- mean((x [i]- xbar)^2) 
  return( m3 / m2^1.5 )
}

mu<-sqrt(8/5) # skewness of chi-square(5)
b<-1;m<-1e2;
library(boot)

ci.norm<-ci.basic<-ci.perc<-matrix(NA,m,2)
for(i in 1:m){ 
 U<-rchisq(m,5) 
  de <- boot(data=U,statistic=sk, R = 2000) 
  ci <- boot.ci(de ,type=c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3];ci.basic[i,]<-ci$basic[4:5] 
  ci.perc[i,]<-ci$percent[4:5]
}
cat('norm =',round(mean(ci.norm[,1]<=mu & ci.norm[,2]>=mu),4), 
    'basic =',mean(ci.basic[,1]<=mu & ci.basic[,2]>=mu), 'perc =',mean(ci.perc[,1]<=mu & ci.perc[,2]>=mu))

## -----------------------------------------------------------------------------
library(bootstrap)
library(knitr)
n<-nrow(scor)
cov.hat<-(n-1)/n*cov(scor)
lamda_hat <- eigen(cov.hat)$values
## estimate theta_hat
theta_hat <- lamda_hat[1]/sum(lamda_hat)

n<-nrow(scor)
theta.j<-numeric(n)
ev<-matrix(nrow = 88,ncol = 5)

for (i in 1:n){ev[i,]<- eigen(cov(scor[-i,]))$values  
               theta.j[i]<-ev[i,1]/sum(ev[i,])}
# estimate bias_jack
bias.jack<-(n-1)*(mean(theta.j)-theta_hat)

# estimate se.jack
se.jack<-sqrt((n-1)*mean((theta.j-mean(theta.j))^2))


print(theta_hat)
print(bias.jack)
print(se.jack)

## -----------------------------------------------------------------------------
library(DAAG); attach(ironslag)

a <- seq(10, 40, .1) #sequence for plotting fits

n <- length(magnetic) #in DAAG ironslag 
e1 <- e2 <- e3 <- e4 <- numeric(n)

# fit models on leave-one-out samples 
for (k in 1:n) {
  y <- magnetic[-k] 
  x <- chemical[-k]
  
  # Linear model
  J1 <- lm(y ~ x) 
  yhat1 <- J1$coef[1] + J1$coef[2] * chemical[k]
  e1[k] <- magnetic[k] - yhat1

  # Quadratic model
  J2 <- lm(y ~ x + I(x^2)) 
  yhat2 <- J2$coef[1] + J2$coef[2] * chemical[k] + J2$coef[3] * chemical[k]^2
  e2[k] <- magnetic[k] - yhat2
  
  # Exponential model
  J3 <- lm(log(y) ~ x) 
  logyhat3 <- J3$coef[1] + J3$coef[2] * chemical[k] 
  yhat3 <- exp(logyhat3)
  e3[k] <- magnetic[k] - yhat3
  
  # cubic polynomial model
  J4 <- lm(y~x+I(x^2)+I(x^3))
  yhat4 <- J4$coef[1] + J4$coef[2] * chemical[k] + J4$coef[3] * chemical[k]^2 +J4$coef[4] * chemical[k]^3
  e4[k] <- magnetic[k] - yhat4
}
par(mar=c(1,1,1,1))
L1 <- lm(magnetic ~ chemical) 
plot(chemical, magnetic, main="Linear", pch=16) 
yhat1 <- L1$coef[1] + L1$coef[2] * a
lines(a, yhat1, lwd=2)

L2 <- lm(magnetic ~ chemical + I(chemical^2)) 
plot(chemical, magnetic, main="Quadratic", pch=16) 
yhat2 <- L2$coef[1] + L2$coef[2] * a + L2$coef[3] * a^2
lines(a, yhat2, lwd=2)

L3 <- lm(log(magnetic) ~ chemical)
plot(chemical, magnetic, main="Exponential", pch=16)
logyhat3 <- L3$coef[1] + L3$coef[2] * a 
yhat3 <- exp(logyhat3) 
lines(a, yhat3, lwd=2)

L4 <- lm(magnetic ~ chemical + I(chemical^2)+I(chemical^3)) 
plot(chemical, magnetic, main="Cubic", pch=16) 
yhat4 <- L4$coef[1] + L4$coef[2] * a + L4$coef[3] * a^2+L4$coef[4] * a^3
lines(a, yhat2, lwd=2)

print(c(mean(e1^2), mean(e2^2), mean(e3^2), mean(e4^2)))

## -----------------------------------------------------------------------------
print(summary(lm(L1))$adj.r.squared)
print(summary(lm(L2))$adj.r.squared)
print(summary(lm(L3))$adj.r.squared)
print(summary(lm(L4))$adj.r.squared)


## -----------------------------------------------------------------------------
# define the cout 5 test function
count5test <- function(x, y) {
X <- x - mean(x)
Y <- y - mean(y)
outx <- sum(X > max(Y)) + sum(X < min(Y))
outy <- sum(Y > max(X)) + sum(Y < min(X))


return(as.integer(max(c(outx, outy)) > 5))
}


n1<-20 
n2<-50
u1<-u2<-0
sigma1<-sigma2<-1


x<-rnorm(n1,u1,sigma1)
y<-rnorm(n2,u2,sigma2)
z<-c(x,y)
# cycle times
R<-1e4
n<-(n1+n2)/2
pool<-c(1:(n1+n2))
# storage of result
temp_result<-numeric(R)
# loop
for (i in 1:R) {
  k<-sample(pool,size=n,replace = FALSE)
  x<-z[k]
  y<-z[-k]
  temp_result[i]<-count5test(x,y)
}
# show the result
alpha_hat<-mean(temp_result)

# MC method
mm<-1e3
MC_method_result<-numeric(mm)
for (i in 1:mm) {
  xx<-rnorm(n1,u1,sigma1)
  yy<-rnorm(n2,u2,sigma2)
  zz<-c(xx,yy)
  temp_r2<-numeric(R)
  for (j in 1:R) {
  k<-sample(pool,size=n,replace = FALSE)
  nx<-zz[k]
  ny<-zz[-k]
  temp_r2[j]<-count5test(nx,ny)
  }
  MC_method_result[i]<-mean(temp_r2)
}
MC_alpha_hat<-mean(MC_method_result)
lable<-c(" one time","permutation in MC method")
result<-c(alpha_hat,MC_alpha_hat)
data.frame(lable,result)


## -----------------------------------------------------------------------------
library(Ball)
library(mvtnorm)
library(boot)
library(ggplot2)
# distance correlation function
dCov <- function(x, y) {
  x <- as.matrix(x); y <- as.matrix(y)
  n <- nrow(x); m <- nrow(y)
  if (n != m || n < 2) stop("Sample sizes must agree")
  if (! (all(is.finite(c(x, y)))))
  stop("Data contains missing or infinite values")
  Akl <- function(x) {
  d <- as.matrix(dist(x))
  m <- rowMeans(d); M <- mean(d)
  a <- sweep(d, 1, m); b <- sweep(a, 2, m)
  b + M
  }
A<- Akl(x); B <- Akl(y)
sqrt(mean(A * B))
}
ndCov2 <- function(z, ix, dims) {
#dims contains dimensions of x and y
p <- dims[1]
q <- dims[2]
d <- p + q
x <- z[ , 1:p] #leave x as is
y <- z[ix, -(1:p)] #permute rows of y
return(nrow(z) * dCov(x, y)^2)
}
# generate sample
n<-seq(from=10,to=100,by=10)
# loop
k<-100
# significant level
alpha<-0.05
pow_dCor_Model1<-pow_ball_Model1<-pow_dCor_Model2<-pow_ball_Model2<-numeric(length(n))
for (j in 1:length(n)) {

  #storage of temp data
  p_ball1<-numeric(k)
  dcor1<-numeric(k)
  p_ball2<-numeric(k)
  dcor2<-numeric(k)
  dcor1<-dcor2<-p_ball1<-p_ball2<-numeric(k)
  for (i in 1:k) {
    set.seed(i)
    # the function "rmvnorm" is used to 
    # generate the multidimensional normal data
    X<-rmvnorm(n[j],rep(0,2),diag(1,2))
    err<-rmvnorm(n[j],rep(0,2),diag(1,2))
    Y1<-(X/4)+err
    Y2<-(X/4)*err
    Z1<-cbind(X,Y1)
    Z2<-cbind(X,Y2)
    t1<-bcov.test(X,Y2,R=99)
    p_ball1[i]<-t1$p.value
    boot.obj1<-boot(data=Z1,statistic=ndCov2,R=99,sim="permutation",dims=c(2, 2))
    temp1<-c(boot.obj1$t0, boot.obj1$t)
    dcor1[i]<-mean(temp1>=temp1[1])
    
    t2<-bcov.test(X,Y2,R=99)
    p_ball2[i]<-t2$p.value
    boot.obj2<-boot(data=Z2,statistic=ndCov2,R=99,sim="permutation",dims=c(2, 2))
    temp2<-c(boot.obj2$t0, boot.obj2$t)
    dcor2[i]<-mean(temp2>=temp2[1])
    }
  pow_dCor_Model1[j]<-mean(dcor1<alpha)
  pow_ball_Model1[j]<-mean(p_ball1<alpha)
  pow_dCor_Model2[j]<-mean(dcor2<alpha)
  pow_ball_Model2[j]<-mean(p_ball2<alpha)  
}
dat<-data.frame(pow_dCor_Model1,pow_ball_Model1,pow_dCor_Model2,pow_ball_Model2)

ggplot(dat,aes(n))+geom_point(y=pow_dCor_Model1,fill="white")+geom_line(y=pow_dCor_Model1,colour="orange")+geom_point(y=pow_ball_Model1,fill="white")+geom_line(y=pow_ball_Model1,colour="green")

ggplot(dat,aes(n))+geom_point(y=pow_dCor_Model2,fill="white")+geom_line(y=pow_dCor_Model2,colour="orange")+geom_point(y=pow_ball_Model2,fill="white")+geom_line(y=pow_ball_Model2,colour="green")

## -----------------------------------------------------------------------------
set.seed(0)
lap_fun<-function(x){
  return(1/2*exp(-abs(x)))
}
rw.Metropolis <- function(n, sigma, x0, N) {
  x <- numeric(N)
  x[1] <- x0
  u <- runif(N)
  k<-0 
  for (i in 2:N) {
    y <- rnorm(1, x[i-1], sigma) 
    if (u[i] <= (lap_fun(y)/ lap_fun(x[i-1])))
      x[i] <- y else
        { x[i] <- x[i-1]
        k<-k+1
} }
return(list(x=x, k=k)) 
}

N <- 2000 
sigma <- c(.05, .5, 2, 16)
x0 <- 25
rw1 <- rw.Metropolis(n, sigma[1], x0, N) 
rw2 <- rw.Metropolis(n, sigma[2], x0, N) 
rw3 <- rw.Metropolis(n, sigma[3], x0, N)
rw4 <- rw.Metropolis(n, sigma[4], x0, N)

#number of candidate points rejected 
print(c(rw1$k, rw2$k, rw3$k, rw4$k))

library(knitr)
library(kableExtra)
knitr::kable(cbind(1-rw1$k/N,1-rw2$k/N,1-rw3$k/N,1-rw4$k/N),formate = "html",col.names = c("acceptance rates of c1","acceptance rates of c2","acceptance rates of c3","acceptance rates  of c4")) %>% kable_styling(position = "center")

## -----------------------------------------------------------------------------
library(VGAM)
par(mar=c(1,1,1,1)) 
    refline <- qlaplace(c(.025, .975))
    rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
    for (j in 1:4) {
        plot(rw[,j], type="l",
             xlab=bquote(sigma == .(round(sigma[j],3))),
             ylab="X", ylim=range(rw[,j]))
        abline(h=refline)
    }

## -----------------------------------------------------------------------------
 par(mfrow=c(1,1)) #reset to default
    
    a <- c(.05, seq(.1, .9, .1), .95)
    Q <- qlaplace(a, 0, 1)
    rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
    mc <- rw[501:N, ]
    Qrw <- apply(mc, 2, function(x) quantile(x, a))
    qq <- data.frame(round(cbind(Q, Qrw), 3))
    names(qq) <- c('True','sigma=0.05','sigma=0.5','sigma=2','sigma=16')
    knitr::kable(qq,format='html')%>% kable_styling(position = "center")

## -----------------------------------------------------------------------------
x1<-log(exp(100))
y1<-exp(log(100))
x2<-log(exp(15))
y2<-exp(log(15))

isTRUE(all.equal(x1,y1))
identical(x1,y1)

isTRUE(all.equal(x2,y2))
identical(x2,y2)

## -----------------------------------------------------------------------------
# find the intervals in which the roots fall  
f <- function(a, k) 1-pt(sqrt(a^2*k/(k+1-a^2)), k)
k <- c(4, 25, 100, 1000)
par(mar=c(1,1,1,1))
for (i in k) {
  g <- function(x) f(x, i-1)-f(x, i)
    a <- seq(0, sqrt(i), .1)
  plot(a, g(a), type = 'l', main = paste('k=',i))
  abline(h = 0)
}

## -----------------------------------------------------------------------------
# Exercise 11.4
f <- function(a, k) 1-pt(sqrt(a^2*k/(k+1-a^2)), k)
k <- c(4:25, 100, 500, 1000)
Ak <- numeric(length(k))
i <- 1
for (j in k) {
  g <- function(x) f(x, j-1)-f(x, j)
  Ak[i] <- uniroot(g, lower = 1, upper = 2)$root
  i <- i+1
}
knitr::kable(cbind(k,Ak))

## -----------------------------------------------------------------------------
# Exercise 11.5
f <- function(k) 2/sqrt(pi*k)*exp(lgamma((k+1)/2)-lgamma(k/2))
ck <- function(a, k) sqrt(a^2*k/(k+1-a^2))
g <- function(u, k) (1+u^2/k)^(-(k+1)/2)
k <- c(4:25, 100, 500, 1000)
root <- numeric(length(k))
i <- 1
for (j in k) {
  ff <- function(a) f(j)*integrate(function(u) {g(u, j)}, 0, ck(a, j))$value -       f(j-1)*integrate(function(u) {g(u, j-1)}, 0, ck(a, j-1))$value 
  root[i] <- uniroot(ff, lower = 1, upper = 2)$root
  i <- i+1
}
knitr::kable(cbind(k, Ak, root))

## ----echo=FALSE---------------------------------------------------------------
    dat <- rbind(Genotype=c('AA','BB','OO','AO','BO','AB','Sum'),
                 Frequency=c('p^2','q^2','r^2','2pr','2qr','2pq',1),
                 Count=c('nAA','nBB','nOO','nAO','nBO','nAB','n'))
    knitr::kable(dat,format='latex')

## -----------------------------------------------------------------------------
nA <- 28
nB <- 24
nOO <- 41
nAB <- 70
# EM algorithm
theta0 <- c(.5, .3)
l <- numeric(1000)
for (j in 1:1000) {
  E <- function(theta) {
    p <- theta[1]
    q <- theta[2]
    r <- 1-p-q
    p0 <- theta0[1]
    q0 <- theta0[2]
    r0 <- 1-p0-q0
    return(2*nA*(log(p)+r0/(p0+2*r0)*log(2*r/p))+2*nB*(log(q)+r0/(q0+2*r0)*log(2*r/q))+2*nOO*log(r)+nAB*log(2*p*q))
  }
  Obj <- function(theta) -E(theta)
  optim <- optim(c(.1, .1), Obj)
  theta0 <- optim$par
  l[j] <- E(theta0)
}
print(theta0)
plot(l[1:10], type = 'o', xlab = 'iterations', ylab = 'log-maximum likelihood values')

## -----------------------------------------------------------------------------
# attach(mtcars)
data1<-as.matrix(mtcars)
mpg<-data1[,1]
disp<-data1[,3]
wt<-data1[,6]
formulas <- list( 
mpg ~ disp, 
mpg ~ I(1 / disp), 
mpg ~ disp + wt,
mpg ~ I(1 / disp) + wt
)
 out<-list(4)
 for (i in 1:4) {
 out[[i]]<-lm(formulas[[i]])
 }

 unlist(out[[1]]$coefficients)
 unlist(out[[2]]$coefficients)
 unlist(out[[3]]$coefficients)
  unlist(out[[4]]$coefficients)


## -----------------------------------------------------------------------------
#lapply
lapply(formulas,lm)

## ----warning=FALSE------------------------------------------------------------
set.seed(0)
# for loops
bootstraps <- lapply(1:10, function(i) {
rows <- sample(1:nrow(mtcars), rep = TRUE) 
mtcars[rows, ]
})
out2<-list()
c
for (i in 1:10){
        out2[i]<-lm(mpg ~ disp,bootstraps[i])
        #c[i]<-out[[i]]$coefficients
}
out2

## -----------------------------------------------------------------------------
lapply(bootstraps,lm,formula=mpg ~ disp)

## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared
a<-lapply(formulas,lm)
lapply(a,rsq)
b<-lapply(bootstraps,lm,formula=mpg ~ disp)
lapply(b,rsq)


## -----------------------------------------------------------------------------
trials <- replicate(
100, 
t.test(rpois(10, 10), rpois(7, 10)),
simplify = FALSE
)
c<-list()
for (i in 1:100){
        c[i]<-trials[[i]]$p.value
}
df<-data.frame(x=1:100,y=c)
sapply(df,class, simplify = "array", USE.NAMES = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  # mcsapply()
#  library(parallel)
#  boot_df <- function(x) x[sample(nrow(x), rep = T), ]
#  rsquared <- function(mod) summary(mod)$r.squared
#  boot_lm <- function(i) {
#    rsquared(lm(mpg ~ wt + disp, data = boot_df(mtcars)))
#  }
#  system.time(sapply(1:1e5, boot_lm))
#  system.time(unlist(mclapply(1:1e5, boot_lm, mc.cores = 4)))

## ----warning=FALSE------------------------------------------------------------
#the function written before
library(kableExtra)
library(GeneralizedHyperbolic)
rw.Metropolis <- function(sigma,x0,N) {
x <- numeric(N)
x[1] <- x0
u <- runif(N)
k <- 0
for (i in 2:N) {
y <- rnorm(1, x[i-1], sigma)
if (u[i] <= exp(abs(x[i-1]) - abs(y))){
x[i] <- y 
k <- k + 1}
else {
x[i] <- x[i-1]

} }
return(list(x=x, k=k))
}

#the function using Rcpp
library(Rcpp)
cppFunction('List rw_Metropolis(double sigma, double x0, int N) {
NumericVector x(N);
x[0] = x0;
int k = 0;
for (int i = 1;i < N;i++) {
double u = runif(1)[0];
double y = rnorm(1, x[i-1], sigma)[0];
if (u <= exp(abs(x[i-1]) - abs(y))){
x[i] = y;
k = k + 1;}
else 
x[i] = x[i-1];
}
List result = List::create(x,k);
return(result);
}')

#generate random samples
set.seed(123)
N <- 1000
sigma <- 1
x0 <- 0
sample1 <- rw.Metropolis(sigma,x0,N)$x
sample2 <- rw_Metropolis(sigma,x0,N)[[1]]

#qq plot
library(car)
qqplot(sample1, sample2, xlab = "the samples using R",
       ylab = "the samples using Rcpp")
x <- seq(-4,4,.01)
lines(x,x,col = "red")

#Campare the computation time
library(microbenchmark)
ts <- microbenchmark(rw1 = rw.Metropolis(sigma,x0,N),rw2 = rw_Metropolis(sigma,x0,N))
summary(ts)[,c(1,3,5,6)]

