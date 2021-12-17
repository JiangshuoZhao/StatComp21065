## -----------------------------------------------------------------------------
## sigma = 0.1
n <- 10000
Sigma <-0.1
u <- runif(n)
x <- sqrt(-2 * Sigma^2 * log(1-u))
hist(x, prob =TRUE, main = expression(sigma==0.1) ) # generated sample mode
y <- seq(0,0.5, .001)
lines(y,y/(Sigma^2)*exp(-y^2/(2*Sigma^2))) #the theoretical mode

## -----------------------------------------------------------------------------
## sigma = 0.5
n <- 10000
Sigma <-0.5
u <- runif(n)
x <- sqrt(-2 * Sigma^2 * log(1-u))
hist(x, prob =TRUE, main = expression(sigma==0.5) ) # generated sample mode
y <- seq(0,3, .001)
lines(y,y/(Sigma^2)*exp(-y^2/(2*Sigma^2))) #the theoretical mode

## -----------------------------------------------------------------------------
## sigma = 1
n <- 10000
Sigma <-1
u <- runif(n)
x <- sqrt(-2 * Sigma^2 * log(1-u))
hist(x, prob =TRUE, main = expression(sigma==1) ) # generated sample mode
y <- seq(0,4, .001)
lines(y,y/(Sigma^2)*exp(-y^2/(2*Sigma^2))) #the theoretical mode

## -----------------------------------------------------------------------------
## p1=0.75
n <-1000
x1 <- rnorm(n)
x2 <- rnorm(n, 3, 1)
p1 = rbinom(n,1,0.75)
x = p1*x1 + (1-p1)*x2
hist(x,prob =TRUE, main = expression(p1==0.75))

## -----------------------------------------------------------------------------
## p1=0.8
n <-1000
x1 <- rnorm(n)
x2 <- rnorm(n, 3, 1)
p1 = rbinom(n,1,0.8)
x = p1*x1 + (1-p1)*x2
hist(x,prob =TRUE, main = expression(p1==0.8))

## -----------------------------------------------------------------------------
## p1=0.5
n <-1000
x1 <- rnorm(n)
x2 <- rnorm(n, 3, 1)
p1 = rbinom(n,1,0.5)
x = p1*x1 + (1-p1)*x2
hist(x,prob =TRUE, main = expression(p1==0.5))

## -----------------------------------------------------------------------------
## p1=0.3
n <-1000
x1 <- rnorm(n)
x2 <- rnorm(n, 3, 1)
p1 = rbinom(n,1,0.3)
x = p1*x1 + (1-p1)*x2
hist(x,prob =TRUE, main = expression(p1==0.3))

## -----------------------------------------------------------------------------
## p1=0.35
n <-1000
x1 <- rnorm(n)
x2 <- rnorm(n, 3, 1)
p1 = rbinom(n,1,0.35)
x = p1*x1 + (1-p1)*x2
hist(x,prob =TRUE, main = expression(p1==0.35))

## -----------------------------------------------------------------------------
n <- 1e4
t <-10;r <- 1; beta <-1 ; lambda <-1 ;
N_t <- rpois(n, lambda*t)
x_t <- rep(0,n)
for (i in 1:n) {
  y <- rgamma(N_t[i], r, beta)
  x_t[i] <- sum(y)
}
hist(x_t)
cat("mean:",mean(x_t))
cat("variance:",var(x_t))
#print(mean(x_t))
#print(var(x_t))

## -----------------------------------------------------------------------------
## sigma = 0.1
n <- 10000
Sigma <-0.1
u <- runif(n)
x <- sqrt(-2 * Sigma^2 * log(1-u))
hist(x, prob =TRUE, main = expression(sigma==0.1) ) # generated sample mode
y <- seq(0,0.5, .001)
lines(y,y/(Sigma^2)*exp(-y^2/(2*Sigma^2))) #the theoretical mode

## -----------------------------------------------------------------------------
## sigma = 0.5
n <- 10000
Sigma <-0.5
u <- runif(n)
x <- sqrt(-2 * Sigma^2 * log(1-u))
hist(x, prob =TRUE, main = expression(sigma==0.5) ) # generated sample mode
y <- seq(0,3, .001)
lines(y,y/(Sigma^2)*exp(-y^2/(2*Sigma^2))) #the theoretical mode

## -----------------------------------------------------------------------------
## sigma = 1
n <- 10000
Sigma <-1
u <- runif(n)
x <- sqrt(-2 * Sigma^2 * log(1-u))
hist(x, prob =TRUE, main = expression(sigma==1) ) # generated sample mode
y <- seq(0,4, .001)
lines(y,y/(Sigma^2)*exp(-y^2/(2*Sigma^2))) #the theoretical mode

## -----------------------------------------------------------------------------
## p1=0.75
n <-1000
x1 <- rnorm(n)
x2 <- rnorm(n, 3, 1)
p1 = sample(0:1, size = n, replace = TRUE, prob = c(.25, .75))
x = p1*x1 + (1-p1)*x2 
hist(x,prob =TRUE, main = expression(p1==0.75))

## -----------------------------------------------------------------------------
## p1=0.8
n <-1000
x1 <- rnorm(n)
x2 <- rnorm(n, 3, 1)
p1 = rbinom(n,1,0.8)
x = p1*x1 + (1-p1)*x2
hist(x,prob =TRUE, main = expression(p1==0.8))

## -----------------------------------------------------------------------------
## p1=0.5
n <-1000
x1 <- rnorm(n)
x2 <- rnorm(n, 3, 1)
p1 = rbinom(n,1,0.5)
x = p1*x1 + (1-p1)*x2
hist(x,prob =TRUE, main = expression(p1==0.5))

## -----------------------------------------------------------------------------
## p1=0.3
n <-1000
x1 <- rnorm(n)
x2 <- rnorm(n, 3, 1)
p1 = rbinom(n,1,0.3)
x = p1*x1 + (1-p1)*x2
hist(x,prob =TRUE, main = expression(p1==0.3))

## -----------------------------------------------------------------------------
## p1=0.35
n <-1000
x1 <- rnorm(n)
x2 <- rnorm(n, 3, 1)
p1 = rbinom(n,1,0.35)
x = p1*x1 + (1-p1)*x2
hist(x,prob =TRUE, main = expression(p1==0.35))

## -----------------------------------------------------------------------------
## alpha = 1, beta = 1, lambda = 1   
n <- 1e4
t <-10;alpha <- 1; beta <-1 ; lambda <-1 ;
N_t <- rpois(n, lambda*t)
x_t <- rep(0,n)
for (i in 1:n) {
  y <- rgamma(N_t[i], alpha, beta)
  x_t[i] <- sum(y)
}
hist(x_t)
cat("mean:",mean(x_t))
cat("variance:",var(x_t))

## -----------------------------------------------------------------------------
## alpha = 2, beta = 3, lambda = 4   
n <- 1e4
t <-10;alpha <- 2; beta <-3 ; lambda <-4 ;
N_t <- rpois(n, lambda*t)
x_t <- rep(0,n)
for (i in 1:n) {
  y <- rgamma(N_t[i], alpha, beta)
  x_t[i] <- sum(y)
}
hist(x_t)
cat("mean:",mean(x_t))
cat("variance:",var(x_t))

## -----------------------------------------------------------------------------
MC_Beta <- function(x, alpha=3, beta=3, n=1000) {
  u <- runif(n)
  thetahat <- numeric(length(x))
  for (i in 1:length(x)){ 
  thetahat[i] <- mean(x[i]* (x[i]*u)^(alpha-1)* (1-u*x[i])^(beta-1) /beta(alpha, beta))
  }
  thetahat
}

## -----------------------------------------------------------------------------
x = seq(0.1, 0.9, 0.1)
estimate <- round(MC_Beta(x),5)
theoretical <- pbeta(x, 3,3)
data.frame(estimate, theoretical,row.names = x)
plot(x, estimate,'l',col =1,)
lines(x, theoretical, col =2,)
gs = c(expression((X1+X2)/2), expression((X+X^{prime})/2) )
legend("topleft", legend = gs,
            lwd = 2, inset = 0.02,col=1:2)

## -----------------------------------------------------------------------------
sampling <- function(n=1, sigma = 1,antithetic = TRUE){
  U = runif(n)
  if (antithetic) X = sqrt(-2*log(U[1:(n)]))*sigma +  sqrt(-2*log(1-U[1:(n)]))*sigma else X = sqrt(-2*log(U))*sigma + sqrt(-2*log(U))*sigma
  X/2
}

## -----------------------------------------------------------------------------
m <- 1000
MC1 <- sampling(n = 1000, antithetic = FALSE)
MC2 <- sampling(n = 1000)
print((var(MC1)-var(MC2))/var(MC1))

## -----------------------------------------------------------------------------
x <- seq(1, 10, .01)
g <- x^2/sqrt(2*pi)*exp(-x^2/2) * (x > 1)
f1 <- 1/sqrt(2*pi) *exp(-x^2/2)
f2 <-  exp(-x)
gs <- c(expression(g/f[1](x)==-x^2),
            expression(g/f[2](x)==1/sqrt(2*pi)*e^{(-x^2/2)+x}))
plot(x, g/f1, type = "l", ylab = "",
       ylim = c(0,3.2), lwd = 2, lty = 2,col = 2)
lines(x, g/f2, lty = 3, lwd=2,col=3)
legend("topright", legend = gs,
           lty = 2:3, lwd = 2, inset = 0.02,col=2:3)

## -----------------------------------------------------------------------------
m <- 10000
est <- sd <- numeric(2)
g <- function(x) {
  x^2/sqrt(2*pi)*exp(-x^2/2) * (x > 1)
}
x <- rnorm(m) #using normal distribution f1 =1/sqrt(2*pi) *exp(-x^2/2)
fg <- x^2 * (x>1) # g/f = x^2 (x>1)
est[1] <- mean(fg)
sd[1] <- sd(fg)/sqrt(m)
x <- rexp(m, 1) #using exponential distribution f2 = e^(-x)
fg <- g(x) / exp(-x)
est[2] <- mean(fg)
sd[2] <- sd(fg)/sqrt(m)
res <- rbind(est=round(est,5), sd=round(sd,5))
colnames(res) <- paste0('f',1:2)
knitr::kable(res,align='c')

## -----------------------------------------------------------------------------
m<-1e4
n <-20
alpha <- .05
L <- U <- numeric(m)
for(i in 1:m){
  x <- rchisq(n, df = 2)
  L[i] <- mean(x)-(sd(x)/sqrt(n-1)) * qt(1-alpha/2,df = n-1)
  U[i] <- mean(x)+(sd(x)/sqrt(n-1)) * qt(1-alpha/2,df = n-1)
}
mean(L<2 & U>2)


## -----------------------------------------------------------------------------
mu0 <- 1 # null hypothesis!
alpha <- .05
m <- 1e4; n <- 20; set.seed(123)
p1 <- p2 <- p3 <- numeric(m)
for (i in 1:m){
  x1 <- rchisq(n,df =1)
  x2 <- runif(n,0,2)
  x3 <- rexp(n,1)
  ttest1 <-  t.test(x1, alternative = "greater", mu = mu0)
  ttest2 <-  t.test(x2, alternative = "greater", mu = mu0)
  ttest3 <-  t.test(x3, alternative = "greater", mu = mu0)
  p1[i] <- ttest1$p.value
  p2[i] <- ttest2$p.value
  p3[i] <- ttest3$p.value
}

p.hat <- c(mean(p1 < alpha), mean(p2 < alpha), mean(p3 < alpha))

se.hat <- sqrt(p.hat*(1-p.hat)/m)

data.frame(p.hat, se.hat,row.names = c("(i)χ2(1)", "(ii)Uniform(0,2)", "(iii)Exponential(1)"))

## -----------------------------------------------------------------------------
library(MASS) # genrate multi-normal distribution
n <- c(10, 20, 30, 50, 100, 500) # sample size
d <- 2 # dimension
cv_U <- qchisq(0.975, d*(d+1)*(d+2)/6)
cv_L <- qchisq(0.025, d*(d+1)*(d+2)/6)
sigma <- (matrix(c(5,0,0,1),2,2))
mu<-c(1,4)

mul_sk <- function(x){
  n <- nrow(x)
  x_bar <- matrix(c(mean(x[,1]),mean(x[,2])),n,2, byrow=TRUE)
  x_ <- x-x_bar
  sigma_hat <- var(x)*(n-1)/n # MLE
  b <- mean((x_ %*% solve(sigma_hat) %*% t(x_))^3)
  return(n*b/6)
}

p.reject <- numeric(length(n)) #to store sim. results
m <- 1000 #num. repl. each sim.

for (i in 1:length(n)) {
  sktests <- numeric(m) #test decisions
  for (j in 1:m) {
    x<-mvrnorm(n[i],mu,sigma)
    sktests[j] <- as.integer((mul_sk(x) <=cv_L) |( mul_sk(x)>= cv_U))
  }
  p.reject[i] <- mean(sktests) #proportion rejected
}

print(p.reject)

## -----------------------------------------------------------------------------

alpha <- .1
n <- 30
m <- 2500
sigma1 <- matrix(c(5,0,0,1),2,2)
sigma2 <- matrix(c(8,0,0,1),2,2)
mu<-c(0,0)
epsilon <- c(seq(0, .15, .01), seq(.15, 1, .05))
N <- length(epsilon)
pwr <- numeric(N) #critical value for the skewness test
cv_U <- qchisq(1-alpha/2, d*(d+1)*(d+2)/6)
cv_L <- qchisq(alpha/2, d*(d+1)*(d+2)/6)
for (j in 1:N) { #for each epsilon
  e <- epsilon[j]
  sktests <- numeric(m)
  for (i in 1:m) {
  #for each replicate
    a <- sum(sample(c(0,1), replace = TRUE,size = n, prob = c(1-e, e)))
    if(a==0){x<-mvrnorm(n,mu,sigma1)}
    else if(a==n){x<-mvrnorm(n,mu,sigma2)}
    else {x1 <- mvrnorm(n-a,mu,sigma1)
          x2 <- mvrnorm(a,mu,sigma2)
          x <- rbind(x1,x2)
          }
    sktests[i] <- as.integer(mul_sk(x) <= cv_L | mul_sk(x) >=cv_U )
  }
  pwr[j] <- mean(sktests)
}
#plot power vs epsilon
plot(epsilon, pwr, type = "b",
xlab = bquote(epsilon), ylim = c(0,1))
abline(h = .1, lty = 3)
se <- sqrt(pwr * (1-pwr) / m)
#add standard errors
lines(epsilon, pwr+se, lty = 3)
lines(epsilon, pwr-se, lty = 3)

## -----------------------------------------------------------------------------
library(bootstrap)
sigma.hat <- cov(scor)
eig <- eigen(sigma.hat)
theta.hat <- (eig$value[1])/sum(eig$values)

B <- 200
n <- nrow(scor)
theta <- numeric(B)

for (b in 1:B){
  i <- sample(1:n, size=n, replace = TRUE)
  x <- scor[i,]
  sigma.i <- cov(x)
  eig <- eigen(sigma.i)
  theta[b] <- (eig$values[1])/sum(eig$values)
}
bias.theta <- mean(theta.hat-theta)
se.theta <- sd(theta)
print(list(bias = bias.theta, se=se.theta))

## -----------------------------------------------------------------------------
sigma.hat <- cov(scor)
eig <- eigen(sigma.hat)
theta.hat <- (eig$value[1])/sum(eig$values)

n <- nrow(scor)
theta <- numeric(n)
for (i in 1:n){
  
  sigma.i <- cov(scor[-i,])
  eig <- eigen(sigma.i)
  theta[i] <- (eig$values[1])/sum(eig$values)
}

bias.theta <- (n-1)*(mean(theta)-theta.hat)

se.theta <- sqrt((n-1)*mean((theta-mean(theta))^2)) 

print(list(bias = bias.theta, se=se.theta))


## -----------------------------------------------------------------------------
library(boot)
sigma.hat <- cov(scor)
eig <- eigen(sigma.hat)
theta.hat <- (eig$value[1])/sum(eig$values)

theta.boot <- function(x,ind){
  sigma.i <- cov(x[ind,])
  eig <- eigen(sigma.i)
  eig$values[1] / sum(eig$values)
}

boot.obj <- boot(scor,statistic = theta.boot, R = 100)

print(boot.ci(boot.obj,type=c("perc", "bca")))


## -----------------------------------------------------------------------------
m=1000
n=100
mu=0
t=numeric(n)
set.seed(1234)
library(boot)
ci.norm=ci.basic=ci.perc=matrix(NA,m,2)

sk=function(x){ 
  x.bat = mean(x)
  m1 = mean((x - x.bat)^3)
  m2 = mean((x - x.bat)^2)
  return (m1/m2^1.5)
}

boot.median=function(x,ind) sk(x[ind])
# normal populations
for(i in 1:m){

  x=rnorm(n)
  boot.obj=boot(data=x,statistic=boot.median,R=100)
  ci=boot.ci(boot.obj,type = c("norm","basic","perc"))
  ci.norm[i,]=ci$norm[2:3]
  ci.basic[i,]=ci$basic[4:5]
  ci.perc[i,]=ci$percent[4:5]
  
}
p1.norm=c(mean(ci.norm[,1]<=mu&ci.norm[,2]>=mu),mean(ci.norm[,1]>mu),mean(ci.norm[,2]<mu))
p1.basic=c(mean(ci.basic[,1]<=mu&ci.basic[,2]>=mu),mean(ci.basic[,1]>mu),mean(ci.basic[,2]<mu))
p1.perc=c(mean(ci.perc[,1]<=mu&ci.perc[,2]>=mu),mean(ci.perc[,1]>mu),mean(ci.perc[,2]<mu))
data.frame(p1.norm,p1.basic,p1.perc,row.names = c("coverage","on the left","on the right"))
# χ2(5) distributions
library(moments)
mu1=(8/5)^(1/2)
t=numeric(n)

for(i in 1:m){

  x=rchisq(n,5)
  de=boot(data=x,statistic=boot.median,R=100)
  ci=boot.ci(de,type = c("norm","basic","perc"))
  ci.norm[i,]=ci$norm[2:3]
  ci.basic[i,]=ci$basic[4:5]
  ci.perc[i,]=ci$percent[4:5]
}
p2.norm=c(mean(ci.norm[,1]<=mu1&ci.norm[,2]>=mu1),mean(ci.norm[,1]>mu1),mean(ci.norm[,2]<mu1))
p2.basic=c(mean(ci.basic[,1]<=mu1&ci.basic[,2]>=mu1),mean(ci.basic[,1]>mu1),mean(ci.basic[,2]<mu1))
p2.perc=c(mean(ci.perc[,1]<=mu1&ci.perc[,2]>=mu1),mean(ci.perc[,1]>mu1),mean(ci.perc[,2]<mu1))
data.frame(p2.norm,p2.basic,p2.perc,row.names = c("coverage","on the left","on the right"))

## -----------------------------------------------------------------------------
R <- 999
set.seed(1)
x <- rnorm(12)
y <- rnorm(12)
z <- c(x,y)
K <- 1:24
t <- numeric(R)
t0 <- cor(x,y,method = "spearman")

for (i in(1:R)){
  k <- sample(K, size = 12,replace = FALSE)
  x1 <- z[k]
  y1 <- z[-k]
  t[i] <- cor(x1, y1, method = "spearman")
}
p1<- mean(c(t0,t) >= t0)
p2<- cor.test(x,y)$p.value
c(p1,p2)

## -----------------------------------------------------------------------------
library(RANN) 
library(boot)
library(energy)
library(Ball)


Tn <- function(z, ix, sizes,k) {
n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
if(is.vector(z)) z <- data.frame(z);
z <- z[ix, ];
NN <- nn2(data=z, k=k+1)
block1 <- NN$nn.idx[1:n1,-1]
block2 <- NN$nn.idx[(n1+1):n,-1]
i1 <- sum(block1 <= n1); i2 <- sum(block2 > n1)
(i1 + i2) / (k * n)
}

eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,
  sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)}

m <- 1000
k <- 3
n1 <- n2 <- 50
R <- 99
N <- c(n1, n2)
p1 <- p2 <- p3 <- p4 <- p5 <- matrix(NA,m,3)

#Unequal variances and equal expectations
for(i in 1:m){
  x1 <- matrix(rnorm(100), ncol=2)
  y1 <- matrix(rnorm(100,sd=1.5),ncol=2)
  z1 <- rbind(x1, y1)
  p1[i,1]<-eqdist.nn(z1,N,k)$p.value
  p1[i,2]<-eqdist.etest(z1,sizes=N,R=R)$p.value
  p1[i,3]<-bd.test(x=x1,y=y1,R=R,seed=i*12345)$p.value
}
pow1 <- colMeans(p1<0.05)
pow1

## -----------------------------------------------------------------------------
#Unequal variances and unequal expectations
for(i in 1:m){
  x2 <- matrix(rnorm(100), ncol=2)
  y2 <- matrix(rnorm(100,mean=0.5,sd=1.5),ncol=2)
  z2 <- rbind(x2, y2)
  p2[i,1]<-eqdist.nn(z2,N,k)$p.value
  p2[i,2]<-eqdist.etest(z2,sizes=N,R=R)$p.value
  p2[i,3]<-bd.test(x=x2,y=y2,R=R,seed=i*12345)$p.value
}
pow2 <- colMeans(p2<0.05)
pow2

## -----------------------------------------------------------------------------
#Non-normal distributions: t distribution with 1 df，bimodel distribution
for(i in 1:m){
  x3 <- matrix(rt(100,df=1), ncol=2)
  y<-numeric()
  for(j in 1:100){
      r<-sample(c(0,1),1,replace=TRUE,prob=c(0.3,0.7))
      y[j]<-ifelse(r==0,rnorm(1,0.5,2),rnorm(1,1,3))
  }
  y3<-matrix(y,ncol=2)
  z3 <- rbind(x3, y3)
  p3[i,1]<-eqdist.nn(z3,N,k)$p.value
  p3[i,2]<-eqdist.etest(z3,sizes=N,R=R)$p.value
  p3[i,3]<-bd.test(x=x3,y=y3,R=R,seed=i*12345)$p.value
}
pow3 <- colMeans(p3<0.05)
pow3


## -----------------------------------------------------------------------------
#unbalanced samples
n3<-25;n4<-250;n5 <- n3+n4;N1<-c(n3,n4)
for(i in 1:m){
  x4 <- matrix(rnorm(50,mean=0.5,sd=1), ncol=2)
  y4 <- matrix(rnorm(500,mean=0.5,sd=1.5),ncol=2)
  z4 <- rbind(x4, y4)
  p4[i,1]<-eqdist.nn(z4,N1,k)$p.value
  p4[i,2]<-eqdist.etest(z4,sizes=N1,R=R)$p.value
  p4[i,3]<-bd.test(x=x4,y=y4,R=R,seed=i*12345)$p.value
}
pow4 <- colMeans(p4<0.05)
pow4

## -----------------------------------------------------------------------------
set.seed(12345)
rw.Metropolis <- function(sigma, x0, N) {
    x <- numeric(N)
    x[1] <- x0
    u <- runif(N)
    k <- 0
    for (i in c(2:N)) {
        y <- rnorm(1, x[i-1],sigma)
        if (u[i] < ((1+x[i-1]^2) / (1 + y^2)))
            x[i] <- y else {
            x[i] <- x[i-1]
            k <- k + 1
        }
    }
    return(list(x=x, k=k))
}

N <- 10000
sigma <- c(.05, .5, 2,  4)

x0 <- 20

rw1 <- rw.Metropolis( sigma[1], x0, N)
rw2 <- rw.Metropolis( sigma[2], x0, N)
rw3 <- rw.Metropolis( sigma[3], x0, N)
rw4 <- rw.Metropolis( sigma[4], x0, N)

#number of candidate points rejected
# no.reject <- data.frame(theta=theta,no.reject=c(rw1$k, rw2$k, rw3$k, rw4$k))
# # knitr::kable(no.reject)
# no.reject

## ----eval=FALSE---------------------------------------------------------------
#      par(mfrow=c(2,2))  #display 4 graphs together
#      refline <- qcauchy(c(.025, .975))
#      rw <- cbind(rw1$x, rw2$x, rw3$x,  rw4$x)
#      for (j in 1:4) {
#          plot(rw[,j], type="l",
#               xlab=bquote(sigma == .(round(sigma[j],3))),
#               ylab="X", ylim=range(rw[,j]))
#          abline(h=refline)
#      }
#      par(mfrow=c(1,1)) #reset to default
#  

## ----eval=FALSE---------------------------------------------------------------
#  
#      a <- c(.05, seq(.1, .9, .1), .95)
#      Q <- qcauchy(a)
#      rw <- cbind(rw1$x, rw2$x, rw3$x, rw4$x)
#      mc <- rw[501:N, ]
#      Qrw <- apply(mc, 2, function(x) quantile(x, a))
#      qq <- data.frame(round(cbind(Q, Qrw), 3))
#      names(qq) <- c('True','sigma=0.05','sigma=0.5','sigma=1','sigma=2')
#      knitr::kable(qq) #latex format
#  

## ----echo=F-------------------------------------------------------------------
N <- 5000 #length of chain
burn <- 1000 #burn-in length
X <- matrix(0, N, 2) #the chain, a bivariate sample
mu1=0.5
mu2=0.5
n = 2
a = 1
b = 1
###### generate the chain #####

X[1, ] <- c(mu1, mu2) #initialize
for (i in 2:N) {
  x2 <- X[i-1, 2]

  X[i, 1] <- rbinom(1,n,x2)

  x1 <- X[i, 1]

  X[i, 2] <- rbeta(1,x1+a,n-x1+b)
  
}
x <- X[(burn+1):N, ]
cat('Means: ',round(colMeans(x),2))
cat('Standard errors: ',round(apply(x,2,sd),2))
cat('Correlation coefficients: ', round(cor(x[,1],x[,2]),2))

## ----echo=F-------------------------------------------------------------------
plot(x[,1],type='l',col=1,lwd=2,xlab='Index',ylab='Random numbers')
lines(x[,2],col=2,lwd=2)
legend('bottomright',c(expression(X[1]),expression(X[2])),col=1:2,lwd=2)

## ----echo=FALSE---------------------------------------------------------------
Gelman.Rubin <- function(psi) {
    # psi[i,j] is the statistic psi(X[i,1:j])
    # for chain in i-th row of X
    psi <- as.matrix(psi)
    n <- ncol(psi)
    k <- nrow(psi)

    psi.means <- rowMeans(psi)     #row means
    B <- n * var(psi.means)        #between variance est.
    psi.w <- apply(psi, 1, "var")  #within variances
    W <- mean(psi.w)               #within est.
    v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
    r.hat <- v.hat / W             #G-R statistic
    return(r.hat)
    }

## -----------------------------------------------------------------------------

    sigma <- 2    #parameter of proposal distribution
    k <- 4          #number of chains to generate
    n <- 15000      #length of chains
    b <- 1000       #burn-in length

    #choose overdispersed initial values
    x0 <- c(-20, -10, 10, 20)

    #generate the chains
    set.seed(2021)
    X <- matrix(0, nrow=k, ncol=n)
    for (i in 1:k)
        X[i, ] <- rw.Metropolis(sigma, x0[i], n)$x

    #compute diagnostic statistics
    psi <- t(apply(X, 1, cumsum))
    for (i in 1:nrow(psi))
        psi[i,] <- psi[i,] / (1:ncol(psi))

    #plot psi for the four chains
#    par(mfrow=c(2,2))
    for (i in 1:k)
      if(i==1){
        plot((b+1):n,psi[i, (b+1):n], type="l",
            xlab='Index', ylab=bquote(phi))
      }else{
        lines(psi[i, (b+1):n], col=i)
    }
    par(mfrow=c(1,1)) #restore default

## ----echo=FALSE---------------------------------------------------------------
    
    #plot the sequence of R-hat statistics
    rhat <- rep(0, n)
    for (j in (b+1):n)
        rhat[j] <- Gelman.Rubin(psi[,1:j])
    plot(rhat[(b+1):n], type="l" ,xlab="", ylab="R")
    abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
f_k <- function(k,d,a) {
  (-1/2)^k * exp((2*k+2)*log(norm(as.matrix(a),"F"))-log(2*k+1)-log(2*k+2)) * exp(lgamma((d+1)/2) +lgamma(k + 3/2) - lgamma(k)-lgamma(k + d/2 +1)) 
}

k<-100
d <-200
a <- rep(1,d)
f_k(1,d,a)
    

## -----------------------------------------------------------------------------
f_sum <- function(d,a) {
  s=0
  k=1
  while (TRUE) {
  sk<- (-1/2)^k * exp((2*k+2)*log(norm(as.matrix(a),"F"))-log(2*k+1)-log(2*k+2)) * exp(lgamma((d+1)/2) +lgamma(k + 3/2) - lgamma(k)-lgamma(k + d/2 +1))

  if (abs(sk) < .Machine$double.eps) {break}
  s = s+sk
  k=k+1
  }
  return(c(sum = s, k = k))
}
k<-100
d <-200
a <- rep(1,d)
f_sum(d,a)


## -----------------------------------------------------------------------------
d <-2
a <- c(1,2)
f_sum(d,a)


## -----------------------------------------------------------------------------
root=function(k){
s1=function(a){
  1-pt(sqrt((k-1)*a^2/(k-a^2)),k-1)
}
s2=function(a){
  1-pt(sqrt(k*a^2/(k+1-a^2)),k)
}
f<-function(a){
  s1(a)-s2(a)
}
return(uniroot(f,interval = c(1e-6,sqrt(k)-1e-6))$root)
}
r = sapply(c(4:25, 100, 500, 1000), function (k) {
  root(k)
  })
r

## -----------------------------------------------------------------------------
find_a <- function(k){
  
  # S1 <-function(a){
  #   jifen = integrate(function(u) { (1 + u^2/(k-1) )^(-k/2)},lower = 0, upper = sqrt(a^2*(k-1)/(k-a^2)) )$value
  #   2* exp( lgamma(k/2)-lgamma( (k-1)/2) )/ sqrt(  pi*(k-1) ) * jifen
  # }
  # 
  # S2 <-function(a){
  #   jifen = integrate(function(u) { (1+u^2/k)^(-(k+1)/2)},lower = 0, upper = sqrt(a^2*k/(k+1-a^2)) )$value
  #   2* exp( lgamma( (k+1)/2 )-lgamma(k/2) )/ sqrt( pi*k ) * jifen
  # }
  
  S1 <- function(a){
    2*( pt( sqrt(a^2*(k-1)/(k-a^2)),k-1) -1/2)
  }

  S2 <- function(a){
    2*( pt( sqrt(a^2*k/(k+1-a^2)),k) -1/2)
  }
  f <- function(a) S1(a)-S2(a)
  return(uniroot(f,interval = c(1e-6,sqrt(k)-1e-6))$root)
}

r = sapply(c(4:25, 100, 500, 1000), function (k) {
  find_a(k)
  })
r

## -----------------------------------------------------------------------------
# \sum_i X_i+3 = \sum_i Y_i
Y =c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
X = c(0.54, 0.48, 0.33, 0.43, 0.91, 0.21, 0.85)
lambda = lambda0 = 1 #inital est. for lambdas
tol = .Machine$double.eps^0.5

while (TRUE){
  Z = rep((lambda+1)/lambda,3) #E step
  lambda = 10/(sum(X)+sum(Z)) #M step
  if (abs(lambda-lambda0) < tol) break
  lambda0 = lambda
}
c(est = lambda,true =28/27)

## -----------------------------------------------------------------------------
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)
lapply(trims, function(trim) mean(x, trim = trim))
lapply(trims, mean, x = x)


## -----------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared

# Exercises 3 
data(mtcars)

formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)

models <- lapply(formulas, lm, data = mtcars)

r2 <- lapply(models, rsq)
r2

## -----------------------------------------------------------------------------
# Exercises 4

bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

linear_model <- function(data){
  lm(mpg ~ disp, data)
}

models <- lapply(bootstraps,linear_model)

r2 <- lapply(models, rsq)
r2


## -----------------------------------------------------------------------------
# a)
set.seed(123)
numeric.df <- data.frame(replicate(6,sample(c(1:10),10,rep=T)))

vapply(numeric.df, sd, 0)


## -----------------------------------------------------------------------------
# b)
data(CO2)

vapply( CO2[ vapply(CO2, is.numeric, logical(1)) ], sd, 0)


## ----eval=FALSE---------------------------------------------------------------
#  
#  library(parallel)
#  cores <- detectCores()
#  
#  mcsapply <- function(x, f, ..., mc.cores=1L) {
#    # res <- mclapply(x, f, ...,mc.cores = mc.cores) #this can be run in linux
#  
#    cluster <- makeCluster(4)
#    clusterExport(cluster,"pause",envir = environment())
#    res <- parLapply(cluster, x, f, ...)
#    simplify2array(res)
#  }
#  
#  

## ----eval=FALSE---------------------------------------------------------------
#  // [[Rcpp::export]]
#  NumericMatrix gibbsC(double a,double b,double n){
#    int N = 10000;
#    NumericMatrix X(N,2);
#    X(0,0) = 0;
#    X(0,1) = 0.5;
#    double X1, X2;
#    for(int i=1; i<N; i++){
#      X2 = X(i-1,1);
#      X(i,0) = rbinom(1,n,X2)[0];
#      X1 = X(i,0);
#      X(i,1) = rbeta(1,X1+a,n-X1+b)[0];
#    }
#    return X;
#  }

## -----------------------------------------------------------------------------
gibbsR <- function(a,b,n){
  N <- 10000          #样本量
  X <- matrix(0, N, 2)  #样本阵
  X[1,] <- c(0,0.5)
  for(i in 2:N){
    X2 <-  X[i-1, 2]
    X[i,1] <- rbinom(1,25,X2)
    X1 <- X[i,1]
    X[i,2] <- rbeta(1,X1+a,25-X1+b)
  }
  X
}

## ----eval=FALSE---------------------------------------------------------------
#  library(Rcpp)
#  dir_cpp <- './'
#  # Can create source file in Rstudio
#  sourceCpp(paste0(dir_cpp,"gibbsC.cpp"))
#  a=1
#  b=1
#  n=25
#  X.R = gibbsR(a,b,n)
#  X.C = gibbsC(a,b,n)
#  qqplot(X.R[,1],X.C[,1])
#  qqplot(X.R[,2],X.C[,2])

## ----eval=FALSE---------------------------------------------------------------
#  library(microbenchmark)
#  
#  ts <- microbenchmark(gibbsR=gibbsR(a,b,n),gibbsC=gibbsC(a,b,n))
#  summary(ts)[,c(1,3,5,6)]
#  

