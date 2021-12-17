#' @importFrom stats rbinom rnorm
gendata = function(n, p, K, sigma = 0.01, seed = 1234){
  set.seed(seed)
  sigma=0.01
  one=rep(1,n)
  Xp1 = matrix(rnorm(n/2*K,mean = 1.5, sd=0.75),nrow = n/2,ncol = K)
  Xf1 = matrix(rnorm(n/2*K,mean = -1.5, sd=0.75),nrow = n/2,ncol = K)
  X2 = matrix(rbinom(n*(p-K),1,0.02),nrow = n,ncol=p-K)*matrix(rnorm(n*(p-K),0,sigma),nrow = n,ncol = p-K)
  X1 = rbind(Xp1,Xf1)
  X = cbind(X1,X2)
  colnames(X)=paste0('X',1:ncol(X))
  
  y = c(rep(1,n/2),rep(-1,n/2))
  
  return(list(x=X,y=y))
}
