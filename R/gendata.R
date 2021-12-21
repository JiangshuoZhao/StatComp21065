#' @title Generate a matrix with n*p have two class.
#' @description This function is to generate a matrix with n*p have two class for classification.
#' @param n The number of sample.
#' @param p The number of feature dimension.
#' @param K The true support set size.
#' @param Mean The  mean of positive sample, '-Mean' is the mean of negative sample.
#' @param Sd The standard deviation of sample.
#' @param sigma standard deviation of noise.
#' @param seed Random seed.
#' @importFrom stats rbinom rnorm
#' @return A list consist of x and y. x is the data matrix, and y is is the corresponding category.
#' @examples 
#' \dontrun{
#' data <- gendata(1000,100,5)
#' }
#' @export
gendata = function(n, p, K,Mean = 1.5,Sd=0.75, sigma = 0.01, seed = 1234){
  set.seed(seed)
  sigma=0.01
  one=rep(1,n)
  Xp1 = matrix(rnorm(n/2*K,mean = Mean, sd = Sd),nrow = n/2,ncol = K)
  Xf1 = matrix(rnorm(n/2*K,mean = -Mean, sd = Sd),nrow = n/2,ncol = K)
  X2 = matrix(rbinom(n*(p-K),1,0.02),nrow = n,ncol=p-K)*matrix(rnorm(n*(p-K),0,sigma),nrow = n,ncol = p-K)
  X1 = rbind(Xp1,Xf1)
  X = cbind(X1,X2)
  colnames(X)=paste0('X',1:ncol(X))
  
  y = c(rep(1,n/2),rep(-1,n/2))
  
  return(list(x=X,y=y))
}
