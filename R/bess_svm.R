l1 = function(x,w,tau=0.05){
  n = dim(x)[1]
  p = dim(x)[2]
  l = matrix(0,n,p)

  for (i in 1:n) {
    z = 1 - x[i,] %*% w
    if (z < 0) l[i,]=0
    else {if (z < tau) l[i,] = c(-z/tau) * x[i,]
      else l[i,] = -x[i,] }
  }
  return(l)
} # l (n*p)

l2 = function(x,w,tau=.05){
  n = dim(x)[1]
  p = dim(x)[2]
  l = matrix(0,n,p)
  for (i in 1:n) {
    z = 1 - x[i,] %*% w
    if (z > 0 & z < tau) l[i,] = x[i,]^2/tau
  }
  return(l)
} # l' (n*p)

grad = function(x,w,tau,alpha){
  n=dim(x)[1]
  l = l1(x,w,tau)
  g = apply(l,2,sum)/n + alpha * w
  return(g)
} # g (p,)

hess = function(x,w,tau,alpha){
  n=dim(x)[1]
  l = l2(x,w,tau)
  h = apply(l,2,sum)/n + alpha
  return(h)
} # h (p,)

get_d = function(x,w,tau,alpha){
  g = grad(x,w,tau,alpha)
  h = hess(x,w,tau,alpha)
  return(-g/h)
} # dual variable

#' @importFrom LiblineaR LiblineaR
svm_w =function(x,y,n,alpha){
  model = LiblineaR(x, y,type=3, kernel = "linear",bias = 0, cost = 1/(n*alpha),verbose=FALSE)
  w = array(model$W)
  return(w)
} # svm solver

bess_svm=function(X,y,T0, alpha=.01, tau=0.05, max.steps=100){
  X = as.matrix(X)
  n = dim(X)[1]
  p = dim(X)[2]
  X_ =  y*X
  E = c(1:p)
  w_intial =rep(0,p)
  d = get_d(X_,w_intial,tau,alpha)
  h = hess(X_,w_intial,tau,alpha) # hess function
  
  delta = h*(w_intial+ d)^2 #bd sacrifice
  sort_ = sort(delta,decreasing = TRUE,method="quick",index.return=TRUE)
  b = sort_$ix  
  A = b[1:T0] # Active set
  I = setdiff(E,A)
  
  iter = 0
  for (iter in (1:max.steps)){
    iter = iter+1
    w = d = rep(0,p)
    X_A = X[,A]
    X_A = as.matrix(X_A, nrow=n)
    w_A = svm_w(X_A,y,n,alpha) # svm subproblem
    # model = LiblineaR(X_A, y,type=3, kernel = "linear",bias = 0, cost = 1/(n*alpha),verbose=FALSE)
    # w_A = array(model$W)
    w[A] = w_A
    
    d = get_d(X_,w,tau,alpha) 
    
    h = hess(X_,w,tau,alpha)
    delta = h*(w + d)^2 #bd sacrifice

    sort_ = sort(delta,decreasing = TRUE,method="quick",index.return=TRUE)
    b = sort_$ix  
    B = b[1:T0] # Active set
    J = setdiff(E,A)
   
    if(all(A,B)) break 
    else {
      A=B
      I=J
    }
    
  }
  #loss
  obj = list(w,iter)
  names(obj) = c("w", "iter")
  return(obj)
}



