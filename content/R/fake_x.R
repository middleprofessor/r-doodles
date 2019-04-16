# fake data with correlation

library(data.table)

#usage
# n <- 10^4 # number of cases (rows of the data)
# p <- 5 # number of variables (columns of the data)
# 
# # start with a matrix. In real life this would be our data
# X <- fake.X(n, p, fake.eigenvectors(p), fake.eigenvalues(p))

random.sign <- function(u){
  # this is fastest of three
  out <- sign(runif(u)-0.5)
  #randomly draws from {-1,1} with probability of each = 0.5
  return(out)
}

fake.eigenvectors <- function(p){
  a <- matrix(rnorm(p*p), p, p) # only orthogonal if p is infinity so need to orthogonalize it
  a <- t(a)%*%a # this is a pseudo-covariance matrix
  E <- eigen(a)$vectors # decompose to truly orthogonal columns
  return(E)
}

fake.eigenvalues <- function(p, m=p, start=2/3, rate=1.5){
  # m is the number of positive eigenvalues
  # start and rate control the decline in the eigenvalue
  
  s <- start/seq(1:m)^rate
  s <- c(s, rep(0, p-m)) # add zero eigenvalues
  L <- diag(s/sum(s)*m) # rescale so that sum(s)=m and put into matrix,
  # which would occur if all the traits are variance standardized
  return(L)
}

fake.cov.matrix <- function(p){
  # p is the size of the matrix (number of cols and rows)
  E <- fake.eigenvectors(p)
  L <- diag(fake.eigenvalues(p))
  S <- E%*%L%*%t(E)
  return(S)
}

# two functions to compute the random data
fake.X <- function(n,p,E,L){
  # n is number of observations
  # p is number of variables
  X <- matrix(rnorm(n*p),nrow=n,ncol=p) %*% t(E%*%sqrt(L))
  return(X)
}

fake.alpha <- function(p, S, R2=0.9999, standardize=TRUE, omniscient=FALSE){
  # generates standardized effect coefficients
  # S is the TRUE covariance matrix of the explanatory variables
  # R2 is the fraction of SStotal explained by the X
  alpha.raw <- rexp(p,1)
  if(omniscient==TRUE){alpha.raw <- sort(alpha.raw,decreasing=TRUE)}
  alpha <- alpha.raw*random.sign(p)
  if(standardize==TRUE){
    alpha <- sqrt(R2)*alpha/sqrt(abs(sum(diag(alpha)%*%S%*%t(diag(alpha)))))
  }
  return(alpha)
}

