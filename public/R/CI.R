# the 95% CI is not the probability of...

library(data.table)

niter <- 1000
n_intervals <- 100

beta_0 <- 10
beta_1 <- 5
beta <- c(beta_0, beta_1)
sigma <- 2.5
n <- 10
x <- factor(rep(c("control", "treatment"), each=n))
fd <- data.table(Treatment=x)
X <- model.matrix(~Treatment, data=fd)
res <- matrix(NA, nrow=niter, ncol=n_intervals)
ci <- matrix(NA, nrow=2, ncol=n_intervals)
for(i in 1:n_intervals){
  # here is our "one" sample
  fd[, y:=X%*%beta + rnorm(n*2, sd=sigma)]
  ci[,i] <- confint(lm(y ~ Treatment, data=fd))[2,]
  # how many resamples are within this inteval?
  for(iter in 1:niter){
    fd[, y:=X%*%beta + rnorm(n*2, sd=sigma)]
    # add b1_hat to the result matrix
    res[iter, i] <- coef(lm(y ~ Treatment, data=fd))[2]
  }
}



