# finding a GLM where average of predictions on link and response scale are far from each other

library(mvtnorm)
library(ggplot2)
library(data.table)
library(MuMIn)

n <- 20
p <- 5
n_cors <- p*(p-1)/2
min_r <- 0.2
max_r <- 0.95
niter <- 500
res_names <- c('mse.link', 'mse.response', 'mse.diff')
res <- matrix(NA, nrow=niter, ncol=(length(res_names) + 1))
colnames(res) <- c('seed', res_names)
R <- matrix(NA, nrow=p, ncol=p)
for(iter in 1:niter){
  the_seed <- round(runif(1, 1, 10^6),0)
  set.seed(the_seed)
  done <- FALSE
  while(done==FALSE){
    for(i in 1:p){
      for(j in 1:i){
        if(i==j){
          R[i,j] <- 1
        }else{
          r <- runif(n=1, min=min_r, max=max_r)*sample(c(-1,1,1,1), size=1)
          R[i,j] <- r
          R[j,i] <- r
        }
      }
    }
    L <- eigen(R)$values
    if(L[p]>0){done <- TRUE}
  }
  exp_beta_0 <- 5
  X <- rmvnorm(n, mean=rep(0,p), sigma=R)
  colnames(X) <- paste('x', 1:p, sep='')
  logb <- log(runif(p,min=0.9, max=1.1))
  # expected count in link space
  E_log_count <- log(exp_beta_0) + X%*%logb
  E_count <- exp(E_log_count)
  # observed counts
  count <- rpois(n=n, lambda=exp(E_log_count))
  # min(count)
  # max(count)
  
  # create data.table and fit 
  dt <- data.table(count=count, X)
  fit <- glm(count ~ x1 + x2 + x3 + x4 + x5, family=poisson(link = "log"), data=dt,na.action=na.fail )
  X <- model.matrix(fit)
  
  # all model regression using MuMIn
  fit.mm <- dredge(fit)
  model_set <- get.models(fit.mm, subset=TRUE) # all models
  fit.avg <- model.avg(model_set) # coeffcients are on link scale

  # (0) MuMIn predict
  link <- predict(fit.avg, backtransform=TRUE)
  
  # (3) compute predictions for each model on link scale. Backtransform to response scale. Average on response scale. This is method of Dormann et al.
  w <- fit.mm$weight
  yhat2b.each_model.link_scale <- sapply(model_set, predict)
  yhat2b.link_scale <- (yhat2b.each_model.link_scale%*%w)[,1]
  yhat3.each_model.response_scale <- exp(yhat2b.each_model.link_scale)
  response <- (yhat3.each_model.response_scale%*%w)[,1]
  
  mse.link <- mean((link-count)^2)
  mse.response <- mean((response-count)^2)
  mse.diff <- mean((response-link)^2)
  res[iter, 'seed'] <- the_seed
  res[iter, res_names] <- c(mse.link, mse.response, mse.diff)
}
res <- data.table(res)
res[, diff.mse:=mse.link-mse.response]
qplot(mse.diff, diff.mse, data=res) + xlab("MSE(Link vs. Response)") + ylab("MSE.link - MSE.response")
mean(res$diff.mse) # positive indicates link scale is worse
quantile(res$diff.mse, seq(.1, .9, .1))