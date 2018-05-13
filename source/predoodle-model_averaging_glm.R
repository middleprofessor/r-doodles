# pre-doodle script
# exploring glm averaging

simulate_poisson <- function(){
  n <- 100
  exp_beta_0 <- 175 # mean Y on response scale
  exp_beta_1 <- 0.99 # effect on response scale
  exp_beta_2 <- 0.99 # effect on response sacle
  
  # create correlated x1 and x2 due to common factor z
  z <- rnorm(n) # common factor to correlate X1 and X2
  r <- 0.6 # correlation between x1 and x2
  alpha <- sqrt(r) # "effect" of Z on X1 and X2
  x1 <- alpha*z + sqrt(1-r)*rnorm(n)
  x2 <- alpha*z + sqrt(1-r)*rnorm(n)

  # expected count in link space
  E_log_count <- log(exp_beta_0) + log(exp_beta_1)*x1 + log(exp_beta_2)*x2 # expected log count
  # observed counts
  count <- rpois(n=n, lambda=exp(E_log_count))

  # create data.table and fit 
  dt <- data.table(count=count, x1=x1, x2=x2)
  fit <- glm(count ~ x1 + x2, family=poisson(link = "log"), data=dt,na.action=na.fail )
  X <- model.matrix(fit)
  
  # predict for this model
  yhat <- predict(fit, type='response')
  yhat.check <- (exp(X%*%coef(fit)))[,1] # should be same
  yhat.check2 <- (X%*%exp(coef(fit)))[,1] # should differ
  head(data.table(yhat=yhat, yhat.check=yhat.check, yhat.check2=yhat.check2))
  
  # all model regression using MuMIn
  fit.mm <- dredge(fit)
  model_set <- get.models(fit.mm, subset=TRUE) # all models
  fit.avg <- model.avg(model_set) # coeffcients are on link scale
  fit.avg
  
  # (0) MuMIn predict
  yhat0.MuMIn <- predict(fit.avg, backtransform=TRUE)
  
  #is this averaged on link or response scale? And is it the coefficients or the prediction that is averaged?
  
  # (1) average coefficients on link scale. compute prediction on link scale. transform predictions to response scale
  b <- fit.avg$coefficients['full',][colnames(X)]
  yhat1 <- exp((X%*%b)[,1]) #

  # (2) compute predictions for each model on link scale. Average on link scale. Backtransform to response scale
  yhat2a <- exp(predict(fit.avg, backtransform=FALSE))
  w <- fit.mm$weight
  yhat2b.each_model.link_scale <- sapply(model_set, predict)
  yhat2b.link_scale <- (yhat2b.each_model.link_scale%*%w)[,1]
  yhat2b <- exp(yhat2b.link_scale)
  
  # (3) compute predictions for each model on link scale. Backtransform to response scale. Average on response scale.
  yhat3.each_model.response_scale <- exp(yhat2b.each_model.link_scale)
  yhat3 <- (yhat3.each_model.response_scale%*%w)[,1]
  
  # (4) backtransform coefficients to response scale. Average coefficients on response scale. Compute prediction on response scale.
  B <- exp(fit.mm[,colnames(X)])
  B[is.na(B)] <- 0.0
  b_ma <- t(B)%*%w
  yhat4 <- (X%*%b_ma)[,1] #

  # (5) average coefficients on link scale. backtransform to response scale. compute prediction on response scale
  b <- exp(fit.avg$coefficients['full',][colnames(X)])
  yhat5 <- (X%*%b)[,1] #
  
  
res <- data.table(MuMIn=yhat0.MuMIn,
                  yhat1=yhat1, 
                  yhat2=yhat2b, 
                  yhat3=yhat3,
                  yhat4=yhat4,
                  yhat5=yhat5)
  
head(res)
}

niter <- 1000
res_cols <- c('mse_link', 'mse_response')
res <- matrix(NA, nrow=niter, ncol=length(res_cols))
colnames(res) <- res_cols
n <- 100
z <- rnorm(n)
# create two correlated variables, x1 and x2, with E[cor] = zeta^2
zeta <- 0.7
sigma <- 3

for(iter in 1:niter){
  x1 <- zeta*z + sqrt(1-zeta^2)*rnorm(n)
  x2 <- zeta*z + sqrt(1-zeta^2)*rnorm(n)
  
  # create a performance measure as function of x1 and x2
  perf <- beta_1*x1 + beta_2*x2 + rnorm(n)*sigma # coefficients both = 1
  #summary(lm(perf ~ x1 + x2))
  
  # transform performance to probability of event
  
  # create fake data
  prob <- expit(perf)
  y <- rbinom(n, 1, prob)
  # y2 <- as.numeric(perf>0)
  # ytab <- data.table(y=y,y2=y2)
  #y <- as.numeric(perf>0)
  dt <- data.table(y=y,x1=x1,x2=x2)
  
  # fit
  fit <- glm(y ~ x1 + x2, data=dt, family=binomial(link='logit'), na.action=na.fail)
  
  # all subsets regression and model average using MuMIn
  fit.all <- dredge(fit)
  fit.avg <- model.avg(fit.all, fit=TRUE) # coeffcients are on link scale
  
  model_set <- get.models(fit.all, subset=TRUE) # all models
  X <- model.matrix(fit)
  
  # (0) MuMIn predict
  yhat.link_scale_bar <- predict(fit.avg, backtransform=TRUE)
  MSE1 <- sqrt(mean((yhat.link_scale_bar-prob)^2))
  
  w <- fit.all$weight
  yhat2.each_model.link_scale <- sapply(model_set, predict)
  yhat3.each_model.response_scale <- expit(yhat2.each_model.link_scale)
  yhat.response_scale_bar <- (yhat3.each_model.response_scale%*%w)[,1]
  MSE2 <- sqrt(mean((yhat.response_scale_bar-prob)^2))
  
  res_i <- data.table(y=y,pred1=yhat.link_scale_bar, pred2=yhat.response_scale_bar)
  res[iter,] <- c(MSE1, MSE2)
}

apply(res,2,mean)

