set.seed(298471979)
N <- 10^5

# Random variance not explained by any of the factors
v.error <- 0.01
# Remaining variance
v.g <- (1-v.error) / 2

# LATENT variables that should be recovered statistically
g1 <- rnorm(N,0,sqrt(v.g))
g2 <- rnorm(N,0,sqrt(v.g))
g3 <- rnorm(N,0,sqrt(v.g))

# Observed data caused by latent variables
y1 <- g1 + g2 + rnorm(N,0,sqrt(v.error))
y2 <- g2 + g3 + rnorm(N,0,sqrt(v.error))
y3 <- g1 + g3 + rnorm(N,0,sqrt(v.error))

Y <- cbind(y1, y2, y3)
cov(Y)

y1 <- 0.577*g1 + 0.577*g2 + 0.577*g3 + rnorm(N,0,sqrt(v.error))
y2 <- 0.509*g1 - 0.807*g2 + 0.296*g3 + rnorm(N,0,sqrt(v.error))
y3 <- -0.636*g1 - .122*g2 + 0.761*g3 + rnorm(N,0,sqrt(v.error))
Y2 <- cbind(y1, y2, y3)
cov(Y2)



n <- 10^5
z <- rnorm(n)
r <- 0.5
b1 <- sqrt(r)
a <- 0.5
b <- 0.5
c <- 0.5
c <- 0.5
g1 <- b1*z + sqrt(1-r)*rnorm(n)
g2 <- b1*z + sqrt(1-r)*rnorm(n)
y1 <- a*g1 + c*g2 + sqrt(1 - (a^2 + c^2 + 2*a*c*r))*rnorm(n)
y2 <- a*g1 + c*g2 + sqrt(1 - (a^2 + c^2 + 2*a*c*r))*rnorm(n)
var(cbind(y1, y2))

a <- 1
r <- 0.25
b1 <- sqrt(r)
g1 <- b1*z + sqrt(1-r)*rnorm(n)
g2 <- b1*z + sqrt(1-r)*rnorm(n)
y1 <- a*g1 + c*g2 + sqrt(1 - (a^2 + c^2 + 2*a*c*r))*rnorm(n)
y2 <- a*g1 + c*g2 + sqrt(1 - (a^2 + c^2 + 2*a*c*r))*rnorm(n)
var(cbind(y1, y2))

e1 <- rnorm(n)
e2 <- rnorm(n)
e3 <- rnorm(n)
e4 <- rnorm(n)
n <- 10^5
z <- rnorm(n)
s12 <- 0.5 
b1 <- sqrt(s12)
a <- 0.5
b <- 0.5
c <- 0.5
d <- 0.5
g1 <- b1*z + e1 # var = b1^2 + 1
g2 <- b1*z + e2 # var = b1^2 + 1
sigma1 <- sqrt(b1^2 + 1)
sigma2 <- sqrt(b1^2 + 1)
# cov(g1, g2) = s12
y1 <- a*g1 + c*g2 + e3 # var (a*sigma1)^2 + (c*sigma2)^2 + 2*a*c*s12 + 1
y2 <- b*g1 + d*g2 + e4 # var (b*sigma1)^2 + (d*sigma2)^2 + 2*b*d*s12 + 1
sigma3 <- sqrt((a*sigma1)^2 + (c*sigma2)^2 + 2*a*c*s12 + 1)
sigma4 <- sqrt((b*sigma1)^2 + (d*sigma2)^2 + 2*b*d*s12 + 1)
a.s <- a*sigma1/sigma3
b.s <- b*sigma1/sigma4
c.s <- c*sigma2/sigma3
d.s <- d*sigma2/sigma4
r12 <- s12/sigma1/sigma2
var(cbind(y1, y2)) #
(a.s*b.s + c.s*d.s + a.s*r12*d.s + c.s*r12*b.s)*sigma3*sigma4 # expected cov(y1, y2)
a*b*sigma1^2 + c*d*sigma2^2 + a*s12*d+ c*s12*b
cor(cbind(y1, y2))
coef(summary(lm(y1 ~g1 + g2)))

a <- 1
r <- 0.25
b1 <- sqrt(r)
g1 <- b1*z + sqrt(1-r)*e1 
g2 <- b1*z + sqrt(1-r)*e2
y1 <- a*g1 + c*g2 + e3
y2 <- b*g1 + d*g2 + e4
var(cbind(y1, y2))
cor(cbind(y1, y2))


