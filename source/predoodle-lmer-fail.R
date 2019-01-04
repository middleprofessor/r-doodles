set.seed(21)
Height=1:10; Height=Height+runif(10,min=0,max=3) #First height measurement
Weight=1:10; Weight=Weight+runif(10,min=0,max=3) #First weight measurement

Height2=Height+runif(10,min=0,max=1) #second height measurement
Weight2=Weight-runif(10,min=0,max=1) #second weight measurement

Height=c(Height,Height2) #combine height and wight measurements
Weight=c(Weight,Weight2)

DF=data.frame(Height,Weight) #generate data frame
DF$ID=as.factor(rep(1:10,2)) #add subject ID
DF$Number=as.factor(c(rep(1,10),rep(2,10)))

fit.lme=lme(Height~Weight,random=~1|ID,data=DF)
fit.lmer =lmer(Height~Weight+(1|ID),data=DF)
coef(fit.lme)
coef(fit.lmer)
qplot(x=Weight, y=Height, group=ID, data=DF) + geom_line()


set.seed(21)
Height=1:10; Height=Height+rnorm(10, mean=1.5, sd=.75) #First height measurement
Weight=1:10; Weight=Weight+rnorm(10, mean=1.5, sd=.75) #First weight measurement

Height2=Height+rnorm(10, mean=0.5, sd=.25) #second height measurement
Weight2=Weight-rnorm(10, mean=0.5, sd=.25) #second weight measurement

Height=c(Height,Height2) #combine height and wight measurements
Weight=c(Weight,Weight2)

DF=data.frame(Height,Weight) #generate data frame
DF$ID=as.factor(rep(1:10,2)) #add subject ID
DF$Number=as.factor(c(rep(1,10),rep(2,10)))

fit.lme=lme(Height~Weight,random=~1|ID,data=DF)
fit.lmer =lmer(Height~Weight+(1|ID),data=DF)
coef(fit.lme)
coef(fit.lmer)

qplot(x=Weight, y=Height, group=ID, data=DF) + geom_line()

#set.seed(21)
n <- 10^4
x <- runif(n, min=1, max=10)
#x <- 1:10
h1 <- x+runif(n, min=0, max=3)
w1 <- x+runif(n, min=0, max=3)
h2 <- h1+runif(n, min=0, max=1)
w2 <- w1-runif(n, min=0, max=1)
dt <- data.table(id=factor(rep(1:n, 2)),
                 h=c(h1, h2),
                 w=c(w1, w2))
# no pooling
coef(summary(lm(h1~w1)))
coef(summary(lm(h2~w2)))
# complete pooling
coef(summary(lm(h~w, data=dt)))
fit.lmer <- lmer(h~w + (1|id), data=dt)
coef(summary(fit.lmer))
#coef(fit.lmer)
