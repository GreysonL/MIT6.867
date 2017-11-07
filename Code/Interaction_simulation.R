
set.seed((116687))
n <- 100000
err <- rnorm(n,0,2) # error is iid Normal(0,2^2)
expit <- function(x) exp(x)/(1+exp(x))
x1 <- rbinom(n,1,.8) #Bernoulli
x2 <- rnorm(n,2,3) # Normal (2,3^2)
x3 <- rnorm(n,-1,10) # Normal(-1,10^2)
x4m <- t(rmultinom(n,1,c(3,6,8,6,12,4,6,3)/sum(c(3,6,8,6,12,4,6,3)))) # Multinomial 8 categories
x5m <- t(rmultinom(n,1,c(6,12,4,6,3)/sum(c(6,12,4,6,3)))) # Multinomial 5 categories
x6 <- rbinom(n,1,.3) #Bernoulli
x7 <- rbinom(n,1,.55) #Bernoulli
# Model
covariates <- cbind(rep(1,n),x1,x2,x3,x4m,x5m,x6,x7,x1*x3,sin(x4m*x6),x5m*x2,expit(x6^2),exp(x2),x3*x7^3)
betas <- runif(dim(covariates)[2],min=-2,max=2)
# P(Y=1)
Py <- expit(covariates%*%betas+err)

data <- data.frame(cbind(Py,covariates))
write.csv(data,file='~/Documents/GitHub/MIT6.867/Data/simulation.csv')


