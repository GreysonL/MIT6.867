
set.seed((116687))
n <- 100000
err <- rnorm(n,0,4) # error is iid Normal(0,2^2)
expit <- function(x){
  r <- exp(x)/(1+exp(x))
  r[abs(x)>99] <- 1
  return(r)
  }
x1 <- rbinom(n,1,.8) # Bernoulli
x2 <- rnorm(n,2,3) # Normal (2,3^2)
x3 <- rnorm(n,-1,2) # Normal(-1,10^2)
x4m <- t(rmultinom(n,1,c(3,6,8,6,12,4,6,3)/sum(c(3,6,8,6,12,4,6,3)))) # Multinomial 8 categories
x5m <- t(rmultinom(n,1,c(6,12,4,6,3)/sum(c(6,12,4,6,3)))) # Multinomial 5 categories
x6 <- rbinom(n,1,.3) # Bernoulli
x7 <- rbinom(n,1,.55) # Bernoulli
# Model
#raw_cont_cov <- cbind(x1,x2,x3)
covariates <- cbind(rep(1,n),x1,x2,x3,x4m,x5m,expit(x6),x7,x2*x3,x1*x3,sin(x4m*x6),x5m*x2,x2^3)

## Interactions:
# x1*x3
# x4m*x6
# x5m*x2
# x3*x7

betas <- runif(dim(covariates)[2],min=-2,max=2)
# P(Y=1)
#Py <- expit(c(covariates%*%betas+err))
Py <- expit(c((covariates%*%betas+err)*.025))
plot(density(Py))
mean(Py)
# Binary data
x1[x1==0] <- -1
x6[x6==0] <- -1
x7[x7==0] <- -1

# Continoues data
x2 <- (x2-mean(x2))/sd(x2)
x3 <- (x3-mean(x3))/sd(x3)


Raw_data <- data.frame(cbind(Py,x1,x2,x3,x4m,x5m,x6,x7))
write.csv(Raw_data,file='~/Documents/GitHub/MIT6.867/Data/Raw_data_simulation.csv',row.names = F)

Interaction_data <- data.frame(cbind(Py,x1,x2,x3,x4m,x5m,x6,x7,x2*x3,x1*x3,x4m*x6,x5m*x2))
write.csv(Interaction_data,file='~/Documents/GitHub/MIT6.867/Data/Interaction_data_simulation.csv',row.names = F)

all_data <- data.frame(cbind(Py,x1,x2,x3,x4m,x5m,x6,x7,10*x2*x3,x1*x3,sin(x4m*x6),x5m*x2,expit(x6^2),exp(x2),x3*x7^3))
write.csv(all_data,file='~/Documents/GitHub/MIT6.867/Data/all_data_simulation.csv',row.names = F)




