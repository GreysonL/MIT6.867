### Simulation where features that matter are X's and interactions
set.seed(867)
N=3000
x1=rnorm(N,mean=0,sd=1)
x2=rnorm(N,mean=0,sd=1)
x3=rnorm(N,mean=0,sd=1)
x4_raw=sample(c(1,2,3,4),N,replace=T)
x4_hot=c()
x5=rnorm(N,mean=0,sd=1)
x6_hot=c()
x6_raw=sample(c(1,2,3,4),N,replace=T)
cat_range=unique(x4_raw)
for (j in 1:length(cat_range)){
  x4_hot=cbind(x4_hot,as.numeric(x4_raw==cat_range[j]))
}
cat_range=unique(x6_raw)
for (j in 1:length(cat_range)){
  x6_hot=cbind(x6_hot,as.numeric(x6_raw==cat_range[j]))
}

## assume there is an interaction between x1 and x2, x3 and x4, while the main effects of x1,x2,x3,x4 
## are weak

inter1=x1*x2
inter2=c()
for (i in 1:N){
  inter2=rbind(inter2,x3[i]*x4_hot[i,])
}

X=cbind(x1,x2,x3,x4_hot,inter1,inter2,x5,x6_hot)
beta=c(0.1,-0.1,0.1,-0.1,0.1,-0.1,0.1,2,0.5,1,1.5,2)
eta=-0.5+X[,1:12]%*%beta+rnorm(N,mean=0,sd=1)
p=exp(eta)/(1+exp(eta))
mean(p)
Y=as.numeric(p>0.5)

## Create additional covariates
### Add powers and logarithms
X_withextra = as.data.frame(X)
cont_vars = c("x1","x2","x3","x5")
for(i in cont_vars){
  X_withextra[,paste0(i,"_sq")] = X_withextra[,i]^2
}
for(i in cont_vars){
  X_withextra[,paste0(i,"_cube")] = X_withextra[,i]^3
}
for(i in cont_vars){
  X_withextra[,paste0(i,"_exp")] = exp(X_withextra[,i])
}

### Add interactions, ratios, and differences
for(i in 1:(length(cont_vars)-1)){
  for(j in (i+1):length(cont_vars)){
    a=X_withextra[,cont_vars[i]]
    b=X_withextra[,cont_vars[j]]
    
    X_withextra[,paste0(cont_vars[i],'*',cont_vars[j])] = a*b
    X_withextra[,paste0(cont_vars[i],'-',cont_vars[j])] = a-b
    X_withextra[,paste0(cont_vars[i],'/',cont_vars[j])] = a/b
    
  }
}



data=cbind(Y,X_withextra) #1 Y, 2:18 X's, 19:48 extra covars
write.csv(data,"C:/Users/Irina/Google Drive/Harvard classwork/MIT 6.867/Final project/Data/2_Cleaned/sim_int_features.csv",row.names = F)


######################################################################################################
## Simulated data 2
######################################################################################################
## assume a non-linear combination of x1,x2,x3,x4, which indcludes interactions, powers, and ratios
X2=cbind(x1,x2,x3,x4_hot,inter1,inter2,x5,x6_hot)
beta2=c(0.1,-0.1,0.1,-0.1,0.1,-0.1,0.1,2,0.5,1,1.5,2)
eta2=-0.5+X[,1:12]%*%beta+rnorm(N,mean=0,sd=1)
p2=exp(eta)/(1+exp(eta))
mean(p2)
Y2=as.numeric(p>0.5)
