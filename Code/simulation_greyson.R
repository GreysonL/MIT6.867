N=3000
x1=rnorm(N,mean=0,sd=1)
x2=rnorm(N,mean=0,sd=1)
x3=rnorm(N,mean=0,sd=1)
x4_raw=sample(c(1,2,3,4),N,replace=T)
x4_hot=c()
cat_range=unique(x4_raw)
for (j in 1:length(cat_range)){
  x4_hot=cbind(x4_hot,as.numeric(x4_raw==cat_range[j]))
}

## assume there is an interaction between x1 and x2, x3 and x4, while the main effects of x1,x2,x3,x4 
## are weak

inter1=x1*x2
inter2=c()
for (i in 1:N){
  inter2=rbind(inter2,x3[i]*x4_hot[i,])
}

X=cbind(x1,x2,x3,x4_hot,inter1,inter2)
beta=c(0.1,-0.1,0.1,-0.1,0.1,-0.1,0.1,2,0.5,1,1.5,2)
eta=-0.5+X%*%beta+rnorm(N,mean=0,sd=1)
p=exp(eta)/(1+exp(eta))
mean(p)
Y=as.numeric(p>0.5)
data=cbind(Y,X)
write.csv(data,"~/GitHub/MIT6.867/Data/sim_g.csv",row.names = F)
