library(bnstruct)
setwd("~/GitHub/MIT6.867/Data")
trainset = read.csv("train.csv",header=T)
trainset[trainset==-1]=NA
feature=trainset[,-c(1,2)]
id_target=trainset[,c(1,2)]
remove(trainset)
summary(feature)
cat_varname=c('ps_ind_02_cat','ps_ind_04_cat','ps_ind_05_cat','ps_ind_06_bin','ps_ind_07_bin',
              'ps_ind_08_bin','ps_ind_09_bin','ps_ind_10_bin','ps_ind_11_bin','ps_ind_12_bin',
              'ps_ind_13_bin','ps_ind_16_bin','ps_ind_17_bin','ps_ind_18_bin','ps_car_01_cat',
              'ps_car_02_cat','ps_car_03_cat','ps_car_04_cat','ps_car_05_cat','ps_car_06_cat',
              'ps_car_07_cat','ps_car_08_cat','ps_car_09_cat','ps_car_10_cat','ps_car_11_cat',
              'ps_calc_15_bin','ps_calc_16_bin','ps_calc_17_bin','ps_calc_18_bin','ps_calc_19_bin',
              'ps_calc_20_bin')
cat_index=match(cat_varname,colnames(feature))
feature=as.matrix(feature)
set.seed(867)
new_index=sample(1:nrow(feature),nrow(feature),replace=F)
feature_shuffle=feature[new_index,]
id_target_shuffle=id_target[new_index,]
feature_impute=c()
N=50000
m=0
while (m<nrow(feature)){
  data=feature_shuffle[(m+1):min(m+N,nrow(feature)),]
  m=m+N
  temp=knn.impute(data,k=5,cat.var=cat_index,to.impute=1:nrow(data),using=1:nrow(data))
  feature_impute=rbind(feature_impute,temp)
}
trainset_impute=cbind(id_target_shuffle,feature_impute)
write.csv(trainset_impute,'trainset_impute.csv',row.names = F)

## testset
testset = read.csv("test.csv",header=T)
testset[testset==-1]=NA
feature=testset[,-1]
id=testset[,1]
remove(testset)
feature=as.matrix(feature)
set.seed(867)
new_index=sample(1:nrow(feature),nrow(feature),replace=F)
test_feature_shuffle=feature[new_index,]
test_id_shuffle=id[new_index]

feature_impute=c()
N=50000
M=floor(892816/12)+1
n=0;m=0
while (n<nrow(feature_shuffle)){
  data_train=feature_shuffle[(n+1):min(n+N,nrow(feature_shuffle)),]
  data_test=test_feature_shuffle[(m+1):min(m+M,nrow(test_feature_shuffle)),]
  data=rbind(data_test,data_train)
  n=n+N
  m=m+M
  temp=knn.impute(data,k=5,cat.var=cat_index,to.impute=1:nrow(data_test),using=(nrow(data_test)+1):nrow(data))
  feature_impute=rbind(feature_impute,temp)
}
testset_impute=cbind(test_id_shuffle,feature_impute)
write.csv(testset_impute,'testset_impute.csv',row.names = F)
