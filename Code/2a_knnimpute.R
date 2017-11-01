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
feature_impute=knn.impute(feature,k=5,cat.var=cat_index,to.impute=1:nrow(feature),using=1:nrow(feature))