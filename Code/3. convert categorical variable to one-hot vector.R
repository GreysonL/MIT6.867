## toy example 
a=c(1,2,3,1,2,3,4,1,2,3)
b=c(1,2,3,4,1,2,3,4,1,2)
d=c(1,2,2,1,2,1,1,1,2,1)
id=c(1,2,3,4,5,6,7,8,9,10)
f=cbind(id,a,b,d)
cat_varname=c('a','b','d')
colnames(f)=c("id","a","b","d")
f=data.frame(f)
for (i in 1:length(cat_varname)){
  one_hot=c()
  drop = cat_varname[i]
  cat_var=f[,drop]
  cat_range=unique(cat_var)
  if (length(cat_range)==2){
    f[cat_var==cat_range[1],drop]=1
    f[cat_var==cat_range[2],drop]=-1
  }
  else{
    f=f[,!(names(f) %in% drop)]
    for (j in 1:length(cat_range)){
      one_hot=cbind(one_hot,as.numeric(cat_var==cat_range[j]))
    }
    one_hot=data.frame(one_hot)
    for (j in 1:length(cat_range)){
      colnames(one_hot)[j]=paste0(drop,"_",cat_range[j])
    }
    f=cbind(f,one_hot)
  }
}

## real data - trainset 
setwd("~/GitHub/MIT6.867/Data")
data = read.csv("trainset_impute50000.csv",header=T)
cat_varname=c('ps_ind_02_cat','ps_ind_04_cat','ps_ind_05_cat','ps_ind_06_bin','ps_ind_07_bin',
              'ps_ind_08_bin','ps_ind_09_bin','ps_ind_10_bin','ps_ind_11_bin','ps_ind_12_bin',
              'ps_ind_13_bin','ps_ind_16_bin','ps_ind_17_bin','ps_ind_18_bin','ps_car_01_cat',
              'ps_car_02_cat','ps_car_03_cat','ps_car_04_cat','ps_car_05_cat','ps_car_06_cat',
              'ps_car_07_cat','ps_car_08_cat','ps_car_09_cat','ps_car_10_cat','ps_car_11_cat',
              'ps_calc_15_bin','ps_calc_16_bin','ps_calc_17_bin','ps_calc_18_bin','ps_calc_19_bin',
              'ps_calc_20_bin')
for (i in 1:length(cat_varname)){
  one_hot=c()
  drop = cat_varname[i]
  cat_var=data[,drop]
  cat_range=unique(cat_var)
  if (length(cat_range)==2){
    data[cat_var==cat_range[1],drop]=1
    data[cat_var==cat_range[2],drop]=-1
  }
  else{
    data=data[,!(names(data) %in% drop)]
    for (j in 1:length(cat_range)){
      one_hot=cbind(one_hot,as.numeric(cat_var==cat_range[j]))
    }
    one_hot=data.frame(one_hot)
    for (j in 1:length(cat_range)){
      colnames(one_hot)[j]=paste0(drop,"_",cat_range[j])
    }
    data=cbind(data,one_hot)
  }
}
write.csv(data,"trainset_impute_onehot.csv",row.names = F)

## real data - testset 
data = read.csv("testset_impute50000.csv",header=T)
cat_varname=c('ps_ind_02_cat','ps_ind_04_cat','ps_ind_05_cat','ps_ind_06_bin','ps_ind_07_bin',
              'ps_ind_08_bin','ps_ind_09_bin','ps_ind_10_bin','ps_ind_11_bin','ps_ind_12_bin',
              'ps_ind_13_bin','ps_ind_16_bin','ps_ind_17_bin','ps_ind_18_bin','ps_car_01_cat',
              'ps_car_02_cat','ps_car_03_cat','ps_car_04_cat','ps_car_05_cat','ps_car_06_cat',
              'ps_car_07_cat','ps_car_08_cat','ps_car_09_cat','ps_car_10_cat','ps_car_11_cat',
              'ps_calc_15_bin','ps_calc_16_bin','ps_calc_17_bin','ps_calc_18_bin','ps_calc_19_bin',
              'ps_calc_20_bin')
for (i in 1:length(cat_varname)){
  one_hot=c()
  drop = cat_varname[i]
  cat_var=data[,drop]
  cat_range=unique(cat_var)
  if (length(cat_range)==2){
    data[cat_var==cat_range[1],drop]=1
    data[cat_var==cat_range[2],drop]=-1
  }
  else{
    data=data[,!(names(data) %in% drop)]
    for (j in 1:length(cat_range)){
      one_hot=cbind(one_hot,as.numeric(cat_var==cat_range[j]))
    }
    one_hot=data.frame(one_hot)
    for (j in 1:length(cat_range)){
      colnames(one_hot)[j]=paste0(drop,"_",cat_range[j])
    }
    data=cbind(data,one_hot)
  }
}
colnames(data)[1]='id'
write.csv(data,"testset_impute_onehot.csv",row.names = F)
