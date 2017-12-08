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
setwd("C:/Users/Irina/Google Drive/Harvard classwork/MIT 6.867/Final project/Data")
data = read.csv("1_Imputed/trainset_impute50000.csv",header=T)
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

############################################################
train = read.csv("trainset_impute_onehot.csv",header=T)
############################################################
cont_inter_name=c("ps_calc_05","ps_calc_06","ps_calc_07","ps_calc_08","ps_calc_09","ps_calc_10",
             "ps_calc_11","ps_calc_12","ps_calc_13","ps_calc_14","ps_car_13","ps_reg_01",
             "ps_reg_02","ps_reg_03")
cont_inter_var=c()
for(i in 1:(length(cont_inter_name)-1)){
  for(j in (i+1):length(cont_inter_name)){
    a=train[,cont_inter_name[i]]
    b=train[,cont_inter_name[j]]
    d=c()
    for (k in 1:length(a)){
      d[k]=a[k]*b[k]
    }
    cont_inter_var=cbind(cont_inter_var,d)
    t=ncol(cont_inter_var)
    colnames(cont_inter_var)[t] = paste0(cont_inter_name[i],'*',cont_inter_name[j])
  }
}

std_inter = scale(cont_inter_var)
std_inter = as.data.frame(cont_inter_var)
cont_name = c("ps_ind_01","ps_ind_03","ps_ind_14","ps_ind_15","ps_reg_01","ps_reg_02",
              "ps_reg_03","ps_car_11","ps_car_12","ps_car_13","ps_car_14","ps_car_15",
              "ps_calc_01","ps_calc_02","ps_calc_03","ps_calc_04","ps_calc_05","ps_calc_06",
              "ps_calc_07","ps_calc_08","ps_calc_09","ps_calc_10","ps_calc_11","ps_calc_12",
              "ps_calc_13","ps_calc_14")
for (i in 1:length(cont_name)){
  train[,cont_name[i]]=scale(train[,cont_name[i]])
}
train=cbind(train,std_inter)
write.csv(train,"trainset_1120.csv",row.names = F)
