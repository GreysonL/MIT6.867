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
setwd("C:/Users/Irina/Google Drive/Harvard classwork/MIT 6.867/Final project/Data/1_Imputed")
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

### Add count of missing values
raw_data = read.csv("train.csv",header=T)
train = train[order(train$id),]
raw_data = raw_data[order(raw_data$id),]
train$na_count <- apply(raw_data, 1, function(x) sum(is.na(x)))

### Add powers and logarithms
train2 = train
for(i in cont_inter_name){
  train2[,paste0(i,"_sq")] = train2[,i]^2
}
for(i in cont_inter_name){
  train2[,paste0(i,"_cube")] = train2[,i]^3
}
for(i in cont_inter_name){
  train2[,paste0(i,"_log")] = log(train2[,i]+0.01)
}

### Add interactions, ratios, and differences
for(i in 1:(length(cont_inter_name)-1)){
  for(j in (i+1):length(cont_inter_name)){
    a=train[,cont_inter_name[i]]
    b=train[,cont_inter_name[j]]
    train2[,paste0(cont_inter_name[i],'*',cont_inter_name[j])] = a*b
    train2[,paste0(cont_inter_name[i],'-',cont_inter_name[j])] = a-b
    train2[,paste0(cont_inter_name[i],'/',cont_inter_name[j])] = a/(b+0.5) #avoid dividing by zero
  }
}

### Scale data
train_scaled = train2
train_scaled[,214:300] = scale(train_scaled[,214:300]) # Split up for RAM limitations
train_scaled[,301:400] = scale(train_scaled[,301:400])
train_scaled[,401:500] = scale(train_scaled[,401:500])
train_scaled[,501:ncol(train_scaled)] = scale(train_scaled[,501:ncol(train_scaled)])

cont_name = c("ps_ind_01","ps_ind_03","ps_ind_14","ps_ind_15","ps_reg_01","ps_reg_02",
              "ps_reg_03","ps_car_11","ps_car_12","ps_car_13","ps_car_14","ps_car_15",
              "ps_calc_01","ps_calc_02","ps_calc_03","ps_calc_04","ps_calc_05","ps_calc_06",
              "ps_calc_07","ps_calc_08","ps_calc_09","ps_calc_10","ps_calc_11","ps_calc_12",
              "ps_calc_13","ps_calc_14")
train_scaled[,cont_name]=scale(train_scaled[,cont_name])


write.csv(train_scaled,"trainset_1208.csv",row.names = F)


############################################################
test = read.csv("testset_impute_onehot.csv",header=T)
############################################################
### Add count of missing values
raw_data_test = read.csv("test.csv",header=T)
test = test[order(test$id),]
raw_data_test = raw_data_test[order(raw_data_test$id),]
test$na_count <- apply(raw_data_test, 1, function(x) sum(is.na(x)))
#raw_data_test$na_count <- apply(raw_data_test, 1, function(x) sum(is.na(x)))
#merge(test,raw_data_test[,c("id", "na_count")], by="id")

### Add powers and logarithms
test2 = test
for(i in cont_inter_name){
  test2[,paste0(i,"_sq")] = test2[,i]^2
}
for(i in cont_inter_name){
  test2[,paste0(i,"_cube")] = test2[,i]^3
}
for(i in cont_inter_name){
  test2[,paste0(i,"_log")] = log(test2[,i]+0.01)
}

### Add interactions, ratios, and differences
for(i in 1:(length(cont_inter_name)-1)){
  for(j in (i+1):length(cont_inter_name)){
    a=test[,cont_inter_name[i]]
    b=test[,cont_inter_name[j]]
    test2[,paste0(cont_inter_name[i],'*',cont_inter_name[j])] = a*b
    test2[,paste0(cont_inter_name[i],'-',cont_inter_name[j])] = a-b
    test2[,paste0(cont_inter_name[i],'/',cont_inter_name[j])] = a/(b+0.5) #avoid dividing by zero
  }
}

### Scale data
test_scaled = test2
for (i in 214:ncol(test_scaled)){ # Split up for RAM limitations
  test_scaled[,i] = scale(test_scaled[,i])
}

cont_name = c("ps_ind_01","ps_ind_03","ps_ind_14","ps_ind_15","ps_reg_01","ps_reg_02",
              "ps_reg_03","ps_car_11","ps_car_12","ps_car_13","ps_car_14","ps_car_15",
              "ps_calc_01","ps_calc_02","ps_calc_03","ps_calc_04","ps_calc_05","ps_calc_06",
              "ps_calc_07","ps_calc_08","ps_calc_09","ps_calc_10","ps_calc_11","ps_calc_12",
              "ps_calc_13","ps_calc_14")

for (i in cont_name){ # Split up for RAM limitations
  test_scaled[,i] = scale(test_scaled[,i])
}

write.csv(test_scaled,"testset_1208.csv",row.names = F)
