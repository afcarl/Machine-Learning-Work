ds_sb = read.table(file="spambase.data", header=FALSE, sep=",")
# print(ds_sb)

ds_one = as.integer(list())
ds_zero = as.integer(list())

attach(ds_sb)
for(i in (1:length(V58))){
  if(V58[i] == 1){
    ds_one = c(ds_one, V58[i])
  }
  else{
    ds_zero = c(ds_zero, V58[i])
  }
}
print(ds_one)
print(ds_zero)

alpha_1 = as.numeric(list())
alpha_2 = as.numeric(list())

for(i in 1:(length(ds_sb)-1)) { 
  for(j in 1:length(ds_one)){ 
    alpha_1 = sum(ds_sb[j,i])/length(ds_one) 
  } 
}

for(i in 1:(length(ds_sb)-1)) { 
  for(j in length(ds_one):length(V58)){ 
    alpha_2[i] = sum(ds_sb[j,i])/length(ds_zero)  
  } 
}

for(i in 1:(length(ds_sb)-1)) {  
  alpha_1[i] <- (sum(ds_sb[1:1813,i])/length(ds_one)) 
}
for(i in 1:(length(ds_sb)-1)) {
  alpha_2[i] <- (sum(ds_sb[1814:4601,i])/length(ds_zero)) 
} 

require("lattice")
xyplot(V1 ~ V57| V58, ds_sb, groups = ds_sb$V58, pch= 0, , main="SPAMBASE DATA")


g1_x_df = 1
g2_x_df = 1
val_1 = as.numeric(list())
val_2 = as.numeric(list())
discriminant = 0
p_class = 0

for(j in 1:4601)
{
  for(i in 1:57) 
  {   
    if(is.infinite(alpha_1[i]^ds_sb [j,i]) || is.infinite(1-alpha_1[i])^(1-ds_sb [j,i]))
    { 
      val_1[i] = 999999999999999999
    }
    else
    {
      if(ds_sb [j,i]>0)
      {val_1[i] = (alpha_1[i])^ds_sb [j,i] }
      else
      {val_1[i] = (1-alpha_1[i])^(1-ds_sb [j,i])    }
      g1_x_df <<- g1_x_df * val_1[i]
    }
    if(is.infinite(alpha_2[i]^ds_sb [j,i]) || is.infinite(1-alpha_1[i])^(1-ds_sb [j,i]))
    {
      val_2[i] = 99999999999
    }
    else
    {
      if(ds_sb [j,i]>0)
      {val_2[i] = (alpha_2[i])^ds_sb [j,i]}
      else
      {val_2[i] = (1-alpha_2[i])^(1-ds_sb [j,i])}
      g2_x_df <<- g2_x_df * val_2[i]        
    }
  }
  discriminant[j] = (g1_x_df-g2_x_df)
  
  if(discriminant[j] > 0) { 
    p_class[j] = 1 
  } 
  else { 
    p_class[j] = 0 
  }
  g1_x_df= 1
  g2_x_df= 1
}

#10-fold cross validation
library(DAAG)
attach(ds_sb)
fit = lm(V5~V1+V2+V3+V4)
summary(cv.lm(df=ds_sb, fit, m=10))


# Confusion matrix
conf_matrix = table(ds_sb$V58, p_class)
print(conf_matrix)
tp = conf_matrix[1,1]
fp = conf_matrix[1,2]
fn = conf_matrix[2,1]
tn = conf_matrix[2,2]
# accuracy
acc = ((tp-fp)-(fn-tn))/4601
print(acc)

# precision
prec_1 = tp/(tp+fp)
print(prec_1)

prec_2 = tn/(tn+fn)
print(prec_2)

# recall
rec_1 = tp/(tp+fn)
print(rec_1)

rec_2 = tn/(tn+fp)
print(rec_2)

# f-measure
f_1 = 2*((prec_1*rec_1)/(prec_1+rec_1))
print(f_1)

f_2 = 2*((prec_2*rec_2)/(prec_2+rec_2))
print(f_2)



plot(discriminant, ds_sb$V1, pch=21, bg=c("red","green")[unclass(p_class+1)], main="Prediction of Spambase data")
