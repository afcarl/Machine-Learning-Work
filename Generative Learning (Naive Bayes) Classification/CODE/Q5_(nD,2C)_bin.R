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
a1 = as.numeric(list())
a2 = as.numeric(list())

for( i in 1:length(ds_one)) {
  a1[i] = sum(ds_sb[i,56:57])
}
sum_1 = sum(a1)

for(i in 1:2) {
  alpha_1[i] = a1[i]/sum_1
}
j = 1
for(i in (length(ds_one)+1):length(ds_sb$V1)) {
  a2[j] = sum(ds_sb[i,56:57])
  j = j + 1
}
sum_2 = sum(a2)
for(i in 1:2) {
  alpha_2[i] = a2[i]/sum_2 
}


plot(ds_sb$V1, ds_sb$V57, pch=21, bg=c("red","green")[unclass(ds_sb$V58+1)], main="SPAMBASE DATA")

x1 = as.numeric(list())
x2 = as.numeric(list())
discriminant = 0
p_class = 0
for(j in 1:length(ds_sb$V1))
{
  for(i in 1:2)
  {
    temp1 = (choose(sum_1,ds_sb[j,c(56,57)[i]]) * alpha_1[i]^ds_sb[j,c(56,57)[i]]  * (1-alpha_1[i])^(sum_1-ds_sb[j,c(56,57)[i]]))
    if(is.nan(temp1) || is.infinite(temp1)){
      x1[i] = 1
    }
    else{ 
      x1[i] = temp1 * 0.39
    }
    
    temp2 = (choose(sum_2,ds_sb[j,c(56,57)[i]])* alpha_2[i]^ds_sb[j,c(56,57)[i]] * (1-alpha_2[i])^(sum_2-ds_sb[j,c(56,57)[i]]))
    if(is.nan(temp2) || is.infinite(temp2)){ 
      x2[i] = 1 
    }
    else{  
      x2[i] = temp2 * 0.61 
    }
  }
  g1_x = prod(x1)
  g2_x = prod(x2)
  discriminant[j] = (g1_x-g2_x)
  if(discriminant[j]>0){
    p_class[j] = 1
  }
  else  
    p_class[j] = 0  
}

plot(ds_sb$V3,discriminant, pch=21, bg=c("red","green")[unclass(p_class)+1], main="Prediction of Spambase data")

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





