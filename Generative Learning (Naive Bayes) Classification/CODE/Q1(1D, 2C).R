ds_iris = read.table(file="iris.data", header=FALSE, sep=",")
print(ds_iris)

attach(ds_iris)
ds_1D = data.frame(V1=c(cbind(V1[1:100])), V5=c(cbind(V5[1:100])))
print(ds_1D)

require ("ggplot2")
ggplot (ds_1D, aes (x = V1, y = V1, colour = as.factor(V5))) + stat_density2d ()

attach(ds_1D)
df_setosa = list()
df_versicolor = list()
for(i in (1:length(V5))){
  if(V5[i] == 1){
    df_setosa = c(df_setosa, V1[i])
  }
  else{
    df_versicolor = c(df_versicolor, V1[i])
  }
}
print(df_setosa)
print(df_versicolor)

#Mean and variance of df_setosa
mean_setosa = mean(as.numeric(df_setosa))
print(mean_setosa)

var_setosa = var(as.numeric(df_setosa))
print(var_setosa)

#Mean and variance of df_versicolor
mean_versicolor = mean(as.numeric(df_versicolor))
print(mean_versicolor)

var_versicolor = var(as.numeric(df_versicolor))
print(var_versicolor)

#Membership function

g1_list = list()
for(i in (1:(length(V1)))){
  g1_x = -log(sqrt(var_setosa)) - (((V1[i] - mean_setosa)^2) / (2*(var_setosa))) + log(0.5)
  g1_list = c(g1_list, g1_x)
}
print(g1_list)

g2_list = list()
for(i in (1:(length(V1)))){
  g2_x = -log(sqrt(var_versicolor)) - (((V1[i] - mean_versicolor)^2) / (2*(var_versicolor))) + log(0.5)
  g2_list = c(g2_list, g2_x)
}
print(g2_list)

# Compute discriminant
discriminant_list = list()
for(i in (1:(length(V1)))){
  discriminant = as.numeric(g1_list[i]) - as.numeric(g2_list[i])
  discriminant_l = c(discriminant_list, discriminant)
  discriminant_list <<- as.numeric(discriminant_l)
}
print(discriminant_list)

#Classify
c1_l = list()
c2_l = list()
class_setosa = list()
class_versicolor = list()
for(i in 1:(length(discriminant_list))){
  if(discriminant_list[i] >= 0){
    c1 = 1
    c1_l = c(c1_l, discriminant_list[i])
    c1_list <<- as.numeric(c1_l)
    class_setosa = as.numeric(c(class_setosa, c1))
  }
  else{
    c2 = 2
    c2_l = c(c2_l, discriminant_list[i])
    c2_list <<- as.numeric(c2_l) 
    class_versicolor = as.numeric(c(class_versicolor, c2))
  }
}
print(c1_list)
print(c2_list)

attach(ds_1D)
predict_list = data.frame(c(c1_list, c2_list))
print(predict_list)

class_df = data.frame(discriminant = c(c1_list, c2_list), class = c(class_setosa, class_versicolor))
print(class_df)

# Plotting the graph 
attach(ds_1D)
plot(class_df$discriminant, V1, pch=21, bg=c("red","green3","blue")[unclass(class_df$class)], main="Prediction of Iris Data")

#10-fold cross validation
library(DAAG)
attach(ds_1D)
fit = lm(V5~V1)
summary(cv.lm(df=ds_1D, fit, m=10))


# Confusion matrix
conf_matrix = table(class_df$class, ds_1D$V5)
print(conf_matrix)
tp = conf_matrix[1,1]
fp = conf_matrix[1,2]
fn = conf_matrix[2,1]
tn = conf_matrix[2,2]

# accuracy
acc = ((tp-fp)-(fn-tn))/length(ds_1D$V5)
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

