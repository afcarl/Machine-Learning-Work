ds_iris = read.table(file="iris.data", header=FALSE, sep=",")
print(ds_iris)

attach(ds_iris)
ds_nD = data.frame(V1=c(cbind(V1[1:100])), V2=c(cbind(V2[1:100])), V3=c(cbind(V3[1:100])), V4=c(cbind(V4[1:100])), V5=c(cbind(V5[1:100])))
print(ds_nD)

require ("ggplot2")
ggplot (ds_nD, aes (x = V1, y = V2, colour = as.factor(V5))) + stat_density2d ()

attach(ds_nD)
df_setosa = data.frame(V1= numeric(0), V2= numeric(0), V3= numeric(0), V4= numeric(0))
df_versicolor = data.frame(V1= numeric(0), V2= numeric(0), V3= numeric(0), V4= numeric(0))

for(i in (1:length(V5))){
  if(V5[i] == 1){
    newRow = data.frame(V1= V1[i], V2= V2[i], V3= V3[i], V4= V4[i])
    df_setosa = rbind(df_setosa, newRow)
  }
  else{
    newRow = data.frame(V1= V1[i], V2= V2[i], V3= V3[i], V4= V4[i])
    df_versicolor = rbind(df_versicolor, newRow)
  }
}
print(df_setosa)
print(df_versicolor)

#Covariance setosa
cov_setosa = cov(df_setosa)
print(cov_setosa)

#Covariance versicolor
cov_versicolor = cov(df_versicolor)
print(cov_versicolor)

# Mean
# ================================================================================================================================

# Mean setosa
mean_setosa_V1 = mean(df_setosa$V1)
print(mean_setosa_V1)

mean_setosa_V2 = mean(df_setosa$V2)
print(mean_setosa_V2)

mean_setosa_V3 = mean(df_setosa$V3)
print(mean_setosa_V3)

mean_setosa_V4 = mean(df_setosa$V4)
print(mean_setosa_V4)

df_mean_setosa = data.frame(c(mean_setosa_V1, mean_setosa_V2, mean_setosa_V3, mean_setosa_V4))
print(df_mean_setosa)

# Mean versicolor
mean_versicolor_V1 = mean(df_versicolor$V1)
print(mean_versicolor_V1)

mean_versicolor_V2 = mean(df_versicolor$V2)
print(mean_versicolor_V2)

mean_versicolor_V3 = mean(df_versicolor$V3)
print(mean_versicolor_V3)

mean_versicolor_V4 = mean(df_versicolor$V4)
print(mean_versicolor_V4)

df_mean_versicolor = data.frame(c(mean_versicolor_V1, mean_versicolor_V2, mean_versicolor_V3, mean_versicolor_V4))
print(df_mean_versicolor)
# ================================================================================================================================

#Membership functions
# ================================================================================================================================

# Membership function (Setosa)

mat_ds = data.matrix(ds_nD)
v1_set = data.matrix(c(mat_ds[,1]-df_mean_setosa[1,1]))
print(v1_set)

v2_set = data.matrix(c(mat_ds[,2]-df_mean_setosa[2,1]))
print(v2_set)

v3_set = data.matrix(c(mat_ds[,3]-df_mean_setosa[3,1]))
print(v3_set)

v4_set = data.matrix(c(mat_ds[,4]-df_mean_setosa[4,1]))
print(v4_set)

var_set = data.frame(c(v1_set), c(v2_set), c(v3_set), c(v4_set))
print(var_set)

mat_setosa_cov = data.matrix(cov_setosa)
print(mat_setosa_cov)


g1_x_df = data.matrix(numeric(0))
for (i in (1:length(var_set[,1]))){
  newRow = data.matrix((data.matrix(var_set[i,])) %*% solve(mat_setosa_cov)) %*% t(data.matrix(var_set[i,]))
  g1_x_df = rbind(g1_x_df, newRow)
}
print (g1_x_df)


# Membership function (Versicolor)

# mat_versicolor = data.matrix(df_versicolor)

v1_ver = data.matrix(c(mat_ds[,1]-df_mean_versicolor[1,1]))
print(v1_ver)

v2_ver = data.matrix(c(mat_ds[,2]-df_mean_versicolor[2,1]))
print(v2_ver)

v3_ver = data.matrix(c(mat_ds[,3]-df_mean_versicolor[3,1]))
print(v3_ver)

v4_ver = data.matrix(c(mat_ds[,4]-df_mean_versicolor[4,1]))
print(v4_ver)

var_ver = data.frame(c(v1_ver), c(v2_ver), c(v3_ver), c(v4_ver))
print(var_ver)

mat_versicolor_cov = data.matrix(cov_versicolor)
print(mat_versicolor_cov)


g2_x_df = data.matrix(numeric(0))
for (i in (1:length(var_ver[,1]))){
  newRow = data.matrix((data.matrix(var_ver[i,])) %*% solve(mat_versicolor_cov)) %*% t(data.matrix(var_ver[i,]))
  g2_x_df = rbind(g2_x_df, newRow)
}
print (g2_x_df)

# ================================================================================================================================

# Compute discriminant
discriminant_list = list()
for(i in (1:100)){
  discriminant = as.numeric(g1_x_df[i]) - as.numeric(g2_x_df[i])
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
  if(discriminant_list[i] < 0){
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

attach(ds_nD)
predict_list = data.frame(c(c1_list, c2_list))
print(predict_list)

class_df = data.frame(discriminant = c(c1_list, c2_list), class = c(class_setosa, class_versicolor))
print(class_df)

# Plotting the graph 
attach(ds_nD)
plot(class_df$discriminant, V1, pch=21, bg=c("red","green3","blue")[unclass(class_df$class)], main="Prediction of Iris Data")

#10-fold cross validation
library(DAAG)
attach(ds_nD)
fit = lm(V5~V1+V2+V3+V4)
summary(cv.lm(df=ds_nD, fit, m=10))

# Confusion matrix
conf_matrix = table(class_df$class, ds_nD$V5)
print(conf_matrix)
tp = conf_matrix[1,1]
fp = conf_matrix[1,2]
fn = conf_matrix[2,1]
tn = conf_matrix[2,2]

# accuracy
acc = ((tp-fp)-(fn-tn))/length(ds_nD$V5)
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

# precision recall curve
require("ROCR")
pred = prediction(c(class_df$class), c(ds_nD$V5) )
perf <- performance(pred, "prec", "rec")
plot(perf)

