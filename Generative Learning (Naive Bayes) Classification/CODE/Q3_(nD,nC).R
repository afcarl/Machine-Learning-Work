d = read.table(file="iris.data", header=FALSE, sep=",")
print(d)

df_iris = data.frame(c(d))
df_iris = data.frame(lapply(df_iris,as.numeric))
print(df_iris)

require ("ggplot2")
ggplot (df_iris, aes (x = V1, y = V2, colour = as.factor(V5))) + stat_density2d ()

attach(df_iris)
df_setosa = data.frame(V1= numeric(0), V2= numeric(0), V3= numeric(0), V4= numeric(0))
df_versicolor = data.frame(V1= numeric(0), V2= numeric(0), V3= numeric(0), V4= numeric(0))
df_virginica = data.frame(V1= numeric(0), V2= numeric(0), V3= numeric(0), V4= numeric(0))

for(i in (1:length(V5))){
  if(V5[i] == 1){
    newRow = data.frame(V1= V1[i], V2= V2[i], V3= V3[i], V4= V4[i])
    df_setosa = rbind(df_setosa, newRow)
  }
  else{
    if(V5[i] == 2){
      newRow = data.frame(V1= V1[i], V2= V2[i], V3= V3[i], V4= V4[i])
      df_versicolor = rbind(df_versicolor, newRow)
    }
    else{
      newRow = data.frame(V1= V1[i], V2= V2[i], V3= V3[i], V4= V4[i])
      df_virginica = rbind(df_virginica, newRow)    
    }
  }
}
print(df_setosa)
print(df_versicolor)
print(df_virginica)


#Covariance setosa
cov_setosa = cov(df_setosa)
print(cov_setosa)

#Covariance versicolor
cov_versicolor = cov(df_versicolor)
print(cov_versicolor)

#Covariance virginica
cov_virginica = cov(df_virginica)
print(cov_virginica)

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


# Mean virginica
mean_virginica_V1 = mean(df_virginica$V1)
print(mean_virginica_V1)

mean_virginica_V2 = mean(df_virginica$V2)
print(mean_virginica_V2)

mean_virginica_V3 = mean(df_virginica$V3)
print(mean_virginica_V3)

mean_virginica_V4 = mean(df_virginica$V4)
print(mean_virginica_V4)

df_mean_virginica = data.frame(c(mean_virginica_V1, mean_virginica_V2, mean_virginica_V3, mean_virginica_V4))
print(df_mean_virginica)
# ================================================================================================================================

#Membership functions
# ================================================================================================================================

# Membership function (Setosa)

mat_ds = data.matrix(df_iris)
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


# Membership function (Virginica)

v1_vg = data.matrix(c(mat_ds[,1]-df_mean_virginica[1,1]))
print(v1_vg)

v2_vg = data.matrix(c(mat_ds[,2]-df_mean_virginica[2,1]))
print(v2_vg)

v3_vg = data.matrix(c(mat_ds[,3]-df_mean_virginica[3,1]))
print(v3_vg)

v4_vg = data.matrix(c(mat_ds[,4]-df_mean_virginica[4,1]))
print(v4_vg)

var_vg = data.frame(c(v1_vg), c(v2_vg), c(v3_vg), c(v4_vg))
print(var_vg)

mat_virginica_cov = data.matrix(cov_virginica)
print(mat_virginica_cov)


g3_x_df = data.matrix(numeric(0))
for (i in (1:length(var_vg[,1]))){
  newRow = data.matrix((data.matrix(var_vg[i,])) %*% solve(mat_virginica_cov)) %*% t(data.matrix(var_vg[i,]))
  g3_x_df = rbind(g3_x_df, newRow)
}
print (g3_x_df)

# ================================================================================================================================                                                                                                                  
# Compute discriminant
discriminant_list = list()
for(i in (1:150)){
  discriminant = ((as.numeric(g1_x_df[i]) - as.numeric(g2_x_df[i])) + (as.numeric(g2_x_df[i]) - as.numeric(g3_x_df[i])) + (as.numeric(g1_x_df[i]) - as.numeric(g3_x_df[i])))/150
  discriminant_l = c(discriminant_list, discriminant)
  discriminant_list <<- as.numeric(discriminant_l)
}
print(discriminant_list)

i1 = list()
for(i in 1:50){
  i1 = c(i1, discriminant_list[i])
}
i1 = as.numeric(i1)
db1 = max(i1)


i2 = list()
for(i in 50:100){
  i2 = c(i2, discriminant_list[i])
}
i2 = as.numeric(i2)
db2 = max(i2)

i3 = list()
for(i in 101:150){
  i3 = c(i3, discriminant_list[i])
}
i3 = as.numeric(i3)
db3 = max(i3)
# ================================================================================================================================                                                                                                                  

#Classify
c1_l = list()
c2_l = list()
c3_l = list()
class_setosa = list()
class_versicolor = list()
class_virginica = list()
for(i in 1:(length(discriminant_list))){
  if(discriminant_list[i] <= db1){
    c1 = 1
    c1_l = c(c1_l, discriminant_list[i])
    c1_list <<- as.numeric(c1_l)
    class_setosa = as.numeric(c(class_setosa, c1))
  }
  else{
    if(discriminant_list[i] <= db2){
    c2 = 2
    c2_l = c(c2_l, discriminant_list[i])
    c2_list <<- as.numeric(c2_l) 
    class_versicolor = as.numeric(c(class_versicolor, c2))
    }
    else{
      c3 = 3
      c3_l = c(c3_l, discriminant_list[i])
      c3_list <<- as.numeric(c3_l) 
      class_virginica = as.numeric(c(class_virginica, c3))      
    }
  }
}
print(c1_list)
print(c2_list)
print(c3_list)

attach(df_iris)
predict_list = data.frame(c(c1_list, c2_list, c3_list))
print(predict_list)

class_df = data.frame(discriminant = c(c1_list, c2_list, c3_list), class = c(class_setosa, class_versicolor, class_virginica))
print(class_df)

# Plotting the graph 
attach(df_iris)
plot(class_df$discriminant, V1, pch=21, bg=c("red","green3","blue")[unclass(class_df$class)], main="Prediction of Iris Data")

#10-fold cross validation
library(DAAG)
attach(df_iris)
fit = lm(V5~V1+V2+V3+V4)
summary(cv.lm(df=df_iris, fit, m=10))

# Confusion matrix
conf_matrix = table(class_df$class, df_iris$V5)
print(conf_matrix)
t11 = conf_matrix[1,1]
f12 = conf_matrix[1,2]
f13 = conf_matrix[1,3]
f21 = conf_matrix[2,1]
t22 = conf_matrix[2,2]
f23 = conf_matrix[2,3]
f31 = conf_matrix[3,1]
f32 = conf_matrix[3,2]
t33 = conf_matrix[3,3]

# accuracy
acc = ((t11-(f12+f13))+(t22-(f21+f23))+(t33-(f31+f32)))/length(df_iris$V5)
print(acc)

# precision
prec_1 = t11/(t11+f12+f13)
print(prec_1)

prec_2 = t22/(t22+f21+f23)
print(prec_2)

prec_3 = t33/(t33+f31+f32)
print(prec_3)

# recall
rec_1 = t11/(t11+f21+f31)
print(rec_1)

rec_2 = t22/(t22+f12+f32)
print(rec_2)

rec_3 = t33/(t33+f13+f23)
print(rec_3)

# f-measure
f_1 = 2*((prec_1*rec_1)/(prec_1+rec_1))
print(f_1)

f_2 = 2*((prec_2*rec_2)/(prec_2+rec_2))
print(f_2)

f_3 = 2*((prec_3*rec_3)/(prec_3+rec_3))
print(f_3)
