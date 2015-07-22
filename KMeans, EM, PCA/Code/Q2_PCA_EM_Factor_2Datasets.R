# Using R built-in datasets with multiple labels
x = ChickWeight
y = DNase

plot(x)
plot(y)

x_lab = x$Diet
x_1 = x$weight
y_1 = x$Time
x_12 = x$Chick
y_12 = x$Diet
y_1f = 0
y_1f = cbind(x_1,y_1,x_12,y_12)

y_lab = y$Run
x_2 = y$conc
y_2 = y$density
y_2f = 0
y_2f = cbind(x_2,y_2)

mean_1 = 0
s_dev1 = 0
for(j in 1:4)
{
  mean_1[j] = mean(y_1f[j,])  
  s_dev1[j] = sd(y_1f[j,])
  for(i in 1:176)
  {
    y_1f[i,j] = ((y_1f[i,j]-mean_1[j])/s_dev1[j])
  }
}

mean_2 = 0
s_dev2 = 0
for(j in 1:2)
{
  mean_2[j] = mean(y_2f[j,])  
  s_dev2[j] = sd(y_2f[j,])
  for(i in 1:100)
  {
    y_2f[i,j] = ((y_2f[i,j]-mean_2[j])/s_dev2[j])
  }
}

sigma = 0
co_var = 0
for(i in 1:4)
{
  x = y_1f[,i]
  x = as.matrix(x)
  sigma = sigma + (x %*% t(x) )
  co_var [i]= cov(x) 
}
#Eigen values 
w = eigen(sigma)
w

rm(list=ls())
