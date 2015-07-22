x_1 = 0
y_1 = 0
for(i in 1:100)
{
  x_1[i] = x_1+0.1*i
  y_1[i] = (1.1/((2.73)^(((x_1[i]-5.6)^2)/(2*(2.3)^2))) + 2.6)
}
plot(x_1,y_1,type = "l",main="First Gaussian")  


x_2 = 10
y_2 = 0
for(i in 1:100)
{
  x_2[i] = x_2+0.1*i
  y_2[i] = (1.1/((2.73)^(((x_2[i]-14.3)^2)/(2*(2.3)^2))) + 2.6)
}
title = "Second Gaussian"
plot(x_2,y_2,type="l",main="Second Gaussian")

x_3 = c(x_1,x_2)
y_3 = c(y_1,y_2)
plot(x_3,y_3,type="l",main="Gaussian Mixture")

# Mapping data to higher dimensions 

x_12 = x_1 ^ 2
y_12 = y_1 ^ 2
x_1y_1 = x_1 * y_1
x_1y_12 = x_1 * y_12
x_12y_1 = x_12 * y_1
x_12y_12 = x_12 * y_12


x_22 = x_2 ^ 2
y_22 = y_2 ^ 2
x_2y_2 = x_2 * y_2
x_2y_22 = x_2 * y_22
x_22y_2 = x_22 * y_2
x_22y_22 = x_22 * y_22

y_1f = 0
y_1f = cbind(x_1,y_1,x_12,y_12,x_1y_1,x_12y_1,x_1y_12,x_12y_12)
y_1f
y_2f = 0
y_2f = cbind(x_2,y_2,x_22,y_22,x_2y_2,x_22y_2,x_2y_22,x_22y_22)
y_2f


# Normalizing the data 
mean_1 = 0
sdev_1 = 0
for(j in 1:8)
{
  mean_1[j] = mean(y_1f[j,])  
  sdev_1[j] = sd(y_1f[j,])
  for(i in 1:100)
  {
    y_1f[i,j] = ((y_1f[i,j]-mean_1[j])/sdev_1[j])
  }
}
mean_2 = 0
sdev_2 = 0
for(j in 1:8)
{
  mean_2[j] = mean(y_2f[j,])  
  sdev_2[j] = sd(y_2f[j,])
  for(i in 1:100)
  {
    y_2f[i,j] = ((y_2f[i,j]-mean_2[j])/sdev_2[j])
  }
}

# Deriving the Standard Deviation 
sigma = 0
co_var = 0
for(i in 1:8)
{
  x = y_1f[,i]
  x = as.matrix(x)
  sigma = sigma + (x %*% t(x) )
  co_var [i]= cov(x) 
}
eigen(sigma) 

# PCA Features
# EM
#============================================================================== 
#COMPUTING P(L|X[I];theta)
Prob_of_l = function()
{
# Probability of L for the First Gaussian 
  prob_l = 0
  alpha = 0                  
  alpha[1] = 100
  alpha[2] = 100
  sigma1 = 150
  pl = 0
  mean_1 = 4
  sdev_1 = 0
  y_1f = cbind(x_1,y_1,x_12,y_12,x_1y_1,x_12y_1,x_1y_12,x_12y_12)
  for(j in 1:8)
  {
    mean_1[j] = mean(y_1f[,j])  
    sdev_1[j] = sd(y_1f[,j])
    for(i in 1:100)
    {      
      y_1f[i,j] = ((y_1f[i,j]-mean_1[j])/sdev_1[j])
    }
  }
  mu_init = 12
  mean_1 = mean_1 
  x = y_1f[1,] - mu_init 
  x = as.matrix(x)
  co_var = cov(y_1f)
  co_var = co_var[1,]
  co_var = as.matrix(co_var)
  pl[1] = ((1/(((2*pi)^4))*(det(cov(y_1f))^(1/2))) * exp( (-1/2) * t(x)  %*% co_var ) )
  pl

  
# Probability of L for the Second Gaussian   
  y_2f = cbind(x_2,y_2,x_22,y_22,x_2y_2,x_22y_2,x_2y_22,x_22y_22)
  mean_2 = 0
  sdev_2 = 0
  for(j in 1:8)
  {
    mean_2[j] = mean(y_2f[,j])  
    sdev_2[j] = sd(y_2f[,j])
    for(i in 1:100)
    {
      y_2f[i,j] = ((y_2f[i,j]-mean_2[j])/sdev_2[j])
    }
  }
  mean_2 = mean_2
  x = y_2f[1,] - mu_init
  x = as.matrix(x)
  co_var = cov(y_2f)
  co_var = co_var[1,]
  co_var = as.matrix(co_var)
  pl[2] = ((1/(((2*pi)^4))*(det(cov(y_2f))^(1/2))) * exp( (-1/2) * t(x)  %*% co_var ) )

# Probability of L for the Mixed Gaussians 
  for(i in 1:2)                               
  {
    prob_l[i] = alpha[i] * pl[i] / sum(alpha * pl)
  }
  prob_l
}
rm(list=ls())
