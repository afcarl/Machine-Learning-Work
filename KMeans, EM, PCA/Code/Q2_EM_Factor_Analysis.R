x_q = c(5.0,4.9,4.8,4.7,4.6,4.5,4.5,4.5,4.5,4.5)
x_1 = 0
y_1 = 0
for(i in 1:100)
{
  x_1[i] = x_1+0.1*i
  y_1[i] = (1.1/((2.73)^(((x_1[i]-5.6)^2)/(2*(2.3)^2))) + 2.6) #Gaussian function
}
plot(x_1,y_1,type="l", main="First Gaussian")  


x_2 = 10
y_2 = 0
for(i in 1:100)
{
  x_2[i] = x_2+0.1*i
  y_2[i] = (1.1/((2.73)^(((x_2[i]-14.3)^2)/(2*(2.3)^2))) + 2.6)
}
title = "Second Gaussian"
plot(x_2,y_2,type="l", main="Second Gaussian")

x_3 = c(x_1,x_2)
y_3 = c(y_1,y_2)
plot(x_3,y_3,type="l",main="Gaussian Mixture")

#Mapping to a higher dimension feature space 
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


mean_1 = 0
s_dev1 = 0
for(j in 1:8)
{
  mean_1[j] = mean(y_1f[,j])  
  s_dev1[j] = sd(y_1f[,j])
  for(i in 1:100)
  {      
    y_1f[i,j] = ((y_1f[i,j]-mean_1[j])/s_dev1[j])
  }
}
mean_2 = 0
s_dev2 = 0
for(j in 1:8)
{
  mean_2[j] = mean(y_2f[,j])  
  s_dev2[j] = sd(y_2f[,j])
  for(i in 1:100)
  {      
    y_2f[i,j] = ((y_2f[i,j]-mean_2[j])/s_dev2[j])
  }
}
prob_l = matrix(0,nrow = 200 ,ncol = 2)
increment  = 0

#EM
#Initializing the parameters by guessing 
EM_func = function(increment)
{
  alpha = 0
  alpha1 = 1
  alpha2 = 1
  mean_init = 12 + increment
  pl1 = 0
  pl2 = 0
  for(j in 1:100)
  {
    x = y_1f[j,] - mean_init
    x = as.matrix(x)
    co_var1 = cov(y_1f)
    co_var1 = co_var1[1,]
    co_var1 = as.matrix(co_var1)
    co_var2 = cov(y_2f)
    co_var2 = co_var2[1,]
    co_var2 = as.matrix(co_var2)
    pl1[j] = ((1/(((2*pi)^4))*(det(cov(y_1f))^(1/2))) * exp( (-1/2) * t(x)  %*% co_var1 ) )
    pl2[j] = ((1/(((2*pi)^4))*(det(cov(y_2f))^(1/2))) * exp( (-1/2) * t(x)  %*% co_var2 ) )
    x = y_2f[j,] - mean_init
    x = as.matrix(x)
    pl1[j+100] = ((1/(((2*pi)^4))*(det(cov(y_1f))^(1/2))) * exp( (-1/2) * t(x)  %*% co_var1 ) )
    pl2[j+100] = ((1/(((2*pi)^4))*(det(cov(y_2f))^(1/2))) * exp( (-1/2) * t(x)  %*% co_var2 ) )
  }
  
# Probability of L for Gaussian Mixing   
  for(i in 1:100)                             
  {
    prob_l[i,1] = (alpha2 * ((pl1[i]) / (pl1[i] + pl2[i])))
    prob_l[i,2] = (alpha1 * ((pl2[i]) / (pl1[i] + pl2[i])))
  }
  for(i in 101:200)
  {
    prob_l[i,1] = (alpha2 * ((pl2[i]) / (pl1[i] + pl2[i])))
    prob_l[i,2] = (alpha1 * ((pl1[i]) / (pl1[i] + pl2[i])))
  }
  prob_l             
  return(sum(prob_l))  
}
prob_l
q = 0
for ( i in 1:10)
{
  q[i] = EM_func(i) * x_q[i]
}
index = c(1:10)
smoothingSpline = smooth.spline(index, q, spar=0.35, tol = 0.0001)
plot(index,q, pch=".", col="blue", main = "EM Optimization", xlab = "Descent")
lines(smoothingSpline, col = "blue", lwd = 2)

rm(list=ls())
