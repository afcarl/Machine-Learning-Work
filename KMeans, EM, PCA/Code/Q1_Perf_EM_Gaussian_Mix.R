set.seed(123)
tau_1_true = 0.25
x = y = rep(0,1000)
for( i in 1:1000 ) {
  if( runif(1) < tau_1_true ) {
    x[i] = rnorm(1, mean=1)
    y[i] = "heads"
  } else {
    x[i] = rnorm(1, mean=7)
    y[i] = "tails"
  }
}

ds = cbind(x,y)
ds

dataset = as.data.frame(ds)

library("car")
densityPlot( ~x, par.settings = list(plot.symbol = list(col=as.factor(y))))

plot(as.integer(dataset$x), pch=20, col=c("red","blue")[unclass(as.numeric(dataset$y))], main="Gaussian Mix")

mu_1 = 0
mu_2 = 1

tau_1 = 0.5
tau_2 = 0.5

for( i in 1:10 ) {
  
  T_1 = tau_1 * dnorm( x, mu_1 )
  T_2 = tau_2 * dnorm( x, mu_2 )
  
  P_1 = T_1 / (T_1 + T_2)
  P_2 = T_2 / (T_1 + T_2) ## note: P_2 = 1 - P_1
  
  tau_1 = mean(P_1)
  tau_2 = mean(P_2)
  
  mu_1 = sum( P_1 * x ) / sum(P_1)
  mu_2 = sum( P_2 * x ) / sum(P_2)
  
  print( c(mu_1, mu_2, tau_1, tau_2) )
  
}

mu = rbind(mu_1, mu_2)
#================================================


n=2

ds = as.matrix(as.numeric(ds[,1]))

dist_list = list()
for (j in (1:n)){
  dist = data.matrix(numeric(0))
  for(i in (1:nrow(ds))){
    d = t(ds[i,]-mu[j,])%*%(ds[i,]-mu[j,])
    dist = rbind(dist,d)
  }
  dist_list = cbind(dist_list,dist)
}
dist_list = as.matrix(dist_list)
dist_list

c_list = as.matrix(dist_list)
c_list

cc = list()

for(i in (1:nrow(c_list))){
  for(j in (1:ncol(c_list))){
    if(min(as.numeric(c_list[i,])) == c_list[i,j]){
      cc = rbind(cc,j)
    }
  }
}
cc

c_list = cbind(c_list,cc)
c_ds = as.data.frame(cbind(ds,cc))
ccc <<- c_ds

attach(c_ds);plot(as.integer(V1), pch=20, col=c("red","blue")[unclass(as.numeric(V2))], main="Gaussian Mix");
points(mu, pch = 3, col = "black")
detach(c_ds)

MSE = mean((as.numeric(c_ds[,2])-as.numeric(dataset$y))**2)
MSE

rm(list=ls())
