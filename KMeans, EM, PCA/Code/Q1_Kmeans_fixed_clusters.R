# Constant number of clusters (n) 

# # Iris Dataset
# #===============================================================
# dataset = read.table(file="iris.data", header=FALSE, sep=",")
# ds = dataset[,-ncol(dataset)]
# #===============================================================

# Ionosphere Dataset
#===============================================================
dataset = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data",sep=",")
ds = dataset[,-ncol(dataset)]
ds = sapply(ds, as.numeric)
#===============================================================


# # Initial Plots - Iris
# plot(dataset$V1,dataset$V2, pch=20, col=c("red","blue","green")[unclass(as.numeric(dataset$V5))])
# plot(ds$V1,ds$V2, pch = 20)

#Initial Plots - Ionosphere
plot(dataset$V4,dataset$V3, pch=20, col=c("red","blue")[unclass(as.numeric(dataset$V35))])
plot(dataset$V4,dataset$V3, pch = 20)


# Considering 'n' clusters and choosing cluster centroids
# n=3 #for Iris
n=2 #for Ionosphere

ds = as.matrix(ds)
mu = as.matrix(ds[sample(nrow(ds),n),])

for(x in 1:100){
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
  
  c_list = as.matrix(dist_list) #distance list
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
  
#   # Plotting for Iris Data 
#   #=========================================================================================
#     attach(c_ds);plot(V1,V2, pch=20, col=c("red","blue","green")[unclass(as.numeric(V5))]);
#     points(mu, pch = 3, col = "black")
#     detach(c_ds)
#   #=========================================================================================
  
  # Plotting for Ionosphere Data 
  #=========================================================================================
  attach(c_ds);plot(V4,V3, pch=20, col=c("red","blue","green")[unclass(as.numeric(V35))]);
  points(mu, pch = 3, col = "black")
  detach(c_ds)
  #=========================================================================================
  
  
  c_ds = as.matrix(c_ds)
  
  c_ds = as.data.frame(c_ds)
  c_ds = sapply(c_ds, as.numeric)
  mu = do.call("rbind", as.matrix(by(c_ds[, -ncol(c_ds)], c_ds[,ncol(c_ds)], colMeans)))  
}

# # MSE Iris
# MSE = mean((c_ds[,5]-as.numeric(dataset$V5))**2)
# MSE

#MSE Ionosphere
MSE = mean((c_ds[,35]-as.numeric(dataset$V35))**2)
MSE

rm(list=ls())
