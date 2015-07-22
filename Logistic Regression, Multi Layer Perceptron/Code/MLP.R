ds_pix = read.table(file="mfeat-pix", header=FALSE)[1:600,]

out = 0
out[1:200] = 0
out[201:400] = 1
out[400:600] = 2

require ("ggplot2")
ggplot (ds_pix, aes (V1,V2, colour = as.factor(out))) + geom_point()

# qplot(V1, V2, data = ds_pix, geom = "auto", color = out)

# Number of inputs present in the input dataset for each class.
input = 200  

# Number of activation units in the hidden layer
a_unit = 3    

# Number of output classes
y_num = 3 

# Number of Features in input dataset.
feat = 240


# Output of the first layer that acts as the input towards the second layer
op_z = array(0,dim=c(input*a_unit,a_unit))

y_hat_set = array(0,dim=c(input*a_unit,a_unit))

# List of parameters of the first layer 
theta_1_list = array(0,dim=c(input*y_num,y_num))
theta_1_list[,1] = 0.001
theta_1_list[,2] = 0.002
theta_1_list[,3] = 0.003

# Learning Rate
alpha = 0.001 

# Applying one vs. all methodology
# Setting one class as 1 and the rest as 0 
op_1 = 0
op_1[1:200] = 1
op_1[201:600] = 0
op_2 = 0
op_2[1:600] = 0
op_2[201:400] = 1
op_3 = 0
op_3[1:400] = 0
op_3[401:600] = 1
op_list =  cbind(op_1,op_2,op_3)

my_func = function(){
for(x in 1:5){

# Calculating the output of the input layer(first layer)
for(i in 1:(input*a_unit))
  for(j in 1:a_unit)
  {    
    op_z[i,j] = (1/(1+exp(-sum(ds_pix[i,])*theta_1_list[i,j])))                                 
  }
# print(op_z)


# Applying Back Propagation and updating the parameters
for(j in 1:a_unit)
{
 for(i in 1:600)
 {
  theta_1_list[i,j] = theta_1_list[i,j] - ( alpha * (sum((op_z[i,j]-op_list[i,j])*sum(ds_pix[i,] ))))
 }
}
# print(theta_1_list)

# Computing the output of the first layer with the updated parameters
# for(i in 1:(input*a_unit))
  for(j in 1:a_unit)
  {    
    op_z[i,j] = (1/(1+exp(-sum(ds_pix[i,])*theta_1_list[i,j])))                                 
  }
# print(op_z)
}


# Initializing the values of the parameters of the hidden layer(second layer) 
theta_2_list = c(0.1,0.01,0.02)
for(x in 1:20){
#Computing the final output 
for(j in 1:200)
{
  y_hat_set[j,1] =  exp(-sum(op_z[j])*theta_2_list[1])
}

for(j in 201:400)
{
  y_hat_set[j,2] =  exp(-sum(op_z[j])*theta_2_list[2])
}

for(j in 401:600)
{
  y_hat_set[j,3] =  exp(-sum(op_z[j])*theta_2_list[3])
}
# print(y_hat_set)


# Applying Back Propagation and updating the parameters
for(j in 1:a_unit)
{
  for(i in 1:600)
  {
# We use a different learning rate here     
    theta_2_list[j] = theta_2_list[j] - ( 0.00001 * (sum(((y_hat_set[i,j]-op_list[i,j])^2)*sum(ds_pix[i,] ))))
  }
}
# print(theta_2_list)

#Computing the final output 
for(j in 1:200)
{
  y_hat_set[j,1] =  exp(-sum(op_z[j])*theta_2_list[1])
}

for(j in 201:400)
{
  y_hat_set[j,2] =  exp(-sum(op_z[j])*theta_2_list[2])
}

for(j in 401:600)
{
  y_hat_set[j,3] =  exp(-sum(op_z[j])*theta_2_list[3])
}
}
print(y_hat_set)
}
my_func()
pred_op = as.matrix(c(y_hat_set[,1][1:200], y_hat_set[,2][201:400], y_hat_set[,3][401:600]))
out = as.matrix(out)

ml = array()
for(i in 1:600){
  m = (pred_op[i] - 1)^2
  ml = c(ml,m)
}
  ml = ml[-1]
  mse = mean(as.numeric(ml))
mse

# Plotting the predicted data 
plot(pred_op,out, pch=21, bg=c("red","blue","green")[unclass(pred_op+1)], main="Prediction of Data", xlab = "Predicted Output", ylab = "Class")

library("RSNNS")
t1 = system.time(replicate(1,mlp(ds_pix,y_hat_set)))
print(t1)

t2 = system.time(replicate(1,my_func()))
print(t2)

perf = c(t1[3], t2[3])
barplot(perf, main="mlp vs my_func Comparison", col="red", space =0.1, cex.axis = 0.8, las=1)
