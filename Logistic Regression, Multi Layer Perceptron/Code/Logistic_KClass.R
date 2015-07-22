ds_iris = read.table(file="iris.data", header=FALSE, sep=",")
print(ds_iris)

attach(ds_iris)
ds_iris = data.frame(V1=c(cbind(V1)), V2=c(cbind(V2)), V3=c(cbind(V3)), V4=c(cbind(V4)), V5=c(cbind(V5)))
print(ds_iris)

require ("ggplot2")
ggplot (ds_iris, aes (x = V1, y = V2, colour = as.factor(V5))) + stat_density2d ()


# This sets Class1 as 1 and the rest as 0 
op_list_1 = data.matrix(integer(0))
for(i in 1:length(ds_iris$V5)){
  if(ds_iris$V5[i] == 1){
    op_list_1 = rbind(op_list_1, 1)
  }
  else{
    op_list_1 = rbind(op_list_1, 0)
  }
}
print(op_list_1)

# This sets Class2 as 1 and the rest as 0 
op_list_2 = data.matrix(integer(0))
for(i in 1:length(ds_iris$V5)){
  if(ds_iris$V5[i] == 2){
    op_list_2 = rbind(op_list_2, 1)
  }
  else{
    op_list_2 = rbind(op_list_2, 0)
  }
}
print(op_list_2)

# This sets Class3 as 1 and the rest as 0 
op_list_3 = data.matrix(integer(0))
for(i in 1:length(ds_iris$V5)){
  if(ds_iris$V5[i] == 3){
    op_list_3 = rbind(op_list_3, 1)
  }
  else{
    op_list_3 = rbind(op_list_3, 0)
  }
}
print(op_list_3)

# ================================================================================================================================
# ================================================================================================================================
# ================================================================================================================================
# ================================================================================================================================

pred_op1 = data.matrix(numeric(0))
pred_op2 = data.matrix(numeric(0))
pred_op3 = data.matrix(numeric(0))


# This function performs logistic regression and gradient descent 
log_reg = function(op_list){
  
  
  theta_0 = 0.5
  theta_1 =-0.1
  theta_2 = 0.1
  theta_3 = -0.5
  theta_4 = 0.3
  
  
  theta_list = data.matrix(c(theta_0, theta_1, theta_2, theta_3, theta_4))
  combine_theta = data.frame(numeric(0))
  
  x_set_iris = data.matrix(ds_iris[,-5])
  x_set = cbind(1,x_set_iris)
  
  for(x in 1:1000){
    combine_theta = c(combine_theta, theta_list)
    y_hat_set = data.matrix(numeric(0))
    
    
    for(i in 1:length(ds_iris$V5)){
      y_hat = (1/(1+exp(-(x_set[i,]%*%theta_list))))
      y_hat_set = rbind(y_hat_set, y_hat)
      pred_op <<- y_hat_set
    }
    
    
    # ================================================================================================================================
    
    alpha = 0.1
    temp = 0
    tl = matrix(numeric(0), ncol = 5 )
    for(i in 1:length(ds_iris$V5)){
      temp = theta_list - alpha * 1/length(x_set[,1]) * (y_hat_set[i] - op_list[i]) * x_set[i,]
      tl = rbind(tl,t(temp))
      
      
    }
    theta_list = tl
    
    tl1 = as.numeric(data.frame(c(min(theta_list[,1])), min(theta_list[,2]),min(theta_list[,3]),min(theta_list[,4]),min(theta_list[,5])))
    theta_list = tl1
    
    
    
  }
  
  plot(y_hat_set, main="Prediction of Iris Data", ylab= "Predicted value")
  plot(as.matrix(combine_theta), main="Optimizing the Parameters using Gradient Descent",  ylab = "Parameters (Theta)")
  
  print(y_hat_set)
  
  print(as.matrix(theta_list))
}
log_reg(op_list = op_list_1)
pred_op1 = pred_op

log_reg(op_list = op_list_2)
pred_op2 = pred_op

log_reg(op_list = op_list_3)
pred_op3 = pred_op

pred_op_list = cbind(pred_op1, pred_op2, pred_op3)

output = data.matrix(numeric(0))
for(i in 1:150){
  m_func = max(pred_op_list[i,])
  output = (c(output, m_func))
}
print(output)
# ================================================================================================================================
# ================================================================================================================================
# ================================================================================================================================
# ================================================================================================================================

# Evaluating the performance of our algorithm wrt time 
st1 = system.time(replicate(5, log_reg(op_list = op_list_1)))
st2 = system.time(replicate(5, log_reg(op_list = op_list_2)))
st3 = system.time(replicate(5, log_reg(op_list = op_list_3)))


# ================================================================================================================================

#Plotting our prediction 
ds_pred = cbind(ds_iris[,-5], output)
plot(ds_pred$output, ds_iris$V5, pch=21, bg=c("red","green3","blue")[unclass(ds_iris$V5)], main="Prediction of Iris Data", xlab = "Predicted Output", ylab = "Class")

