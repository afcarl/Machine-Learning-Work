ds_iris = read.table(file="iris.data", header=FALSE, sep=",")
print(ds_iris)

attach(ds_iris)
ds_nD = data.frame(V1=c(cbind(V1[1:100])), V2=c(cbind(V2[1:100])), V3=c(cbind(V3[1:100])), V4=c(cbind(V4[1:100])), V5=c(cbind(V5[1:100])))
print(ds_nD)

require ("ggplot2")
ggplot (ds_nD, aes (x = V1, y = V2, colour = as.factor(V5))) + stat_density2d ()

#oplist has Class1 as 1 and Class2 as 0 
op_list = data.matrix(integer(0))
for(i in 1:length(ds_nD$V5)){
  if(ds_nD$V5[i] == 1){
    op_list = rbind(op_list, 1)
  }
  else{
    op_list = rbind(op_list, 0)
  }
}
print(op_list)


# ================================================================================================================================
# ================================================================================================================================
# ================================================================================================================================
# ================================================================================================================================


#Performing logistic regression
#Optimizing gradient using Newton method
# ================================================================================================================================

logit = function(){
theta_0 = -1
theta_1 = -1
theta_2 = 0.5
theta_3 = 0.5
theta_4 = 1

theta_list = data.matrix(c(theta_0, theta_1, theta_2, theta_3, theta_4))

x_set_iris = data.matrix(ds_nD[,-5])
x_set = cbind(1,x_set_iris)
print(x_set)


for(x in 1:50){
  y_hat_set = data.matrix(numeric(0))
  for(i in 1:length(ds_nD$V5)){
    y_hat = (1/(1+exp(-(x_set[i,]%*%theta_list))))
    y_hat_set = rbind(y_hat_set, y_hat)
    p <<- y_hat_set
  }
  print(y_hat_set)
  
  #gradient
  # ================================================================================================================================
  
  gradient = t(x_set[])%*%(op_list - y_hat_set)
  print(gradient)
  # ================================================================================================================================
  
  temp_vector = data.matrix(numeric(0))
  for(i in 1:100){
    #   temp = (y_hat_set[i] * (1 - y_hat_set[i]))
    temp_vector = c(temp_vector, (y_hat_set[i] * (1 - y_hat_set[i])))
  }
  print(temp_vector)
  
  diag_set = diag(temp_vector)
  
  # hessian 
  # ================================================================================================================================
  
  hessian = t(x_set[])%*%diag_set%*%x_set[]
  print(hessian)
  # ================================================================================================================================
  
  theta_list = theta_list + 0.01*(solve(hessian)%*%gradient)
}
print(theta_list)
}
logit()

# ================================================================================================================================
# ================================================================================================================================
# ================================================================================================================================
# ================================================================================================================================

# Classify
# ================================================================================================================================

class_1 = data.frame(numeric(0))
class_0 = data.frame(numeric(0))

for(i in 1:(length(y_hat_set))){
  if (y_hat_set[i] >= mean(y_hat_set)){
    class_1 = rbind(class_1, 1)
  }
  else{
    class_0 = rbind(class_0, 0)
  }
}
class_1 = class_1[!is.na(class_1)]
class_0 = class_0[!is.na(class_0)]
print(class_1)
print(class_0)

pred_op = c(class_1, class_0)
print(pred_op)
# ================================================================================================================================

#Plotting our prediction 
plot(y_hat_set,ds_nD$V5, pch=21, bg=c("red","blue")[unclass(pred_op+1)], main="Prediction of Iris Data", xlab = "Predicted Output", ylab = "Class")


#Testing different polynomial models on the dataset.
# ================================================================================================================================

attach(ds_nD)

poly_model2 = glm(V5 ~ (poly(V1, 2)+poly(V2, 2)+poly(V3, 2)+poly(V4, 2)))#Quadratic
summary(poly_model2)


poly_model3 = glm(V5 ~ (poly(V1, 3)+poly(V2, 3)+poly(V3, 3)+poly(V4, 3)))#Cubic
summary(poly_model3)

poly_model4 = glm(V5 ~ (poly(V1, 4)+poly(V2, 4)+poly(V3, 4)+poly(V4, 4)))#Quartic
summary(poly_model4)

anova(poly_model2,poly_model3)
# ================================================================================================================================

poly_train = function(train){
  attach(ds_iris)
  train_poly = glm(V2~(V1+I(V1^2)))
  plot(train_poly)
  summary(train_poly)
  plot(V1, V2, type="p", lwd=1)
  points(V1, predict(train_poly), type="l", col="red", lwd=2)
  train_pol = function(x){
    poly_model2$coefficient[3]*x^2 + poly_model2$coefficient[2]*x + poly_model2$coefficient[1]
  }
  curve(train_pol, col="red", type = "l", lwd=1)
  points(V1, V2, type="p", lwd=1)
}
poly_train(train=ds_iris)

st = system.time(replicate(5, logit()))
