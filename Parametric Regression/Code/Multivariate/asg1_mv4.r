
#Loading the first sample dataset and plotting the data.
# ================================================================================================================================
m_dataset4 = read.table(file="mvar-set4.dat", header=FALSE)
attach(m_dataset4)
print(m_dataset4)
plot(m_dataset4)
# ================================================================================================================================


#split_fn is a function that splits the dataset into training and testing set
# ================================================================================================================================
split_fn = function(split_size){
  splitds = sample(1:nrow(m_dataset4), size=split_size*nrow(m_dataset4))
  train_set <<- m_dataset4[splitds,]
  print(train_set)
  test_set <<- m_dataset4[-splitds,]
  print(test_set)
}
split_fn(split_size=0.8)
# print(train_set)
# print(test_set)
# ================================================================================================================================

a = lm(V6~V1+V2+V3+V4+V5)
coef(a)

# Applying linear regression and predicting the dependent variable
# ================================================================================================================================
#lin_train is a function that fits a linear model onto the training data and determines the error(Mean Square Error).
lin_train = function(train){
  attach(train)
  library(plot3D)
  theta_0 = 1.012036e-02
  theta_1 = 4.667644e-05
  theta_2 = 1.094061e-04
  theta_3 = 3.952420e-05
  theta_4 = 2.393757e-04
  theta_5 = -1.836758e-06
  
  for(i in 1:length(train)){
    y_hat <<- (theta_0+(theta_1*V1)+(theta_2*V2)+(theta_3*V3)+(theta_4*V4)+(theta_5*V5))
    error = ((y_hat-V6[i])^2)
  }
  #   persp3d(V1,V2,y_hat)
  mean_train_error <<- mean(error)
  print(mean_train_error)
  scatter3D(V1,V2,y_hat)
}
lin_train(train = train_set)


#lin_test is a function that fits a linear model onto the test data and determines the error(Mean Square Error).
lin_test = function(test){
  attach(test)
  library(plot3D)
  theta_0 = 1.012036e-02
  theta_1 = 4.667644e-05
  theta_2 = 1.094061e-04
  theta_3 = 3.952420e-05
  theta_4 = 2.393757e-04
  theta_5 = -1.836758e-06
  
  for(i in 1:length(test)){
    y_hat <<- (theta_0+(theta_1*V1)+(theta_2*V2)+(theta_3*V3)+(theta_4*V4)+(theta_5*V5))
    error = ((y_hat-V6[i])^2)
  }
  #   persp3d(V1,V2,y_hat)
  mean_test_error <<- mean(error)
  print(mean_test_error)
  plot3d(V1,V2,y_hat)
}
lin_test(test=test_set)  
# ================================================================================================================================


# Performing 10-fold Cross Validation on training and test set
# ================================================================================================================================

library(DAAG)
attach(train_set)
fit = lm(V3~V1+V2)
cv.lm(df=train_set, fit, m=10)

attach(test_set)
fit = lm(V3~V1+V2)
cv.lm(df=train_set, fit, m=10)
# ================================================================================================================================

#error_compare compares the training and testing error and represents it using a barplot
# ================================================================================================================================

error_compare = function(avg_train_err, avg_test_err){
  err = c(avg_train_err, avg_test_err)
  barplot(err, main="Training vs Testing Error Comparison",xlab = "Count",ylab="Error", col="red", space =0.1, cex.axis = 0.8, las=1, names.arg=c("Train_error", "Test_Error"))
}
error_compare(avg_train_err=mean_train_error, avg_test_err=mean_test_error)
# ================================================================================================================================


# grad_descent function derives the gradient descent to help find the local minimum
# In case of linear regression the local minimum is the same as the global minimum
#=================================================================================================================================
grad_descent = function(train){
  attach(train)
  theta_0 = 1.012036e-02
  theta_1 = 4.667644e-05
  theta_2 = 1.094061e-04
  theta_3 = 3.952420e-05
  theta_4 = 2.393757e-04
  theta_5 = -1.836758e-06
  
  alpha = 0.04
  count = 0
  
  
  for(n in 1:200){ #Performing 200 iterations
    for(i in 1:length(train)){
      #Improving the coefficient on each iteration       
      temp_0 = (theta_0 - (alpha * (1/length(train)*((theta_0+(theta_1*V1[i])+(theta_2*V2[i])+(theta_3*V3[i])+(theta_4*V4[i])+(theta_5*V5[i]))-V6[i]))))
      temp_1 = (theta_1 - (alpha * (1/length(train)*((theta_0+(theta_1*V1[i])+(theta_2*V2[i])+(theta_3*V3[i])+(theta_4*V4[i])+(theta_5*V5[i]))-V6[i]))))
      temp_2 = (theta_2 - (alpha * (1/length(train)*((theta_0+(theta_1*V1[i])+(theta_2*V2[i])+(theta_3*V3[i])+(theta_4*V4[i])+(theta_5*V5[i]))-V6[i]))))
      
      theta_0 = temp_0
      theta_1 = temp_1
      theta_2 = temp_2
      
      y_hat = (theta_0+(theta_1*V1)+(theta_2*V2)+(theta_3*V3)+(theta_4*V4)+(theta_5*V5))
      error = ((y_hat-V6[i])^2)
      #   error = ((1/(2*length(dataset_m1)))*((y_hat-V2[i])^2))
      
      count = count+1
      
    }
    MSE <<- c(mean_train_error)
    MSE = c(MSE,mean(error)) #Obtaining the mean square error for each iteration
    print(MSE)
    aa <<- (MSE[length(MSE)])
    plot(MSE)
    
  }
}
grad_descent(train = train_set)

#=================================================================================================================================


#soln_compare compares the error in explicit and iterative solutions and represents it using a barplot
#=================================================================================================================================

soln_compare = function(explicit_err, iterative_err){
  err = c(explicit_err, iterative_err)
  barplot(err, main="Explicit vs Iterative Solution Error Comparison",xlab = "Count",ylab="Error", col="red", space =0.1, cex.axis = 0.8, las=1, names.arg=c("Explicit_error", "Iterative_Error"))
}
soln_compare(explicit_err=mean_train_error, iterative_err=(aa))

#=================================================================================================================================

# gauss derives the Gaussian kernel density
#=================================================================================================================================

gaus = function(train){
  d = density((V1+V2+V3+V4+V5), bw = 0.3, kernel="gaussian")
  plot(d, main = "Gaussian Kernel Density Function")
  #   points(V1,V2,V3,V4,V5,type="p",lwd=1)
}
gaus(train=train_set)

#=================================================================================================================================
