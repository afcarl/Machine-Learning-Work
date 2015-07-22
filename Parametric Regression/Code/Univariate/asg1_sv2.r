#Loading the second sample dataset and plotting the data.
# ================================================================================================================================
dataset2 = read.table(file="svar-set2.dat", header=FALSE)
attach(dataset2)
print(dataset2)
plot(dataset2)
# ================================================================================================================================


#split_fn is a function that splits the dataset into training and testing set
# ================================================================================================================================
split_fn = function(split_size){
  splitds = sample(1:nrow(dataset2), size=split_size*nrow(dataset2))
  train_set <<- dataset2[splitds,]
  print(train_set)
  test_set <<- dataset2[-splitds,]
  print(test_set)
}
split_fn(split_size=0.8)
# print(train_set)
# print(test_set)
# ================================================================================================================================


# Applying linear regression and predicting the dependent variable
# ================================================================================================================================
#lin_train is a function that fits a linear model onto the training data and determines the error(Mean Square Error).
lin_train = function(train){
  attach(train)
  res11 = list()
  res12 = list()
  for(i in 1:length(V1)){
    b11 = ((V1[i]-mean(V1))*(V2[i]-mean(V2)))
    b12 = ((V1[i]-mean(V1))^2)
    res11 = c(res11,b11)
    res12 = c(res12,b12)
  }
  res11 = Reduce("+",res11)
  print(res11)
  res12 = Reduce("+",res12)
  print(res12)
  
  res1 = res11/res12   #Derived co-efficient
  print(res1)
  
  res0 = mean(V2) - (res1*mean(V1))   #Derived intercept
  print(res0)
  
  y_hat = res0 + (res1*V1) #Linear regression hypothesis
  print (y_hat)
  plot (y_hat,V1)
  summary(y_hat)
  
  mean_train_error <<- (mean((y_hat-V2)^2)) #Mean Square Error cost function
  print(mean_train_error)
}
lin_train(train = train_set)

#lin_test is a function that fits a linear model onto the test data and determines the error(Mean Square Error).
lin_test = function(test){
  attach(test)
  res11 = list()
  res12 = list()
  for(i in 1:length(V1)){
    b11 = ((V1[i]-mean(V1))*(V2[i]-mean(V2)))
    b12 = ((V1[i]-mean(V1))^2)
    res11 = c(res11,b11)
    res12 = c(res12,b12)
  }
  res11 = Reduce("+",res11)
  print(res11)
  res12 = Reduce("+",res12)
  print(res12)
  
  res1 = res11/res12   #Derived co-efficient
  print(res1)
  
  res0 = mean(V2) - (res1*mean(V1))   #Derived intercept
  print(res0)
  
  y_hat = res0 + (res1*V1) #Linear regression hypothesis
  print (y_hat)
  plot (y_hat,V1)
  summary(y_hat)
  
  mean_test_error <<- (mean((y_hat-V2)^2)) #Mean Square Error cost function
  print(mean_test_error)
}
lin_test(test=test_set)

# ================================================================================================================================


# Performing 10-fold Cross Validation on training and test set
# ================================================================================================================================

library(DAAG)
attach(train_set)
fit = lm(V2~V1)
cv.lm(df=train_set, fit, m=10)

attach(test_set)
fit = lm(V2~V1)
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



#Testing different polynomial models on the dataset.
# ================================================================================================================================

attach(dataset2)

poly_model2 = lm(V2~(V1+I(V1^2)))#Quadratic
summary(poly_model2)

poly_model3 = lm(V2~(V1+I(V1^2)+I(V1^3)))#Cubic
summary(poly_model3)

poly_model4 = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)))#Quartic
summary(poly_model4)

poly_model4 = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)))#Quartic
summary(poly_model4)

anova(poly_model2,poly_model3,poly_model4)

# ================================================================================================================================


#Using quartic model(degree = 4) on training set
# ================================================================================================================================

poly_train = function(train){
  attach(train)
  train_poly = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)))
  plot(train_poly)
  summary(train_poly)
  plot(V1, V2, type="p", lwd=1)
  points(V1, predict(train_poly), type="l", col="red", lwd=2)
  train_pol = function(x){
    poly_model4$coefficient[5]*x^4 + poly_model4$coefficient[4]*x^3 + poly_model4$coefficient[3]*x^2 + poly_model4$coefficient[2]*x + poly_model4$coefficient[1]
  }
  curve(train_pol, col="red", type = "l", lwd=1)
  points(V1, V2, type="p", lwd=1)
}
poly_train(train=train_set)
# ================================================================================================================================


#Using quartic model(degree = 4) on test set
# ================================================================================================================================

poly_test = function(test){
  attach(test)
  test_poly = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)))
  plot(test_poly)
  summary(test_poly)
  plot(V1, V2, type="p", lwd=1)
  points(V1, predict(test_poly), type="l", col="red", lwd=2)
  test_pol = function(x){
    poly_model4$coefficient[5]*x^4 + poly_model4$coefficient[4]*x^3 + poly_model4$coefficient[3]*x^2 + poly_model4$coefficient[2]*x + poly_model4$coefficient[1]
  }
  curve(test_pol, col="red", type = "l", lwd=1)
  points(V1, V2, type="p", lwd=1)
}
poly_test(test=test_set)
# ================================================================================================================================


#Reducing the training data to 50% of the overall dataset.
# ================================================================================================================================

split_fn(split_size=0.5)
lin_train(train = train_set)
lin_test(test=test_set)
error_compare(avg_train_err=mean_train_error, avg_test_err=mean_test_error)
poly_train(train=train_set)
poly_test(test=test_set)
# ================================================================================================================================
