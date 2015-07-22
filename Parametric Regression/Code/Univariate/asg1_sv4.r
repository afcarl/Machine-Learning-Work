#Loading the first sample dataset and plotting the data.
# ================================================================================================================================
dataset4 = read.table(file="svar-set4.dat", header=FALSE)
attach(dataset4)
print(dataset4)
plot(dataset4)
# ================================================================================================================================


#split_fn is a function that splits the dataset into training and testing set
# ================================================================================================================================
split_fn = function(split_size){
  splitds = sample(1:nrow(dataset4), size=split_size*nrow(dataset4))
  train_set <<- dataset4[splitds,]
  print(train_set)
  test_set <<- dataset4[-splitds,]
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

attach(dataset4)

poly_model2 = lm(V2~(V1+I(V1^2)))#Quadratic
summary(poly_model2)

poly_model3 = lm(V2~(V1+I(V1^2)+I(V1^3)))#Cubic
summary(poly_model3)

poly_model4 = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)))#Quartic
summary(poly_model4)

poly_model5 = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)+I(V1^5)))#Degree=5
summary(poly_model5)

poly_model6 = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)+I(V1^5)+I(V1^6)))#Degree=6
summary(poly_model6)

poly_model7 = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)+I(V1^5)+I(V1^6)+I(V1^7)))#Degree=7
summary(poly_model7)

poly_model8 = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)+I(V1^5)+I(V1^6)+I(V1^7)+I(V1^8)))#Degree=8
summary(poly_model8)

poly_model10 = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)+I(V1^5)+I(V1^6)+I(V1^7)+I(V1^8)+I(V1^9)+I(V1^10)))#Degree=10
summary(poly_model10)

poly_model12 = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)+I(V1^5)+I(V1^6)+I(V1^7)+I(V1^8)+I(V1^9)+I(V1^10)+I(V1^11)+I(V1^12)))#Degree=12
summary(poly_model12)

poly_model15 = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)+I(V1^5)+I(V1^6)+I(V1^7)+I(V1^8)+I(V1^9)+I(V1^10)+I(V1^11)+I(V1^12)+I(V1^13)+I(V1^14)+I(V1^15)))#Degree=15
summary(poly_model15)
# anova(poly_model2,poly_model3)
# ================================================================================================================================


#Using degree=12 model on training set
# ================================================================================================================================

poly_train = function(train){
  attach(train)
  train_poly = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)+I(V1^5)+I(V1^6)+I(V1^7)+I(V1^8)+I(V1^9)+I(V1^10)+I(V1^11)+I(V1^12)))
  plot(train_poly)
  summary(train_poly)
  plot(V1, V2, type="p", lwd=1)
  points(V1, predict(train_poly), type="l", col="red", lwd=2)
  train_pol = function(x){
    poly_model12$coefficient[13]*x^12 + poly_model12$coefficient[12]*x^11 + poly_model12$coefficient[11]*x^10 + poly_model12$coefficient[10]*x^9 + poly_model12$coefficient[9]*x^8 + poly_model12$coefficient[8]*x^7 +poly_model12$coefficient[7]*x^6 +poly_model12$coefficient[6]*x^5 +poly_model12$coefficient[5]*x^4 +poly_model12$coefficient[4]*x^3 +poly_model12$coefficient[3]*x^2 + poly_model12$coefficient[2]*x + poly_model12$coefficient[1]
  }
  curve(train_pol, col="red", type = "l", lwd=1)
  points(V1, V2, type="p", lwd=1)
}
poly_train(train=train_set)
# ================================================================================================================================


#Using degree=12 on test set
# ================================================================================================================================

poly_test = function(test){
  attach(test)
  test_poly = lm(V2~(V1+I(V1^2)+I(V1^3)+I(V1^4)+I(V1^5)+I(V1^6)+I(V1^7)+I(V1^8)+I(V1^9)+I(V1^10)+I(V1^11)+I(V1^12)))
  plot(test_poly)
  summary(test_poly)
  plot(V1, V2, type="p", lwd=1)
  points(V1, predict(test_poly), type="l", col="red", lwd=2)
  test_pol = function(x){
    poly_model12$coefficient[13]*x^12 + poly_model12$coefficient[12]*x^11 + poly_model12$coefficient[11]*x^10 + poly_model12$coefficient[10]*x^9 + poly_model12$coefficient[9]*x^8 + poly_model12$coefficient[8]*x^7 +poly_model12$coefficient[7]*x^6 +poly_model12$coefficient[6]*x^5 +poly_model12$coefficient[5]*x^4 +poly_model12$coefficient[4]*x^3 +poly_model12$coefficient[3]*x^2 + poly_model12$coefficient[2]*x + poly_model12$coefficient[1]
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
