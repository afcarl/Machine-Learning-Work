# Wisconsin Breast Cancer Dataset
#============================================================================
ds = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data",sep=",")
ds$V7 = NULL
ds$V1 = NULL
#================================== 
ds_x = as.matrix(ds[,-(length(ds))])
#================================== 

# require ("ggplot2")
# ggplot (ds, aes (x = V11, y = V22, colour = as.factor(V5))) + stat_density2d (h=0.1)
#================================== 

x0 = as.matrix(seq(1,1,length = length(ds_x[,1])))
ds_x = as.matrix(cbind(x0, ds_x))
#================================== 


y_generator = function(y_label){
  ds_y <<- data.matrix(integer(0))
  for(i in 1:length(ds$V5)){
    if(ds[i,length(ds)] == y_label){
      ds_y <<- rbind(ds_y, 1)
    }
    else{
      ds_y <<- rbind(ds_y, 0)
    }
  }
}
y_generator(y_label = 2)
#================================== 
# Formulating the Logistic Regression Algorithm
#---------------------------------------------------
theta_list = (as.matrix(rep(0.1, length(ds))))
theta_list

y_hat_list = data.matrix(numeric(0))
y_hat = 0

# Initial y prediction 
for(i in 1:length(ds_y)){
  y_hat = 1/1+(exp(-(ds_x[i,]%*%((theta_list)))))
  y_hat_list = rbind(y_hat_list, y_hat)  
}            

#Gradient descent and optimization

temp = data.matrix(numeric(0))
temp_list = matrix(numeric(0),nrow=length(ds))
y_temp = 0
y_temp_list = data.matrix(numeric(0))
n = 0.1

for(x in 1:1000){
  temp = t(theta_list) + (1/length(ds_y))*(n * (t(y_hat_list - ds_y) %*% ds_x)) 
  theta_list = t(as.matrix(temp))
  
  y_temp = 0
  y_temp_list = data.matrix(numeric(0))
  for(i in 1:length(ds_y)){
    y_temp =  ((1/length(ds_y))*(1/1+(exp(-(ds_x[i,]%*%((theta_list)))))))
    #       y_temp <<-  (((1/1+(exp(-(ds_x[i,]%*%((theta_list))))))))
    y_temp_list = rbind(y_temp_list, y_temp)        
  }
  y_hat_list = y_temp_list
}

#Decision boundary
y_pred = data.matrix(numeric(0))
for(i in 1:length(y_hat_list)){
  if(y_hat_list[i]>=0.5){
    y_pred = rbind(y_pred, 1)
  }
  else{
    y_pred = rbind(y_pred, 0)
  }
}
mse = function(){
  sample <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data",sep=",")
  y = (matrix(c(ifelse(sample$V11==2, 0, 1))))
  sample = cbind(sample[,-11], y )  
  op_mean_list = list()
  length_list = list()  
  for(i in 50:90){    
    size1 = i/100
    t.idx<-sample(1:699, size = size1 * 699)
    sample.tr<-sample[t.idx,]
    sample.te<-sample[-t.idx,]   
    logit1<-glm(sample.tr$y~., sample.tr, family=binomial())
    logit1
    summary(logit1)   
    prob<<-predict(logit1, sample.te, type="response")
    length = length(sample.tr$y)
    length_list = c(length_list, length)    
    pred_class = (matrix(c(ifelse(prob>0.5, 1, 0))))   
    table(Class=sample.te$y, pred=pred_class)
    op_mean = mean(sample.te$y != pred_class)
    op_mean_list = c(op_mean_list, op_mean)
  }
  x = as.numeric(op_mean_list)
  y = as.numeric(length_list)
  smoothingSpline = smooth.spline(y, x, spar=0.35, tol = 0.0001)
  plot(y,x, pch=".", col="blue", main = "Logistic Regression Error \n Breast Cancer", xlab = "Training data length", ylab = "Mean Error")
  lines(smoothingSpline, col = 'red', lwd = 2)
}

# ================================================================================================================================
#Function for performance evaluation
perf_eval = function(y_hat, y_real){
  # Confusion matrix 
  conf_matrix = table(y_hat, y_real, useNA = "always")
  print(conf_matrix)
  tp = conf_matrix[1,1]
  fp = conf_matrix[1,2]
  fn = conf_matrix[2,1]
  tn = conf_matrix[2,2]
  
  # accuracy
  acc <<- ((tp-fp)-(fn-tn))/length(y_real)
  # print(acc)
  
  # precision
  prec_1 <<- tp/(tp+fp)
  # print(prec_1)
  
  prec_2 <<- tn/(tn+fn)
  # print(prec_2)
  
  # recall
  rec_1 <<- tp/(tp+fn)
  # print(rec_1)
  
  rec_2 <<- tn/(tn+fp)
  # print(rec_2)
  
  # f-measure
  f_1 <<- 2*((prec_1*rec_1)/(prec_1+rec_1))
  # print(f_1)
  
  f_2 <<- 2*((prec_2*rec_2)/(prec_2+rec_2))
  # print(f_2)
}

# ================================================================================================================================
perf_eval(y_hat=y_pred, y_real=ds_y)
mse()
st = system.time(replicate(5, mse()))
st
eval_score = function(){
  cat("\nAccuracy: ",acc)
  cat("\nPrecision 1: ",prec_1)
  cat("\nPrecision 2: ",prec_2)
  cat("\nRecall 1: ",rec_1)
  cat("\nRecall 2: ",rec_2)
  cat("\nF1 score: ",f_1)
  cat("\nF2 score: ",f_2)
}
eval_score()
rm(list=ls())
