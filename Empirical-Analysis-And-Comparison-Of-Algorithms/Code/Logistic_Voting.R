# Voting Dataset
#============================================================================
data = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data",sep=",")
ds = data[,-1]
y = data[,1]
ds = cbind(ds, y)
#================================== 

ds_x = as.matrix(ds[,-(length(ds))])
#================================== 

# require ("ggplot2")
# ggplot (ds, aes (x = V11, y = V22, colour = as.factor(V5))) + stat_density2d (h=0.1)
#================================== 

ds_x = ifelse(ds_x == 'y', 1, 0)
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
y_generator(y_label = "republican")
# print(ds_y)
#================================== 

# theta_list = as.matrix(rbind(seq(0.1,0.1,length=1), seq(0.1,0.1,length=1), seq(0.1,0.1,length=1), seq(0.1,0.1,length=1), seq(0.1,0.1,length=1)))
# theta_list
#================================== 

# log_reg = function(ds_y, n, iter){
#   theta_list = t(matrix(rexp(length(ds), rate=.01), ncol=length(ds))/1000)
#   theta_list
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

for(x in 1:2000){
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
# }

# log_reg(ds_y = ds_y, n = 0.1, iter = 10)
# y_generator(y_label = "Iris-versicolor")
# log_reg(ds_y = ds_y, n = 0.1, iter = 505)
# print(y_pred)
mse = function(){
sample <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data",sep=",")

op_mean_list = list()
length_list = list()
for(i in 40:100){
  
  size1 = i/100
  
  
  t.idx<-sample(1:435, size = size1*435)
  sample.tr<-sample[t.idx,]
  sample.te<-sample[-t.idx,]
  
  logit1<-glm(V1~., sample.tr, family=binomial())
  
  #   logit1<-glm(V1~., data=sample.tr[,!colnames(sample.tr) %in% c(sample[1])], family=binomial())
  
  #   mod2 <- glm(z~., data=train[,!colnames(train) %in% c("y")], family="binomial")
  logit1
  summary(logit1)
  
  prob<<-predict(logit1, sample.te, type="response")
  length = length(sample.tr$V1)
  length_list = c(length_list, length)
  pred_class = (matrix(c(ifelse(prob>0.5, "republican", "democrat"))))
  
  table(Class=sample.te$V1, pred=pred_class)
  op_mean = mean(sample.te$V1 != pred_class)
  op_mean_list = c(op_mean_list, op_mean)
}
x = as.numeric(op_mean_list)
y = as.numeric(length_list)
smoothingSpline = smooth.spline(y, x, spar=0.35, tol = 0.0001)
plot(y,x, pch=".", col="blue", main = "Logistic Regression Error \n Voting Records", xlab = "Training data length", ylab = "Mean Error")
lines(smoothingSpline, col = 'red', lwd = 2)
}
# ================================================================================================================================
#Function for performance evaluation
perf_eval = function(y_hat, y_real){
  # Confusion matrix 
  conf_matrix = table(y_hat, y_real)
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


# y_generator(y_label = "R")
# print(ds_y)
# log_reg(ds_y = ds_y, n = 0.1, iter = 400)
# print(theta_list)
# print(y_hat_list)
# print(y_pred)
perf_eval(y_hat=y_pred, y_real=ds_y)

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
mse()
st = system.time(replicate(5, mse()))
st
rm(list=ls())

