#Ensure working directory is that in which 'data' file is stored
#load in dataset
load("rpi_data.RData")
yarn_data<-rpi_data
rm("processed_dataset")

library(e1071)
library(Metrics)
library(doParallel)
library(caret)
library(base)

start<-proc.time()
set.seed(57)

#create formula for svm
rpi_data<-rpi_data[,c(2,1,c(3:ncol(rpi_data)))]
form<-formula(rpi_data)

#create folds for CV
set.seed(57)
rpi_data$fold <- caret::createFolds(1:nrow(rpi_data), k = 5, list = FALSE) #try with 10 folds

#set parameter grid
cost <- c(0.01,0.1,1,10)
gamma <- c(0.001,0.01,0.1,1)
PG <- expand.grid(cost = cost, gamma = gamma)

#go through parameter grid with for loop - NOTE IF THIS DOESN'T WORK, JUST USE DOPAR ON THIS LOOP
result <- foreach(i = 1:nrow(PG), .combine = rbind) %:% { 
  #need to state libraries within parallisation section
  require(foreach)
  require(Metrics)
  c <- PG[i, ]$cost
  g <- PG[i, ]$gamma
  
  #10 fold cv
  out <- foreach(j = 1:max(rpi_data$fold), .combine = rbind, .inorder = FALSE) %dopar% { 
    training <- rpi_data[rpi_data$fold != j, ]
    test<-rpi_data[rpi_data$fold == j, ]
    training <- training[!rowSums(is.na(training)),]
    test<-test[!rowSums(is.na(test)),]
    svm.fit <- e1071::svm(form, data = training, type = "eps-regression", kernel = "radial", cost = c, gamma = g, na.action=na.omit,scale=FALSE)
    pred <- predict(svm.fit, test)
    data.frame(actual = test$rating_total, predicted = pred)
  }
  
  #determine performance for each variation of parameters
  #this is the evaluation criterion - use RMS (or similar)
  RMSerror<-rmse(out$actual,out$predicted)
  data.frame(PG[i, ], RMSE = RMSerror)
}
proc.time()-start