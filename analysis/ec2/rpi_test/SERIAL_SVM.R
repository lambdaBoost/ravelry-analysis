#Ensure working directory is that in which 'data' file is stored
#load in dataset
#load in dataset
####################################################### USE THIS SECTION IF ALL DATA TO BE USED
#load in dataset
#load("data.RData")
#yarn_data<-processed_dataset
#rm("processed_dataset")

####################################################### USE THIS SECTION IF ONLY A SAMPLE OF THE DATA IS TO BE USED
load("data.RData")
samples<-sample(c(1:nrow(processed_dataset)),2000)
yarn_data<-processed_dataset[samples,]
rm("processed_dataset")
#######################################################

library(e1071)
library(Metrics)
library(doParallel)
library(caret)
library(base)

start<-proc.time()
set.seed(57)

#create formula for svm
yarn_data<-yarn_data[,c(2,1,c(3:ncol(yarn_data)))]
form<-formula(yarn_data)

#create folds for CV
set.seed(57)
yarn_data$fold <- caret::createFolds(1:nrow(yarn_data), k = 5, list = FALSE) #try with 10 folds

#set parameter grid
cost <- c(1,1e2,1e3,1e7)
#gamma <- c(1e-7,1e-5,1e-4,1)
#cost <- c(1e-3,1,100,1e5)
gamma <- c(1e-5,1e-3,1e-1,1)
PG <- expand.grid(cost = cost, gamma = gamma)

#go through parameter grid with for loop - NOTE IF THIS DOESN'T WORK, JUST USE DOPAR ON THIS LOOP
result <- foreach(i = 1:nrow(PG), .combine = rbind) %do% { #replace do with : to make parallel
  c <- PG[i, ]$cost
  g <- PG[i, ]$gamma
  
  #10 fold cv
  out <- foreach(j = 1:max(yarn_data$fold), .combine = rbind, .inorder = FALSE) %do% { #replace do with dopar to make parallel
    training <- yarn_data[yarn_data$fold != j, ]
    test<-yarn_data[yarn_data$fold == j, ]
    training <- training[!rowSums(is.na(training)),]
    test<-test[!rowSums(is.na(test)),]
    svm.fit <- e1071::svm(form, data = training, type = "eps-regression", kernel = "radial", cost = c, gamma = g, na.action=na.omit,scale=TRUE)
    pred <- predict(svm.fit, test)
    data.frame(actual = test$rating_total, predicted = pred)
  }
  
  #determine performance for each variation of parameters
  #this is the evaluation criterion - use RMS (or similar)
  RMSerror<-rmse(out$actual,out$predicted)
  data.frame(PG[i, ], RMSE = RMSerror)
}
proc.time()-start
