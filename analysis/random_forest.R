#TO PARALLELIZE - JUST PARALLELISE THE CROSS VALIDATION
#AND MAYBE A TUNEGRID (AT PRESENT JUST USING HOLDOUT)
#~4HRS TO RUN SINGLE RANDOM FOREST ON SINGLE CORE

library( randomForest)
library(caret)
library(base)
library(Metrics)


#load in dataset
load("data.RData")
yarn_data<-processed_dataset
rm("processed_dataset")


start<-proc.time()
#create formula for svm
yarn_data<-yarn_data[,c(2,1,c(3:ncol(yarn_data)))]
form<-formula(yarn_data)


#create folds for CV
set.seed(57)
yarn_data$fold <- caret::createFolds(1:nrow(yarn_data), k = 5, list = FALSE) #try with 10 folds


#create training and testing set for holdout validation
training <- yarn_data[yarn_data$fold != 1, ]
test<-yarn_data[yarn_data$fold == 1, ]

#create RF model. Note, number of attributes is 1/3 of total attributes for each tree
rf.model= randomForest(form ,data=training ,
                        mtry=floor(ncol(yarn_data))/3, importance =TRUE, na.action=na.omit)

#try the RF model on the testing data
test<-test[!rowSums(is.na(test)),]
rf.pred = predict(rf.model ,newdata=test)
rmse(rf.pred,test$rating_total)

proc.time()-start