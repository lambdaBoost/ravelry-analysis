#Script to fit SVM to the "yarn_data" dataset.
#To run this script please ensure the following:
##1. The "data" image is saved in the working directory.
##2. The relevant lines are uncommented between rows 9 and 17 to determine whether the entire dataset or just a sample is used.
##3. If the script has already been run and the best parameters known, uncomment lines 94 onwards to train and test the winning model and produce plots
#This script was run on EC2 and required almost 40 hours.


#load in dataset
#UNCOMMENT ONE OR THE OTHER OF THE FOLLOWING:
####################################################### USE THIS SECTION IF ALL DATA TO BE USED
#load in dataset
load("data.RData")
yarn_data<-processed_dataset
rm("processed_dataset")

####################################################### USE THIS SECTION IF ONLY A SAMPLE OF THE DATA IS TO BE USED
#load("data.RData")
#samples<-sample(c(1:nrow(processed_dataset)),5000)
#yarn_data<-processed_dataset[samples,]
#rm("processed_dataset")
#######################################################

yarn_data<-(filter(yarn_data,rating_average>0.1))

library(e1071)
library(Metrics)
library(doParallel)
library(caret)
library(base)
library(dplyr)



start<-proc.time()
set.seed(57)

#create compute cluster
cl<-makeCluster(detectCores())
registerDoParallel(cl)

#create formula for svm
yarn_data<-yarn_data[,c(5,1,2,3,4,c(6:ncol(yarn_data)))]
form<-formula(yarn_data)

#create stratified folds for CV
set.seed(57)
yarn_data$fold <- caret::createFolds(1:nrow(yarn_data), k = 5, list = FALSE) #try with 10 folds

#set parameter grid
cost <- c(1,1e2,1e3,1e7)
gamma <- c(1e-5,1e-3,1e-1,1)


PG <- expand.grid(cost = cost, gamma = gamma)

#go through parameter grid with for loop (runs in parallel through dopar functionality)
result <- foreach(i = 1:nrow(PG), .combine = rbind) %:% 
  foreach(j = 1:max(yarn_data$fold), .combine = rbind, .inorder = FALSE) %dopar% { 
    #need to state libraries within parallisation section
    require(foreach)
    require(Metrics)
    c <- PG[i, ]$cost
    g <- PG[i, ]$gamma
    
    #5 fold cv
    #make training and testing set for this fold 
    training <- yarn_data[yarn_data$fold != j, ]
    test<-yarn_data[yarn_data$fold == j, ]
    training <- training[!rowSums(is.na(training)),]
    test<-test[!rowSums(is.na(test)),]
    set.seed(57)
    #fit SVM model to this validation fold
    svm.fit <- e1071::svm(form, data = training, kernel = "radial", cost = c, gamma = g, na.action=na.omit,scale=TRUE)
    pred <- predict(svm.fit, test)
    out<-data.frame(actual = test$rating_average, predicted = pred)
    
    
    
    #determine performance for each variation of parameters
    #this is the evaluation criterion
    RMSerror<-rmse(out$actual,out$predicted)
    data.frame(PG[i, ], RMSE = RMSerror)
    
  }

#print results for all parameters - this is our output - the most succesful model here determines the parameters for the best model
for (i in seq(1,76,by=5)){print(mean(result$RMSE[seq(i,i+4,by=1)]))}

proc.time()-start

#UNCOMMENT FROM HERE BELOW TO PRODUCE PLOTS FOR THE BEST MODEL AS DETERMINED ABOVE
#to test the winning model
#set.seed(57)
#make a single training and testing set for plotting purposes
#training_ind<-sample(1:nrow(yarn_data),floor(0.8*nrow(yarn_data)))
#training<-yarn_data[training_ind,-45]
#testing<-yarn_data[-training_ind,-45]
#testing<-testing[!rowSums(is.na(testing)),]
#set.seed(57)
#fit SVM model to training set
#svm.model<- svm(form ,data=training ,cost=1, gamma=1e-5, importance =TRUE, na.action=na.omit)
#pred <- predict(svm.model, testing)
#results<-as.data.frame(cbind(testing$rating_average,pred))
#colnames(results)<-c("actual","predicted")
#results<-results[order(results$actual),]
#plot the results
#plot(1:nrow(results),results$predicted,pch=".",col="red",main="actual and predicted ratings for SVM",xlab="test_instance",ylab="rating_average",ylim=c(2,5.5))
#points(results$actual,pch=".")
#legend(6000,3.0,legend=c("predicted","actual"),pch=c(19,19),col=c("red","black"),cex=0.6)
#rmse(results$predicted,results$actual)
