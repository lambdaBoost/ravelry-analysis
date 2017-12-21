#Script to fit random forest to the "yarn_data" dataset.
#To run this script please ensure the following:
##1. The "data" image is saved in the working directory.
##2. The relevant lines are uncommented between rows 28 and 36 to determine whether the entire dataset or just a sample is used.
##3. If the script has already been run and the best parameters known, uncomment lines 95 onwards to train and test the winning model and produce plots
#This script was run on EC2 and required 4 hours.



library( randomForest)
library(caret)
library(base)
library(Metrics)
library(doParallel)
library(foreach)
library(dplyr)

start<-proc.time()
set.seed(57)

#create compute cluster
cl<-makeCluster(detectCores())
registerDoParallel(cl)

#COMMENT OUT RELEVANT BELOW TO PROCESS WHOLE DATASET OR JUST A SAMPLE
#######################################################
#load in dataset
load("data.RData")
yarn_data<-processed_dataset
rm("processed_dataset")

####################################################### USE THIS SECTION IF ONLY A SAMPLE OF THE DATA IS TO BE USED
#load("data.RData")
#samples<-sample(c(1:nrow(processed_dataset)),10000)
#yarn_data<-processed_dataset[samples,]
#rm("processed_dataset")
#######################################################

yarn_data<-(filter(yarn_data,rating_average>0.1))
start<-proc.time()
#create formula for svm
yarn_data<-yarn_data[,c(5,1,2,3,4,c(6:ncol(yarn_data)))]
form<-formula(yarn_data)


#create folds for CV (5 fold)
set.seed(57)
yarn_data$fold <- caret::createFolds(1:nrow(yarn_data), k = 5, list = FALSE) 

#set parameter grid
mtry <- c(ncol(yarn_data)/3,10,20,40)
nodesize <- c(1,3,5,7)
PG <- expand.grid(mtry = mtry, nodesize = nodesize)


#go through parameter grid with for loop - NOTE IF THIS DOESN'T WORK, JUST USE DOPAR ON THIS LOOP
result <- foreach(i = 1:nrow(PG), .combine = rbind) %:% 
  foreach(j = 1:max(yarn_data$fold), .combine = rbind, .inorder = FALSE) %dopar% {
    #need to state libraries within parallisation section
    require(foreach)
    require(Metrics)
    require(randomForest)
    m <- PG[i, ]$mtry
    n <- PG[i, ]$nodesize
    
    #5 fold cv
    training <- yarn_data[yarn_data$fold != j, ]
    test<-yarn_data[yarn_data$fold == j, ]
    training <- training[!rowSums(is.na(training)),]
    test<-test[!rowSums(is.na(test)),]
    set.seed(57)
    rf.model<- randomForest(form ,data=training ,
                           mtry=m, nodesize=n, importance =TRUE, na.action=na.omit)
    
    pred <- predict(rf.model, test)
    out<-data.frame(actual = test$rating_average, predicted = pred)
    
    
    
    #determine performance for each variation of parameters
    #this is the evaluation criterion - use RMS (or similar)
    RMSerror<-rmse(out$actual,out$predicted)
    data.frame(PG[i, ], RMSE = RMSerror)
    
  }

#print results for tuned parameters
for (i in seq(1,76,by=5)){print(mean(result$RMSE[seq(i,i+4,by=1)]))}

proc.time()-start

#INPUT THE RESULTS OF THE ABOVE INTO LINE 99 AND UNCOMMENT BELOW TO PRODUCE PLOTS FROM WINNING MODEL

#to test the winning model
#training_ind<-sample(1:nrow(yarn_data),floor(0.8*nrow(yarn_data)))
#training<-yarn_data[training_ind,-45]
#testing<-yarn_data[-training_ind,-45]
#testing<-testing[!rowSums(is.na(testing)),]
#rf.model<- randomForest(form ,data=training ,mtry=10, nodesize=7, importance =TRUE, na.action=na.omit)
#pred <- predict(rf.model, testing)
#results<-as.data.frame(cbind(testing$rating_average,pred))
#colnames(results)<-c("actual","predicted")
#results<-results[order(results$actual),]
#plot(1:nrow(results),results$predicted,pch=".",col="red",main="actual and predicted ratings for random forest",xlab="test_instance",ylab="rating_average",ylim=c(2,5.5))
#points(results$actual,pch=".")
#legend(6000,3.0,legend=c("predicted","actual"),pch=c(19,19),col=c("red","black"),cex=0.6)
#rmse(results$predicted,results$actual)

#variable importance
#imps<-varImp(rf.model)
#imps<-imps[order(imps$Overall,decreasing=T),,drop=F]
#imps<-data.frame(overall<-imps$Overall,name<-row.names(imps))
#scale function
#scalef<-function(x){(x-min(x))/(max(x)-min(x))}
#imps$overall....imps.Overall<-scalef(imps$overall....imps.Overall)
#par(mar=c(8,5,3,2))
# barplot(imps$overall....imps.Overall,names.arg=imps$name....row.names.imps.,las=2, xlab="",ylab="relative importance",main="variable importances for random forest model",cex.names=0.7)

#make formula from important variables
#form<-rating_average ~  
#  +     min_gauge + grams + yardage + 
#  +     id + yarn_weight_ply + 
#  +     Acrylic + company_yarns_count + 
#  +     vegetable + animal + synthetic
