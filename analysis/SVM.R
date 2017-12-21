#Ensure working directory is that in which 'data' file is stored
#load in dataset
####################################################### USE THIS SECTION IF ALL DATA TO BE USED
#load in dataset
load("data.RData")
yarn_data<-processed_dataset
rm("processed_dataset")

####################################################### USE THIS SECTION IF ONLY A SAMPLE OF THE DATA IS TO BE USED
#load("data.RData")
#samples<-sample(c(1:nrow(processed_dataset)),20000)
#yarn_data<-processed_dataset[samples,]
#rm("processed_dataset")
#######################################################

library(e1071)
library(Metrics)
library(parallelSVM)
start<-proc.time()

set.seed(57)
#generate training and testing sets
#test set is 20% of data
test_indices<-sample(nrow(yarn_data),floor(0.2*nrow(yarn_data)),replace=FALSE)
test<-yarn_data[test_indices,]
train<-yarn_data[-test_indices,]

set.seed(57)
#fit svm model to training data
#svmfit=svm(rating_average~., data=train , kernel ="radial", cost=1000000,gamma=0.00000001, scale=TRUE)
svmfit=svm(rating_total~., data=train , kernel ="radial", cost=10000000,gamma=0.00001, scale=TRUE)
#svmfit=svm(rating_total~synthetic, data=train , kernel ="radial", cost=100000,gamma=0.1, scale=TRUE)
#view support vectors
#svmfit$index
#plot(svmfit,train)

#predict - omits rows with NAS
predicted<-predict(svmfit,test[!rowSums(is.na(test)),])


#omit NAs from test data - note this halves the size. MAY CHANGE LATER
test<-test[!rowSums(is.na(test)),]

#plot actuals and predicted
plot(c(1:length(predicted)),test$rating_average,pch="o",col="black")
points(c(1:length(predicted)),predicted,pch="o",col="red")

#calculate mean RMS error
RMSerror<-rmse(test$rating_total,predicted)
RMSerror

proc.time()-start