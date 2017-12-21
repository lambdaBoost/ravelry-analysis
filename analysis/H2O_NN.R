library(h2o)
library(caret)
library(base)
library(Metrics)
library(dplyr)

start<-proc.time()
set.seed(57)



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
#remove zero ratings
yarn_data<-(filter(yarn_data,rating_average>0.1))

#initialise compute cluster
#changed port to 50001 as not workig on default of 54321
h2o.init(nthreads=-1, ip = "127.0.0.1", port = 50001)
h2o.removeAll() 

#define training validation and testing data - comment out one of following whether standardisation required
#yarn_data<-as.h2o(scale(yarn_data),na.action=omit) #STANDARDISED
yarn_data<-as.h2o(yarn_data,na.action=omit) #NOT STANDARDISED
splits <- h2o.splitFrame(yarn_data, c(0.6,0.19,0.2), seed=57)
train  <- h2o.assign(splits[[1]], "train.hex") # 60%
valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%
test   <- h2o.assign(splits[[3]], "test.hex")  # 20%

#define dependant and independant variables
response <- "rating_average"
predictors <- setdiff(names(yarn_data), response)


#to get info on model go to localhost:50001 in browser and run getModel "dl_model_first"




#Now build models with varying parameters
#define hyper parameters
hyper_params <- list(
  hidden=list(c(32,32,32),c(64,64),c(64,48,32),c(43,43,22)),
  input_dropout_ratio=c(0,0.05,0.1),
  rate=c(0.001,0.01,0.02,0.05),
  rate_annealing=c(1e-8,1e-7,1e-6,1e-4),
  l1=c(1e-3,1e-5,1e-7),
  l2=c(1e-3,1e-5,1e-7)
)

#build models with parameter grid
set.seed(57)
grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id="dl_grid", 
  training_frame=yarn_data,
  #validation_frame=valid, 
  x=predictors, 
  y=response,
  epochs=10,
  stopping_metric="RMSE",         ##Use RMSE as stopping metric
  stopping_tolerance=1e-2,        ## stop when misclassification does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  adaptive_rate=F,                ## manually tuned learning rate
  momentum_start=0.5,             ## manually tuned momentum
  momentum_stable=0.9, 
  momentum_ramp=1e7, 
  #l1=1e-5,
  #l2=1e-5,
  activation=c("Rectifier"),           ##Used due to previous success
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params=hyper_params,
  variable_importances=T,          ##Allow to enable viewing later
  nfolds=5,
  fold_assignment="Modulo" 
)

#view best model
grid <- h2o.getGrid("dl_grid",sort_by="rmse",decreasing=FALSE)
grid@summary_table[1,]
best_model <- h2o.getModel(grid@model_ids[[1]])
best_model

print(best_model@allparameters)#note, the model name from here can be used in flow to view the model (via getModel command)
print(h2o.performance(best_model, valid=T))
print(h2o.rmse(best_model, valid=T))
head(as.data.frame(h2o.varimp(best_model)))

#to view the best model
pred <- as.vector(h2o.predict(best_model, test))
results<-as.data.frame(cbind(as.vector(test$rating_average),pred))
colnames(results)<-c("actual","predicted")
results<-results[order(results$actual),]
plot(1:nrow(results),results$predicted,pch=".",col="red",main="actual and predicted ratings for deep neural network",xlab="test_instance",ylab="rating_average",ylim=c(2,5))
points(results$actual,pch=".")
legend(6000,3.0,legend=c("predicted","actual"),pch=c(19,19),col=c("red","black"),cex=0.6)
rmse(results$predicted,results$actual)

#important variables
imps<-as.data.frame(h2o.varimp(best_model))
imps<-imps[,c(1,2)]
imps<-imps[order(imps$relative_importance,decreasing=T),]
imps<-data.frame(overall<-imps$relative_importance,names<-imps$variable)
par(mar=c(8,5,3,2))
barplot(imps$overall,names.arg=imps$names,las=2, xlab="",ylab="relative importance",main="variable importances for deep NN model",cex.names=0.7)
