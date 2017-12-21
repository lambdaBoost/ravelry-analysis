#Dimensionality reduction and Principal Component Regression.
#This script takes the dataset produced by the 'datagrab' file and reduces the number of attributes.
#PCR is also performed at the end to search for important variables.
#To run please ensure the following:
##1. The 'processed_dataset' file - which is the output o the 'datagrab' script, must be saved in the working directory.
##2. To automatically save the output of this scripe, please uncomment lines 117 and 118

library(pls)

#load in raw dataset
load("processed_dataset.RData")

#convert list columns to vectors 
attrs<-names(processed_dataset)

#first 17 columns are lists in original dataset
for( j in (1:17)){

#replace nulls with zero (null is equal to zero in the raw database)
attr_vector<-c(processed_dataset[,j])
for (i in(1:length(attr_vector))){
  if(is.null(attr_vector[i][[1]])==TRUE){
    attr_vector[i]<-0}
  else{attr_vector[i]<-attr_vector[i]}
  }

attr_vector<-unlist(attr_vector)
processed_dataset[,j]<-attr_vector
}

#================================================================================================
#Remove attributes with no info, or just text

#remove hook size attributes - hook size is completely dependant on needle size so gives no new info
drops<-grep('hook',attrs,TRUE)
processed_dataset<-processed_dataset[,-drops]

#remove attributes which are unlikely to carry any useful information (text attributes)
drops<-c("permalink","name","company_name","company_permalink")
processed_dataset<-processed_dataset[ , !(names(processed_dataset) %in% drops)]


#remove 'texture' and 'needle name' attributes. The former is text based. The latter are dependant on needle size
drops<-c("texture","max_needle_name","min_needle_name")
processed_dataset<-processed_dataset[ , !(names(processed_dataset) %in% drops)]

#yarn weight names could be converted to dummy variables, but this is pointless since it is dependant on ply - remove
drops<-c("yarn_weight_name")
processed_dataset<-processed_dataset[ , !(names(processed_dataset) %in% drops)]

#now delete all 'id' attributes - these contain no information but keep 'id' since this gives indication of date added
drops<-c("company_id","min_needle_id","max_needle_id","yarn_weight_id")
processed_dataset<-processed_dataset[ , !(names(processed_dataset) %in% drops)]

#remove average rating and rating count since the relationship between these and overall rating is trivial
drops<-c("rating_total","rating_count") ##FLAG - CHANGED TO RATING TOTAL FROM RATING AVERAGE
processed_dataset<-processed_dataset[ , !(names(processed_dataset) %in% drops)]

#most needle size attributes can be removed since they are just variatins of the same measurement (metric, imperial etc)
drops<-c("max_needle_pretty_metric","max_needle_crochet","max_needle_us","max_needle_us_steel",
         "max_needle_knitting","min_needle_pretty_metric","min_needle_crochet","min_needle_us","min_needle_us_steel",
         "min_needle_knitting","yarn_weight_min_gauge","yarn_weight_wpi","yarn_weight_max_gauge",
         "yarn_weight_knit_gauge","yarn_weight_crochet_gauge")
processed_dataset<-processed_dataset[ , !(names(processed_dataset) %in% drops)]


#convert allattributes to
processed_dataset <- as.data.frame(sapply(processed_dataset, as.numeric))
#=================================================================================================
#Make new dependant attributes for fiber types (plant, animal or synthetic)

#create new columns for the new attributes
vegetable<-rep(0,nrow(processed_dataset))
animal<-rep(0,nrow(processed_dataset))
synthetic<-rep(0,nrow(processed_dataset))

#list attribute names
vegetable_names<-c("Cotton", "Linen", "Bamboo","Soy","Plant fiber","Hemp","Tencel")
animal_names<-c("Wool","Mohair","Merino","Silk","Angora","Cashmere","Alpaca","Llama",
                "Camel","Bison","Yak","Qiviut")
synthetic_names<-c("Nylon","Rayon","Acryllic","Polyester","Metallic","Microfiber")


processed_dataset<-cbind(processed_dataset,vegetable,animal,synthetic)

#create totals (note no NAs since these were previously replaced with zeros)
processed_dataset$vegetable<-rowSums(processed_dataset[,names(processed_dataset) %in% vegetable_names])
processed_dataset$animal<-rowSums(processed_dataset[,names(processed_dataset) %in% animal_names])
processed_dataset$synthetic<-rowSums(processed_dataset[,names(processed_dataset) %in% synthetic_names])

#note, the 'other' attribute is for fibers of unknown type. May wish to consider this similarly to the above atributes
#=================================================================================================
#We are now left with 44 attributes for analysis

#remove thread size attribute as it is constant (no info and doesn't allow scaling)
#remove average rating and rating count since the relationship between these and overall rating is trivial
drops<-c("thread_size")
processed_dataset<-processed_dataset[ , !(names(processed_dataset) %in% drops)]

#rename column 34 to remove odd formatting
colnames(processed_dataset)[34] <- "Plant_fiber"

#standardise the data prior to any model fitting (optional - doesn't seem to make any difference in practice)
#processed_dataset<-scale(processed_dataset)


#perform PCR
set.seed (57)

pcr_model <- pcr(rating_average~., data = na.omit(processed_dataset), scale = TRUE, validation = "CV")
#validation plot for the pcr model with cross-validation MSE plotted
validationplot(pcr_model,val.type="RMSE")

#we see that the lowest MSE occurs when alomst all variables are used

#save workspace
#rm(list=setdiff(ls(), "processed_dataset"))
#save.image("data.R")
