#SOM script for data exploration.
#This script outputs heatplots for each variable in the 'yarn_data' dataset and saves them to disk.
#To run please ensure the following:
##1. The 'data' dataset is saved in the working directory.
##2. The filepath to save the heatplots to is as desired (row 59)

library(kohonen)
library(RColorBrewer)
library(colorRamps)
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


yarn_data<-na.omit(yarn_data)
yarn_data<-(filter(yarn_data,rating_average>0.1))
yarn_data<-scale(yarn_data)

#start<-proc.time()

som_grid <- somgrid(xdim = 40, ydim=40, topo="hexagonal")


set.seed(57)
yarn_data.som <- som(yarn_data, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)

plot(yarn_data.som, main = "Yarn Data",type="count")



#create heatplots for each attribute
for ( i in (1:length(dimnames(yarn_data)[[2]]))){
  png(paste0("SOM plots/",dimnames(yarn_data)[[2]][i],".png"))
plot(yarn_data.som, type = "property", property = yarn_data.som$codes[[1]][,i],
  palette.name = colorRamps::blue2red,ann=FALSE,yaxt='n',xaxt='n',main="")
title(main=dimnames(yarn_data)[[2]][i],cex.main=2)
dev.off()
}

#plot clusters
som_cluster <- cutree(hclust(dist(yarn_data.som$codes[[1]])), 10)
pretty_palette <- c("red", 'blue', 'green', 'yellow', 'purple', 'orange','black','grey','pink','white')
png("clusters.png")
plot(yarn_data.som, bgcol = pretty_palette[som_cluster], main = "Clusters",cex.main=6)
add.cluster.boundaries(yarn_data.som, som_cluster)
dev.off()