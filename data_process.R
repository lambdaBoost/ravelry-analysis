#load in image from datagrab script. This is called 'yarn_data'

attach(dataset)

#process max needle size column into seperate columns
#get attribute names

max_needle_names<-names(max_needle_size[[1]])

#create max needle size dataframe
max_needle_size_data<-data.frame(matrix(ncol=length(max_needle_names)))

colnames(max_needle_size_data)<-max_needle_names

#populate the dataframe
for (i in(1:length(max_needle_size))){
  for(n in (1:length(max_needle_names))){
    attr<-max_needle_names[n]
    if(attr %in% names(max_needle_size[[i]])){
    max_needle_size_data[i,attr]<-as.character(max_needle_size[[i]][attr])}
    else{ max_needle_size_data[i,attr]==NULL}

  }
  print(i)
}







#process min hook size column into seperate columns
#get attribute names
min_hook_names<-names(min_hook_size[[3]])

#create max needle size dataframe
min_hook_size_data<-data.frame(matrix(ncol=length(min_hook_names)))

colnames(min_hook_size_data)<-min_hook_names

#populate the dataframe
for (i in(1:length(min_hook_size))){
  for(n in (1:length(min_hook_names))){
    attr<-min_hook_names[n]
    if(attr %in% names(min_hook_size[[i]])){
      min_hook_size_data[i,attr]<-as.character(min_hook_size[[i]][attr])}
    else{ min_hook_size_data[i,attr]==NULL}
    
  }
  print(i)
}






#process max hook size column into seperate columns
#get attribute names
max_hook_names<-names(max_hook_size[[3]])

#create max needle size dataframe
max_hook_size_data<-data.frame(matrix(ncol=length(max_hook_names)))

colnames(max_hook_size_data)<-max_hook_names

#populate the dataframe
for (i in(1:length(max_hook_size))){
  for(n in (1:length(max_hook_names))){
    attr<-max_hook_names[n]
    if(attr %in% names(max_hook_size[[i]])){
      max_hook_size_data[i,attr]<-as.character(max_hook_size[[i]][attr])}
    else{ max_hook_size_data[i,attr]==NULL}
    
  }
  print(i)
}







#process yarn company column into seperate columns
#get attribute names
yarn_company_names<-names(yarn_company[[3]])

#create yarn company dataframe
yarn_company_data<-data.frame(matrix(ncol=length(yarn_company_names)))

colnames(yarn_company_data)<-yarn_company_names

#populate the dataframe
for (i in(1:length(yarn_company))){
  for(n in (1:length(yarn_company_names))){
    attr<-yarn_company_names[n]
    if(attr %in% names(yarn_company[[i]])){
      yarn_company_data[i,attr]<-as.character(yarn_company[[i]][attr])}
    else{ yarn_company_data[i,attr]==NULL}
    
  }
  print(i)
}




#process yarn weight column into seperate columns
#get attribute names
yarn_weight_names<-names(yarn_weight[[3]])

#create yarn company dataframe
yarn_weight_data<-data.frame(matrix(ncol=length(yarn_weight_names)))

colnames(yarn_weight_data)<-yarn_weight_names

#populate the dataframe
for (i in(1:length(yarn_weight))){
  for(n in (1:length(yarn_weight_names))){
    attr<-yarn_weight_names[n]
    if(attr %in% names(yarn_weight[[i]])){
      yarn_weight_data[i,attr]<-as.character(yarn_weight[[i]][attr])}
    else{ yarn_weight_data[i,attr]==NULL}
    
  }
  print(i)
}





#process max needle size column into seperate columns
#get attribute names
min_needle_names<-names(min_needle_size[[1]])

#create max needle size dataframe
min_needle_size_data<-data.frame(matrix(ncol=length(min_needle_names)))

colnames(min_needle_size_data)<-min_needle_names

#populate the dataframe
for (i in(1:length(min_needle_size))){
for(n in (1:length(min_needle_names))){
attr<-min_needle_names[n]
if(attr %in% names(min_needle_size[[i]])){
min_needle_size_data[i,attr]<-as.character(min_needle_size[[i]][attr])}
else{ min_needle_size_data[i,attr]==NULL}

}
print(i)
}


#process yarn_fibres to get dummy variables

#establist list of yarn names
fiber_names<-list()

#for each yarn, get the names of fibres present
for (i in(1:length(yarn_fibers))){  
  yarn_fiber_indices<-which(names(unlist(yarn_fibers[[i]]))=="fiber_type.name")
  yarn_fiber_row<-unlist(yarn_fibers[[i]])[yarn_fiber_indices]
  fiber_names<-c(fiber_names,yarn_fiber_row)
  print(i)
}
fiber_names<-unique(fiber_names)

#make dataframe for yarn fibers
yarn_fibers_data<-data.frame(matrix(rep( 0, len=length(fiber_names)*length(yarn_fibers)), nrow = length(yarn_fibers)))
colnames(yarn_fibers_data)<-fiber_names


#next go through and populate - do NOT unlist each row this time - can match up the individual list attributes
for (n in (1:length(yarn_fibers))){ #    
  if (length(yarn_fibers[[n]])>0) {
  for (m in (1:length(yarn_fibers[[n]]))){
    if(is.numeric(yarn_fibers[[n]][[m]]$percentage)){
    yarn_fibers_data[n,yarn_fibers[[n]][[m]]$fiber_type$name]<-yarn_fibers[[n]][[m]]$percentage
    }
    else{yarn_fibers_data[n,yarn_fibers[[n]][[m]]$fiber_type$name]<-0}
  }
  }
  else {
    yarn_fibers_data[n,]<-rep(0,26)
  }
  print(n)
 }



#RUN ALL OR NOTHING FROM THIS POINT BELOW!!
#delete the processed attributes from the dataset
drops<-c("max_needle_size","min_hook_size","max_hook_size","notes_html","yarn_fibers","yarn_company","yarn_weight","min_needle_size")
processed_dataset<-dataset[ , !(names(dataset) %in% drops)]

#delete last 2 rows (NAS present in these rows prevent proper data handling)
processed_dataset<-processed_dataset[1:109556,]

#change columns names for processed attributes
colnames(max_hook_size_data)<-c("max_hook_hook","max_hook_metric","max_hook_pretty_metric","max_hook_us_steel","max_hook_knitting","max_hook_name",         
"max_hook_crochet","max_hook_us","max_hook_id")
colnames(min_hook_size_data)<-c("min_hook_hook","min_hook_metric","min_hook_pretty_metric","min_hook_us_steel","min_hook_knitting","min_hook_name",         
                                "min_hook_crochet","min_hook_us","min_hook_id")
colnames(min_needle_size_data)<-c("min_needle_hook","min_needle_pretty_metric","min_needle_crochet","min_needle_us","min_needle_us_steel","min_needle_name","min_needle_knitting","min_needle_id","min_needle_metric"  )
colnames(max_needle_size_data)<-c("max_needle_hook","max_needle_pretty_metric","max_needle_crochet","max_needle_us","max_needle_us_steel","max_needle_name","max_needle_knitting","max_needle_id","max_needle_metric"  )
colnames(yarn_weight_data)<-c("yarn_weight_min_gauge","yarn_weight_wpi","yarn_weight_max_gauge","yarn_weight_knit_gauge","yarn_weight_name","yarn_weight_crochet_gauge",
                              "yarn_weight_ply","yarn_weight_id" )
colnames(yarn_company_data)<-c("company_name","company_yarns_count","company_permalink","company_id")

#insert the processed attributes to the overall dataset
processed_dataset<-cbind(processed_dataset,max_hook_size_data)
processed_dataset<-cbind(processed_dataset,min_hook_size_data)
processed_dataset<-cbind(processed_dataset,max_needle_size_data)
min_needle_size_data<-min_needle_size_data[1:109556,]
processed_dataset<-cbind(processed_dataset,min_needle_size_data)
yarn_weight_data<-yarn_weight_data[1:109556,]
processed_dataset<-cbind(processed_dataset,yarn_weight_data)
yarn_fibers_data<-yarn_fibers_data[1:109556,]
processed_dataset<-cbind(processed_dataset,yarn_fibers_data)
yarn_company_data<-yarn_company_data[1:109556,]
processed_dataset<-cbind(processed_dataset,yarn_company_data)

#write to disk
write.csv(as.character(processed_dataset),"processed_dataset.csv")
save.image("~/R/ravelry/data/processed_dataset.RData")

#now need to absolutely check rows are in sync and change to appropriate datatype