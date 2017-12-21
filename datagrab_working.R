#run this using the ids obtained from the datagrab code to get raw data for all valid yarns
#this data must then be processed using the 'process_data' script. Ensure to save an image of the global environment at teh end to save the data
#read ids file. This is a list of valid ids, obtained from the previous datagrab
ids<-read.csv("ids.csv")
ids<-as.character(ids[,1])



#max = 113337 min=101
yarn<-GET("https://api.ravelry.com/yarns/5000.json", config=config("token"=ravelry.token))
yarn_data<-content(yarn)

names1<-names(yarn_data$yarn)
names2<-names(yarn_data$yarn$yarn_weight)
names3<-names(yarn_data$yarn$yarn_company)
names4<-names(yarn_data$yarn$yarn_fibers)
names5<-names(yarn_data$yarn$min_needle_size)
all_names<-c(names1,names2,names3,names4,names5)

#make data frames
dataset<-data.frame(matrix(nrow=1,ncol=25))
for(i in(1:25)){
  attrs<-c("wpi","rating_total","permalink","organic","max_gauge","gauge_divisor","max_needle_size","rating_average","min_hook_size",
           "max_hook_size","min_gauge","discontinued","rating_count","machine_washable","grams","thread_size","name","notes_html","yardage","id","texture","yarn_fibers","yarn_company","yarn_weight","min_needle_size")
  colnames(dataset)[i]<-attrs[i]
}


all_attrs<-c("wpi","rating_total","permalink","organic","max_gauge","gauge_divisor","max_needle_size","rating_average","min_hook_size",
         "max_hook_size","min_gauge","discontinued","rating_count","machine_washable","grams","thread_size","name","notes_html","yardage","id","texture","yarn_fibers","yarn_company","yarn_weight","min_needle_size")

yarn_matrix<-matrix(ncol=25)

fibers<-list()

for (k in (1:length(ids))){
  print(k)
  tryCatch({
  single_yarn<-GET(paste0("https://api.ravelry.com/yarns/",ids[k],".json"), config=config("token"=ravelry.token))
  single_yarn_data<-content(single_yarn)
 
  yarn_row<-list()


  for( j in(1:25)){
    attr<-colnames(dataset[j])
    yarn_row[j]<-single_yarn_data$yarn[attr]
  }
  


  
  #add row to yarn matrices
  yarn_row<-matrix(yarn_row,nrow=1)

  yarn_matrix<-rbind(yarn_row,yarn_matrix)
}, error=function(e){})
}

dataset<-as.data.frame(yarn_matrix)
names(dataset)<-all_attrs


#uncomment to write the datasets to disk
#dataset<-as.matrix(dataset)
#note - the yarn_company, max_needle_size, min_hook_size, max_hook_size,min_needle_size, yarn_fibers, yarn weight
#attributes are in lists and do not save properly to csv (due to commas in the records). Probably best to save these seperately or something.
#write.table(dataset,"dataset.csv",sep=",")

