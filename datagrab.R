#this is similar to the datagram_working file. Run this first to get a list of valid yarn ids


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
dataset<-data.frame(matrix(nrow=1,ncol=21))
for(i in(1:21)){
  attrs<-c("wpi","rating_total","permalink","organic","max_gauge","gauge_divisor","max_needle_size","rating_average","min_hook_size",
           "max_hook_size","min_gauge","discontinued","rating_count","machine_washable","grams","thread_size","name","notes_html","yardage","id","texture")
  colnames(dataset)[i]<-attrs[i]
}

#yarn_weight
dataset2<-data.frame(matrix(nrow=1,ncol=8))
for(i in(1:8)){
  attrs2<-c("knit_gauge","crochet_gauge","wpi","id","ply","max_gauge","min_gauge","name")
  colnames(dataset2)[i]<-attrs2[i]
}

#yarn company
dataset3<-data.frame(matrix(nrow=1,ncol=4))
for(i in(1:4)){
  attrs3<-c("id","yarns_count","permalink","name" )
  colnames(dataset3)[i]<-attrs3[i]
}

#YARN_FIBRE ATTRIBUTE HAS VARIABLE HEADINGS -WILL NEED TO SAVE EACH ENTRY AS PLAIN TEXT



all_attrs<-c("wpi","rating_total","permalink","organic","max_gauge","gauge_divisor","max_needle_size","rating_average","min_hook_size",
         "max_hook_size","min_gauge","discontinued","rating_count","machine_washable","grams","thread_size","name","notes_html","yardage","id","texture",
         "knit_gauge","crochet_gauge","wpi","id","ply","max_gauge","min_gauge","name","id","yarns_count","permalink","name")

yarn_matrix<-matrix(ncol=21)
yarn_matrix2<-matrix(ncol=8)
yarn_matrix3<-matrix(ncol=4)
fibers<-list()

for (k in (1:200000)){
  print(k)
  tryCatch({
  single_yarn<-GET(paste0("https://api.ravelry.com/yarns/",k,".json"), config=config("token"=ravelry.token))
  single_yarn_data<-content(single_yarn)
 
  yarn_row<-list()
  yarn_row2<-list()
  yarn_row3<-list()

  for( j in(1:21)){
    attr<-colnames(dataset[j])
    yarn_row[j]<-single_yarn_data$yarn[attr]
  }
  
  for( j in(22:29)){
    attr<-colnames(dataset2[j-21])
    yarn_row2[j-21]<-single_yarn_data$yarn$yarn_weight[attr]
  }
  for( j in(30:33)){
    attr<-colnames(dataset3[j-29])
    yarn_row3[j-29]<-single_yarn_data$yarn$yarn_company[attr]
  }

  
  #store fibers seperately due to variable headers
  fibers[k]<-as.character(single_yarn_data$yarn$yarn_fibers)
  
  #add row to yarn matrices
  yarn_row<-matrix(yarn_row,nrow=1)
  yarn_row2<-matrix(yarn_row2,nrow=1)
  yarn_row3<-matrix(yarn_row3,nrow=1)
  yarn_matrix<-rbind(yarn_row,yarn_matrix)
  yarn_matrix2<-rbind(yarn_row2,yarn_matrix2)
  yarn_matrix3<-rbind(yarn_row3,yarn_matrix3)}, error=function(e){})
}

dataset<-as.data.frame(yarn_matrix,colnames=attrs)
dataset2<-as.data.frame(yarn_matrix2,colnames=attrs2)
dataset3<-as.data.frame(yarn_matrix3,colnames=attrs3)

#these are our ouput data sets -we can cbind these together, along with the fiber data
names(dataset)<-attrs
names(dataset2)<-attrs2
names(dataset3)<-attrs3

#uncomment to write the datasets to disk
#dataset<-as.matrix(dataset)
#dataset2<-as.matrix(dataset2)
#dataset3<-as.matrix(dataset3)
#write.table(dataset,"dataset.csv",sep=",")
#write.table(dataset2,"dataset2.csv",sep=",")
#write.table(dataset3,"dataset3.csv",sep=",")
