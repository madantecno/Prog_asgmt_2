complete <- function(directory, id = 1:332) {

         path=paste("Practice Code/Week 2 asginment R Prog/",directory,sep="")        
         data = multmerge(path)
         required_data=na.omit(data)
         result<-as.data.frame(table(required_data$ID))
         colnames(result)<-c("ID","nobs")
         new_result<-result[result$ID %in% id,]
         return(new_result)           
}

multmerge = function(mypath){
        filenames<-list.files(path=mypath, full.names=TRUE)
        datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
        Reduce(function(x,y) {x<-rbind(x,y)}, datalist)
}
