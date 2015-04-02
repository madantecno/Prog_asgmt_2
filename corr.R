corr<- function(directory,threshold=0){
        
        path=paste("Practice Code/Week 2 asginment R Prog/",directory,sep="")        
        data = multmerge(path)
        required_data=na.omit(data)
        result<-as.data.frame(table(required_data$ID))
        colnames(result)<-c("ID","nobs")
        
        id_above_threshold<-result[result$nobs>threshold,]
        i<-1
        corval<-data.frame()
        while(i <= 332){
                if(i %in% id_above_threshold$ID){
                        data_to_be_cor<-required_data[required_data$ID==i,]
                
                corval<-rbind(corval,cor(data_to_be_cor$sulfate,data_to_be_cor$nitrate))
        }
        i<-i+1
        }
        return(corval)
}


multmerge = function(mypath){
        filenames<-list.files(path=mypath, full.names=TRUE)
        datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
        Reduce(function(x,y) {x<-rbind(x,y)}, datalist)
}
