pollutantmean <- function(directory,pollutant, id = 1:332){
        
       
        path=paste("Practice Code/Week 2 asginment R Prog/",directory,sep="")
        
        data = multmerge(path)
        
        final_data <- subset(data, ID %in% id)
        if(pollutant=="sulfate"){
                required_mean<- mean(final_data$sulfate, na.rm = TRUE)
        }
        else{
                required_mean<- mean(final_data$nitrate, na.rm = TRUE)
        }
                print(required_mean)
}

multmerge = function(mypath){
        
        filenames<-list.files(path=mypath, full.names=TRUE)
        
        datalist = lapply(filenames, function(x){read.csv(file=x,header=T)})
        
        Reduce(function(x,y) {x<-rbind(x,y)}, datalist)
}
