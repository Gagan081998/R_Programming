corr<-function(directory, threshold = 0){
  olddir<-getwd() #obtaining the current working directory
  newdir<-paste(olddir,"/",directory,sep="")  #new directory where the .csv files are stored
  setwd(newdir) #setting working directory to the new directory
  id<-1:332
  monitor<-as.character(id)
  cr<-vector(mode = "numeric")
  for( i in 1:length(id))
  {
    if(id[i]<10 & id[i]>0)
    {
      monitor<-paste("00",as.character(id[i]),".csv",sep="") #appending the required content to match it with the .csv files
    }
    else if(id[i]<100 &id[i]>9)
    {
      monitor<-paste("0",as.character(id[i]),".csv",sep="")
    }
    else
    {
      monitor<-paste(as.character(id[i]),".csv",sep="")
    }
    data<-read.csv(monitor) #storing the content of the csv file in data
    nobs<-sum(complete.cases(data)) #sum of the completed.cases gives the number of complete observations
    if(nobs > threshold)
    {
      cr<-c(cr,cor(data$sulfate[complete.cases(data)],data$nitrate[complete.cases(data)]))
    }
  }
  setwd(olddir)
  return (cr)
}