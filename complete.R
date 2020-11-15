complete<-function(directory,id =1:332)
{
  olddir<-getwd() #obtaining the current working directory
  newdir<-paste(olddir,"/",directory,sep="")  #new directory where the .csv files are stored
  setwd(newdir) #setting working directory to the new directory
  monitor<-as.character(id)
  nobs<-vector()  #vector to store the number of completed cases

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
    nobs<-c(nobs,sum(complete.cases(data))) #sum of the completed.cases gives the number of complete observations
   }
  com<-data.frame(id=id,nobs=nobs)
  setwd(olddir) #setting the old directory back to the working directory
  return(com)
}