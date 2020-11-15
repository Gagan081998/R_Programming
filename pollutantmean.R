pollutantmean <- function(directory,pollutant,id=1:332)
{
  olddir<-getwd() #getting current directory
  newdir<- paste(olddir,"/",directory,sep = "") #combining the present directory with the location of the specdata
  setwd(newdir) #changing directory
  monitor<-as.character(id)
  flag<-0 
  
  #For reading from the files in the directory with the id as given in the id list      
  for( i in 1:length(id))
  {
    if(id[i]<10 & id[i]>0)
    {
      monitor<-paste("00",as.character(id[i]),".csv",sep="")  #appending the required content to match it with the .csv files
    }
    else if(id[i]<100 &id[i]>9)
    {
      monitor<-paste("0",as.character(id[i]),".csv",sep="")
    }
    else
    {
      monitor<-paste(as.character(id[i]),".csv",sep="")
    }
    if(flag==0)     #to check if its the first file that is being read of the id list
    {
      data<-read.csv(monitor)
      flag<-1
    }
    else
    {
      data<-rbind(data,read.csv(monitor)) #combining the contents of more than one csv file together
    }
  }
  x<-data[pollutant]  
  x_tidy<- x[!(is.na(x))]   #selecting on the content of the required pollutant 
  avg<-mean(x_tidy,na.rm = TRUE)  #calculating mean of the pollutant data obtained after cleaning
  setwd(olddir) # setting the old directory back to the working directory
  return (avg)  #returning the calculated mean 
  }