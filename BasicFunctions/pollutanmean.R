#Write a function named 'pollutantmean' that calculates 
#the mean of a pollutant (sulfate or nitrate) across a specified list of monitors. 
#The function 'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
#particulate matter data from the directory specified in the 'directory' argument 
#and returns the mean of the pollutant across all of the monitors, 
#ignoring any missing values coded as NA


pollutanmean <- function( directory, pollutant, id=1:332){
        #loop throug all file
       
        len<-0;
        total<-0;
        for(i in head(id,1):tail(id,1)){
                #read data files in directory
               if(i<10){
                       
                       i=paste(c("00",i),collapse = "")
               }else if(i<100){
                       i=paste(c("0",i),collapse = "")
               }
                
                filename = paste(directory, i, sep = "/")
                filename1 = paste(filename, "csv", sep = ".")
                print(filename1)
                data_fil <- read.csv(filename1);
                total<-sum(data_fil[pollutant],na.rm=TRUE)+total;
                print(total)
                len = len + nrow(data_fil[!is.na(data_fil[pollutant]),])
                print(len)
                
              
                
        }
        total/len
}