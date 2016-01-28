#Write a function that reads a directory full of files and reports
#the number of completely observed cases in each data file. 
#The function should return a data frame where the first column is the name of
#the file and the second column is the number of complete cases.#

complete<- function( directory, id=1:332){
        #loop throug all file
        print(is.vector(id))
        print(length(id))
      
        
        L<-list();
        k=1;
        for(i in 1:length(id)){
                #read data files in directory
                i<-id[i];
                if(i<10){
                        
                        i=paste(c("00",i),collapse = "")
                }else if(i<100){
                        i=paste(c("0",i),collapse = "")
                }
                
                filename = paste(directory, i, sep = "/")
                filename1 = paste(filename, "csv", sep = ".")
                #print(filename1)
                data_fil <- read.csv(filename1);
                #get 
                L[[k]]=c(i,nrow(data_fil[complete.cases(data_fil),]));
                
                k=k+1;
                
                
        }
        L
}