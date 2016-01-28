#Write a function that takes a directory of data files and a threshold 
#for complete cases and calculates the correlation between sulfate and nitrate 
#for monitor locations where the number of completely observed cases
#(on all variables) is greater than the threshold. 
#The function should return a vector of correlations for the monitors 
#that meet the threshold requirement. 
#If no monitors meet the threshold requirement, 
#then the function should return a numeric vector of length 0.

corr<- function( directory, threshold){
        #loop throug all fie
        DATA<- list()
        all.the.files <- list.files(directory,full=TRUE)
        print(all.the.files[1])
        for(i in 1:length(all.the.files)){
                filename = paste(all.the.files[i], sep = ".");
                data_fil <- read.csv(filename);
              if(nrow(data_fil[complete.cases(data_fil),])>threshold)  {
                      DATA<- do.call(rbind(DATA,c(data_fil["nitrate"],data_fil["sulfate"])));
              }
                
        }
        #all.the.data <- lapply( all.the.files,  read.csv, header=TRUE)
        #DATA <- do.call("rbind", all.the.data)
       #d<-cor(DATA["sulfate"],DATA["nitrate"],use = "complete.obs")
       DATA

}