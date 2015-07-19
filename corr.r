
corr <- function(directory,threshold=0){
  
  #### Write a function that takes a directory of data files and 
  #### a threshold for complete cases and calculates the correlation 
  #### between sulfate and nitrate for monitor locations where the 
  #### number of completely observed cases (on all variables) is 
  #### greater than the threshold. The function should return a vector 
  #### of correlations for the monitors that meet the threshold requirement. 
  #### If no monitors meet the threshold requirement, then the function 
  #### should return a numeric vector of length 0.
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  newpath <- paste("./",directory,sep="")
  filelist <- list.files(path = newpath)
  filepaths <- paste(newpath,"/",filelist,sep="")
  for (i in 1:332){
    tmp1 <- read.csv(filepaths[i])
    tmp2 <- subset(tmp1,is.na(tmp1$sulfate)==FALSE & is.na(tmp1$nitrate)==FALSE)
    if (nrow(tmp2)>= threshold){
      df <- c(df,cor(tmp2$sulfate,tmp2$nitrate))
    }
  }
  
  d <- unlist(df)
  d
  

}