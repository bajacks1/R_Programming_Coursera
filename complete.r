
complete <- function(directory, ID = 1:332) {
  
  #### Write a function that reads a directory full of files and 
  #### reports the number of completely observed cases in each data file. 
  #### The function should return a data frame where the first column is 
  #### the name of the file and the second column is the number of complete cases
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  
  newpath <- paste("./",directory,sep="")
  filelist <- list.files(path = newpath)
  filepaths <- paste(newpath,"/",filelist,sep="")
  for (i in ID){
    tmp1 <- read.csv(filepaths[i])
    id_val <- unique(tmp1$ID)
    tmp2 <- subset(tmp1,is.na(tmp1$sulfate)==FALSE & is.na(tmp1$nitrate)==FALSE)
    df_prep <- c(id_val,nrow(tmp2)) 
    ##df_prep <- c(i,nrow(tmp2)) ##this works but only because of file order
    df <- rbind(df, df_prep)
  }
  
  colnames(df) <- c("id","nobs")
  
  print(df)
  
}