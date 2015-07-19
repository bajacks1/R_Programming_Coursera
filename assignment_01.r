#####################################
## Functions for programming assignment #1
#####################################

pollutantmean <- function(directory, pollutant, ID = 1:332) {
  
  #### PART 1:
  #### INSTRUCTIONS: Write a function named 'pollutantmean' that calculates 
  #### the mean of a pollutant (sulfate or nitrate) across a specified list 
  #### of monitors. The function 'pollutantmean' takes 
  #### three arguments: 'directory', 'pollutant', and 'id'. 
  #### Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
  #### particulate matter data from the directory specified in the 'directory' 
  #### argument and returns the mean of the pollutant across all of the monitors, 
  #### ignoring any missing values coded as NA. A prototype of the function is as follows 
  
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  
  newpath <- paste("./",directory,sep="")
  filelist <- list.files(path = newpath)
  filepaths <- paste(newpath,"/",filelist,sep="")
  for (i in ID){
    df <- rbind(df, read.csv(filepaths[i]))
  }
  
  
  
  if (pollutant == "sulfate") {
    mean.default(df$sulfate,na.rm=TRUE) 
  }
  else if (pollutant == "nitrate") {
    mean.default(df$nitrate,na.rm=TRUE)
  }
  else {
    print("PARAMETER ERROR: Pollutant type incorrectly defined. Choose from sulfate or nitrate.")
  }
  
  
  
}

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
      df <- rbind(df,cor(tmp2$sulfate,tmp2$nitrate))
    }
  }
  df
  ##summary(df)
  
}