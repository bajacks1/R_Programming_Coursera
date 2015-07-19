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
