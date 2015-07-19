proportion_sample_size <- function(p, ME=0.02, Z=1.96){
  var <- p * (1-p)
  n <- var * (Z^2) / (ME^2)
  n
  
}