convert_2lev_factor_to_logical <- function(factor_vector){
  
  levels(factor_vector) <- c(FALSE,TRUE) #"n" vor "y" im Alphabet, deshalb False,True
  factor_vector <- as.logical(factor_vector)
  
  return(factor_vector)
}
