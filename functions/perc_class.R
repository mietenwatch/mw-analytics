#Function for binary varibles: determine % of most frequent realisation   
perc_class <- function(x, char) {
  stopifnot(class(x)=="character" | class(x)=="factor")
  t <- table(x)
  perc <- t[char]*100 / sum(t)
  return( paste0(round(perc, digits=0), "%"))
}