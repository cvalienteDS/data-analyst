
get_data <- function(){
  loginfo('Reading data from disk')
  df <- read.csv( "./sources/movies.csv", 
                  header=T , 
                  dec=".",
                  sep=',', 
                  encoding = "latin1",
                  stringsAsFactors = F,
                  na.strings=c("#N/D", "#VALOR","#¡VALOR!", "#VALOR!", "#DIV/0!", "#NOMBRE?", "#REF", "Err:522", "#REF!"))
  
  df <- dplyr::select(df, -c("director", "star" , "writer"))
  
  return(df)
}