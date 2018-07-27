## classifyr
##
##$ The point of pigeon_classify() is to quickly look over the columns in a df
##$    and convert any columns incorrectly typed "character" to "numeric".
##$    This is planned to happen after datacleaning steps already occur, but
##$    it can do very rudimentary cleaning as well (converting characters to NULL)

pigeon_classify <- function(x, null_values = NULL){
  
  Strtype <- sapply(x, class)
  
  null_values <- c(null_values, "", " ", "-", "_", "NA", "NA_real_", "NA_complex_", "NA_character_", "NA_integer",
                   "na", "n/a", "N/A", "Na", "null", "Null", "unknown", "Unknown", "UNKNOWN", "?")
  null_values <- unique(null_values)
  
  x[x %in% null_values] <- NULL
  
  for(i in seq(Strtype)){
    
    if(Strtype[i] == "character"){
    
    } else { next }
      
    }

}
