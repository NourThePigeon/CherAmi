## classifyr
##
##$ The point of pigeon_classify() is to quickly look over the columns in a df
##$    and convert any columns incorrectly typed "character" to "numeric".
##$    This is planned to happen after datacleaning steps already occur, but
##$    it can do very rudimentary cleaning as well (converting characters to NULL)
pigeon_classify <- function(x, na_values = NA){

  Strtype <- sapply(x, class)

  na_values <- c(na_values, NA, "", " ", "-", "_", "NA", NA_real_, NA_complex_,
                 NA_character_, NA_integer_, "NA_real_", "NA_complex_",
                 "NA_character_", "NA_integer_", "na", "n/a", "N/A", "Na", "null",
                 "Null", "unknown", "Unknown", "UNKNOWN", "?", "??", "???", "x", NULL)
  na_values <- unique(na_values)

  for(i in seq(Strtype)){

    if(Strtype[i] == "character"){
      y <- x[,i]
      y[y %in% na_values] <- NA
      x[,i] <- y

      if(any(str_detect(x[,i], "[^0-9]") %in% TRUE)){
        next
      } else {
        x[,i] <- as.numeric(x[,i])
      }

    } else { next }

    }
return(x)
}

