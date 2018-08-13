## classifyr
##
##$ The point of pigeon_classify() is to quickly look over the columns in a df
##$    and convert any columns incorrectly typed "character" to "numeric".
##$    This is planned to happen after datacleaning steps already occur, but
##$    it can do very rudimentary cleaning as well (converting characters to NULL)

pigeon_classify <- function(x, null_values = NULL){


}

##$ Shortcut
pclassify <- function(x, null_values = NULL){

  pigeon_classify(x, null_values)

}
