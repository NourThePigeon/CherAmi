#' Quickly takes raw data to processed data
#'
#' @param method vector of types of data being used
#' @param patterns.regex vector of Regular Expression to find files
#' @param patterns.exc vector of Regular Expression to exclude from files
#' @param path vector of file locations
#' @return A dataframe with aggregated looking, widened data, and trial information
#' @example
#' pigeon_quick(c("datavyu", "habit"), c("NumbR10", "Number Replication"), c("Number Replication", ".txt"))
pigeon_quick <- function(method, patterns.regex = c(rep(NULL,length(method))), patterns.exc = c(rep(NULL,length(method))), path = c(rep(getwd(),length(method)))){

  Inprogress <- list(rep("",length(method)))

  for(i in seq(method)){
    Inprogress[[i]] <- pigeon_import(method[i], patterns.regex[i], patterns.exc[i], path[i])
    Inprogress[[i]] <- pigeon_clean(Inprogress[[i]], method[i])
  }

  OUT <- pigeon_process(x = Inprogress, method)

  invisible(return(OUT))

}
