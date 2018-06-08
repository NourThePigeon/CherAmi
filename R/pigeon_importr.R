#' Batch imports Datavyu, Habit, or Director Data
#'
#' @param method Type of raw data being clean
#' @param pattern.regex Regular expression being searched
#' @param pattern.exc Regular expression being excluded
#' @param path File location
#' @return Raw data of a single type compiled in a large list with each element being a unique participant
#' @examples
#' pigeon_import("datavyu", "NumbR10", "Number Replication")
#' pigeon_import("habit", "Number Replication")
#' pigeon_import("director", "MR10", ".txt", "./data")
pigeon_import <- function(method = "default", pattern.regex = NULL, pattern.exc = NULL, path = getwd()){

  import_base <- function(path = getwd(), pattern.regex = NULL, pattern.exc = NULL, pattern.ext = ".csv") {
    files <- list.files(path, pattern = pattern.ext)
    if(is.character(pattern.regex)) {
      files <- files[grepl(pattern.regex,files)]
    }
    if(is.character(pattern.exc)){
      files <- files[!grepl(pattern.exc, files)]
    }
    if (pattern.ext == ".csv"){
      files_list <- lapply(files, readr::read_csv)
    } else if (pattern.ext == ".txt"){
      files_list <- lapply(files, readr::read_lines)
    }
    invisible(return(files_list))
  }

  if (method == "default"){

    OUT <- import_base(path, pattern.regex, pattern.exc)

  } else if (method == "habit"){

    OUT <- import_base(path, pattern.regex, pattern.exc)
    for (i in seq(OUT)) {
      srow <- OUT[[i]]$SubjectID
      srow <- grep("Phase", srow)
      if (length(srow) != 0){
        OUT[[i]] <- OUT[[i]][1:(srow - 1), ]
      }
      OUT[[i]] <- OUT[[i]][!is.na(OUT[[i]]$StimName), ]
    }

    invisible(return(OUT))

  } else if (method == "habitlive"){

    OUT <- import_base(path, pattern.regex, pattern.exc)
    files <- list.files(path, pattern = pattern.regex)
    if(is.character(pattern.exc)){
      files <- files[!grepl(pattern.exc, files)]
    }
    for (i in seq(OUT)) {
      srow <- OUT[[i]]$SubjectID
      srow <- grep("Phase", srow)
      OUT[[i]] <- OUT[[i]][!is.na(OUT[[i]]$StimName), ]
      if (identical(srow, integer(0))) {
        OUT[[i]] <- paste(OUT[[i]]$SubjectID[1], "does not have any live coding")
      } else if (nrow(OUT[[i]]) == srow) {
        OUT[[i]] <- paste(OUT[[i]]$SubjectID[1], "does not have any live coding")
      } else {
        colnames(OUT[[i]]) <- OUT[[i]][srow, ]
        OUT[[i]] <- OUT[[i]][(srow + 1):nrow(OUT[[i]]), ]    }

    }

    for(i in seq(OUT)){
      if(is.na(OUT[[i]][[1]])){
        OUT[[i]] <- paste(files[i], "does not have any live coding and an error was present")
      }
    }

    invisible(return(OUT))

  } else if (method == "datavyu"){

    OUT <- import_base(path, pattern.regex, pattern.exc)

    for (i in seq(OUT)){
      fourth <- seq(4, ncol(OUT[[i]]), 4)
      trialnames <- colnames(OUT[[i]])
      trialnames[fourth] <- sub('\\..*', '\\.code01', names(OUT[[i]][fourth]))
      names(OUT[[i]]) <- trialnames
    }

    invisible(return(OUT))

  } else if (method == "director"){
    OUT <- import_base(path, pattern.regex, pattern.exc, pattern.ext = ".txt")

    files <- list.files(path, pattern = pattern.ext)
    if(is.character(pattern.regex)) {
      files <- files[grepl(pattern.regex,files)]
    }
    if(is.character(pattern.exc)){
      files <- files[!grepl(pattern.exc, files)]
    }

    for(i in seq(OUT)){
      OUT[[i]] <- stringr::str_split_fixed(as.vector(OUT[[i]]), "\t", n = Inf)
    }

    names(OUT) <- files

    invisible(return(OUT))
  }
}
