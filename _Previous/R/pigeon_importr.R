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
pigeon_import <- function(method = "datavyu", pattern.regex = NULL, pattern.exc = NULL, path = getwd()){

  ##$ This is a function inside pigeon_import. It does the most basic job of pigeon_import.
  ##.     IE finding the file you want and reading them sensibly into a large list.
  import_base <- function(path = getwd(), pattern.regex = NULL, pattern.exc = NULL, pattern.ext = ".csv") {
    files <- list.files(path, pattern = pattern.ext)
    if(is.character(pattern.regex)) {
      files <- files[grepl(pattern.regex, files)]
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

  if (method == "habit"){
    ##$ habit outputs data two ways on top of each other. This takes the top half.
    ##$      This has overall data and per-look data following in columns.

    OUT <- import_base(path, pattern.regex, pattern.exc)

    ##. This removes the bottom section of the habit output
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
    ##$ This exports the bottom section of the habit output.
    ##$     This has per-look data by row.

    OUT <- import_base(path, pattern.regex, pattern.exc)
    files <- list.files(path, pattern = pattern.regex) ##!habitlive: check this out. I think it's redundant.
    if(is.character(pattern.exc)){ ##!habitlive: same as above
      files <- files[!grepl(pattern.exc, files)] ##!habitlive: same as above
    }

    ##. This removes the top section of the habit output
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

  } else if (method == "datavyu" | method == "datavyu2" | method == "reliability"){
    ##$ All three of these are imported the same way since they use the same files.

    OUT <- import_base(path, pattern.regex, pattern.exc)

    ##. This renames the fourth column to end in ".code01" instead of ".original"
    ##$     Makes later processing much easier.
    for (i in seq(OUT)){
      fourth <- seq(4, ncol(OUT[[i]]), 4)
      trialnames <- colnames(OUT[[i]])
      trialnames[fourth] <- sub('\\..*', '\\.code01', names(OUT[[i]][fourth]))
      names(OUT[[i]]) <- trialnames
    }

    invisible(return(OUT))

  } else if (method == "director"){
    ##$ This is the most different since director uses tab-delimination, not comma.
    ###     Director is also annoying how it outputs data so we have to do it manually.

    pattern.ext = ".txt"
    OUT <- import_base(path, pattern.regex, pattern.exc, pattern.ext) ##!import_base: add in functionality to export filenames instead of raw data as well

    ##. records the filenames.
    ##$     important because import_base loses that info and oft director output has no part_info
    files <- list.files(path, pattern = pattern.ext)
    if(is.character(pattern.regex)) {
      files <- files[grepl(pattern.regex, files)]
    }
    if(is.character(pattern.exc)){
      files <- files[!grepl(pattern.exc, files)]
    }

    ##. Actually separates out the tabs into columns
    for(i in seq(OUT)){
      OUT[[i]] <- stringr::str_split_fixed(as.vector(OUT[[i]]), "\t", n = Inf)
    }

    ##. Names the list elements so we know what participant it is.
    names(OUT) <- files

    invisible(return(OUT))
  }
}
