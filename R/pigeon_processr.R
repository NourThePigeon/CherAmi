#' Processes and combines cleaned data
#'
#' @param x A list of data to be processed and cleaned
#' @param method A vector listing types of input data
#' @param exports Logical or 1:3, determines what csv.s are exported and when
#' @param endformat "wide" or "long"
#' @param join "full","left","right","" determines how data will be combined
#' @return A dataframe with aggregated looking, widened data, and trial information
#' @example
#' pigeon_process(list(vyu.data, habit.data), c("datavyu", "habit"), join = "left")
pigeon_process <- function(x, method = "default", exports = TRUE, endformat = "wide", join = "full"){

  OUT_temp <- list(rep("",length(method)))

  for (i in seq(x)){
    if(method[i] == "datavyu"){

      datavyu <- x[[i]]

      datavyu <- datavyu[ , c("part", "trial", "coder1.code01", "look_1.code01", "look_1.duration")]

      datavyu <- dplyr::summarise(
        dplyr::group_by(datavyu, part, trial, coder1.code01, look_1.code01),
        aggregate = sum(look_1.duration))

      datavyu$study <- gsub("[[:digit:]]", "", datavyu$part)
      datavyu$part <- gsub("[^[:digit:]]", "", datavyu$part)
      datavyu <- transform(datavyu, part      = as.numeric(part),
                           trial     = as.numeric(trial),
                           aggregate = as.numeric(aggregate),
                           study     = as.character(study))

      datavyu <- tidyr::spread(datavyu, look_1.code01, aggregate)

      colnames(datavyu) <- tolower(colnames(datavyu))

      OUT_temp[[i]] <- datavyu

    } else if (method[i] == "habit") {

      habit <- x[[i]]
      habit$part <- habit$SubjectID
      habit <- habit[ , -grep("SubjectID", names(habit))]
      habit$study <- gsub("[[:digit:]]", "", habit$part)
      habit$part <- gsub("[^[:digit:]]", "", habit$part)
      habit$part <- as.numeric(habit$part)
      colnames(habit) <- tolower(colnames(habit))

      OUT_temp[[i]] <- habit


    } else if (method[i] == "director"){

      director <- x[[i]]
      director$part <- as.numeric(director$part)
      director$trial <- as.numeric(director$trial)

      colnames(director) <- tolower(colnames(director))

      OUT_temp[[i]] <- director

    }

  }

  OUT <- dplyr::full_join(OUT_temp[[1]],OUT_temp[[2]], by = c("part", "study","trial"))

  if (endformat == "wide"){
    if (any(grepl("director", OUT))){
      OUT <- tidyr::gather(OUT, info, val, -trial, -part, -study, -order, -sex, -code, -agemos)
    } else {
      OUT <- tidyr::gather(OUT, info, val, -trial, -part, -study, -order)
    }
    OUT <- tidyr::unite(OUT, info1, info, trial)
    OUT <- tidyr::spread(OUT, info1, val)
  }

  invisible(return(OUT))

}
