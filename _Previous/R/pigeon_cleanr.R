#' Cleans raw data to be ready for processing
#'
#' @param x List of raw data
#' @param method Type of raw data being clean
#' @return A dataframe with usable, organised, and standardized but not processed data.
#' @example
#' pigeon_clean(mydata1, "datavyu")
pigeon_clean <- function(x, method = "datavyu"){
  if (method == "habit") {

    for(i in seq(x)) {

      ##!habit: once I finish classifyr this would be where to implement. would add flexibility
      x[[i]]$Trial <- as.integer(x[[i]]$Trial)
      x[[i]]$Repeat <- as.integer(x[[i]]$Repeat)
      x[[i]]$StimID <- as.integer(x[[i]]$StimID)
      x[[i]]$'Trial Start' <- as.integer(x[[i]]$'Trial Start')
      x[[i]]$'Trial End' <- as.integer(x[[i]]$'Trial End')
      x[[i]]$TotalLook <- as.integer(x[[i]]$TotalLook)
      x[[i]]$TotalLookAway <- as.integer(x[[i]]$TotalLookAway)
      x[[i]]$TotalLeft <- as.integer(x[[i]]$TotalLeft)
      x[[i]]$TotalCenter <- as.integer(x[[i]]$TotalCenter) ##!habit: add something to determine if it's 1,2, or 3 scope. Currently only works with 3 scope.
      x[[i]]$TotalRight <- as.integer(x[[i]]$TotalRight)
      x[[i]]$LookEnabled <- as.integer(x[[i]]$LookEnabled)
      x[[i]]$LookDisabled <- as.integer(x[[i]]$LookDisabled)

      x[[i]]$Trial <- seq(1,nrow(x[[i]]))

    }

    OUT <- do.call(rbind, x)
    invisible(return(OUT))

  } else if (method == "director"){

    ##$ Cleans up the part_info
    files <- names(x)
    files <- gsub("\\..*", "", files)
    numbers <- gsub("[^[:digit:]]", "", files)
    study <- gsub("[[:digit:]]", "", files)

    ##$ This is the actual cleaning, director needs a lot of it.
    for(i in seq(x)) {

      ##. Cleans out missing data and attaches metadata to each line.
      x[[i]][x[[i]] == ""] <- NA
      x[[i]] <- data.frame(x[[i]], stringsAsFactors = FALSE)

      srow <- grep(":", x[[i]][, 1])
      x_temp <- x[[i]][(srow[2] + 1):nrow(x[[i]]), ]
      x_temp <- x_temp[ , colSums(is.na(x_temp)) < nrow(x_temp)]
      colnames(x_temp) <- c("trial_info", "order", "side_info")

      colnames(x[[i]]) <- x[[i]][srow[1] + 1, ]
      x[[i]] <- x[[i]][(srow[1] + 2):(srow[2] - 1), ]
      x[[i]] <- x[[i]][rowSums(is.na(x[[i]]))<ncol(x[[i]]), ]
      x[[i]] <- x[[i]][ , colSums(is.na(x[[i]])) < nrow(x[[i]])]
      x[[i]] <- cbind(x[[i]], x_temp)

      x[[i]] <- x[[i]][complete.cases(x[[i]][, 1]), ]

      x[[i]]$Code <- rep(files[i], length(x[[i]]$Code))
      x[[i]]$study <- rep(study[i], length(x[[i]]$Sub))
      x[[i]]$part <- rep(numbers[i], length(x[[i]]$Sub))
      x[[i]] <- x[[i]][ , -grep("Sub",colnames(x[[i]]))]

      x[[i]]$part <- as.integer(x[[i]]$part)
      x[[i]]$AgeMos <- as.integer(x[[i]]$AgeMos)
      x[[i]]$trial <- as.integer(x[[i]]$trial)
    }

    OUT <- do.call(rbind, x)
    invisible(return(OUT))

  } else if (method == "datavyu"){

    ##. This renames the fourth column to end in ".code01" instead of ".original"
    ##$     Makes later processing much easier.
    ##!datavyu: check to see if I included this in importr, should be here not there.
    for (i in seq(x)){
      fourth <- seq(4,ncol(x[[i]]),4)
      trialnames <- colnames(x[[i]])
      trialnames[fourth] <- sub('\\..*', '\\.code01', names(x[[i]][fourth]))
      names(x[[i]]) <- trialnames

      if (is.na(x[[i]]$coder1.onset[1])){
        x[[i]] <- NA
        next()
      }

      ##$ Keeps track of the correct trials done
      ##.     Oft for reliability checking we won't do every trial. This screws up R w/o this code.
      trial <- rep(0,length(x[[i]]$coder1.onset[!is.na(x[[i]]$coder1.onset)]))
      ttrial <- x[[i]]$trial.onset[!is.na(x[[i]]$trial.onset)]

      for(t in 1:length(trial)){
        for(tt in 1:length(ttrial)){
          if(x[[i]]$coder1.onset[t] == ttrial[tt]){
            trial[t] <- tt
          } else {
            next()
          }
        }
      }

      ##$ Corresponds all the data together
      x_subset <- as.list(rep("",length(trial)))

      for(j in 1:length(na.omit(x_subset))){

        x_subset[[j]] <- data.frame(na.omit(subset(x[[i]],x[[i]]$look_1.onset >= x[[i]]$trial.onset[j] &
                                                     x[[i]]$look_1.onset <= x[[i]]$trial.offset[j],
                                                   select = c(look_1.onset, look_1.offset, look_1.code01))))

        x_subset[[j]] <- dplyr::mutate(x_subset[[j]], look_1.duration = look_1.offset - look_1.onset)

        x_subset[[j]]$trial <- rep(trial[j], length(x_subset[[j]]$look_1.duration))

        x_subset[[j]]$coder1.code01 <- rep(x[[i]]$coder1.code01[j], length(x_subset[[j]]$look_1.duration))

        x_subset[[j]]$part <- rep(x[[i]]$participant.code01[1], length(x_subset[[j]]$look_1.duration))

      }

      x[[i]] <- do.call(rbind, x_subset)

    }

    OUT <- do.call(rbind, x)
    OUT <- OUT[complete.cases(OUT), ]
    invisible(return(OUT))

  } else if (method == "datavyu2") {
    ##$ Same exact thing as datavyu, but for second coder (reliability step)

    for (i in seq(x)){
      fourth <- seq(4,ncol(x[[i]]),4)
      trialnames <- colnames(x[[i]])
      trialnames[fourth] <- sub('\\..*', '\\.code01', names(x[[i]][fourth]))
      names(x[[i]]) <- trialnames

      if (is.na(x[[i]]$coder2.onset[1])){
        x[[i]] <- NA
        next()
      }

      trial <- rep(0,length(x[[i]]$coder2.onset[!is.na(x[[i]]$coder2.onset)]))
      ttrial <- x[[i]]$trial.onset[!is.na(x[[i]]$trial.onset)]

      for(t in 1:length(trial)){
        for(tt in 1:length(ttrial)){
          if(x[[i]]$coder2.onset[t] == ttrial[tt]){
            trial[t] <- tt
          } else {
            next()
          }
        }
      }

      x_subset <- as.list(rep("",length(trial)))

      for(j in 1:length(na.omit(x_subset))){

        x_subset[[j]] <- data.frame(na.omit(subset(x[[i]],x[[i]]$look_2.onset >= x[[i]]$trial.onset[j] &
                                                     x[[i]]$look_2.onset <= x[[i]]$trial.offset[j],
                                                   select = c(look_2.onset, look_2.offset, look_2.code01))))

        x_subset[[j]] <- dplyr::mutate(x_subset[[j]], look_2.duration = look_2.offset - look_2.onset)

        x_subset[[j]]$trial <- rep(trial[j], length(x_subset[[j]]$look_2.duration))

        x_subset[[j]]$coder2.code01 <- rep(x[[i]]$coder2.code01[j], length(x_subset[[j]]$look_2.duration))

        x_subset[[j]]$part <- rep(x[[i]]$participant.code01[1], length(x_subset[[j]]$look_2.duration))

      }

      x[[i]] <- do.call(rbind, x_subset)
    }

    OUT <- do.call(rbind, x)
    OUT <- OUT[complete.cases(OUT), ]
    invisible(return(OUT))
  }

}
