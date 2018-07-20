# library(dplyr)


pigeon_name <- function(x, ..., criteria, method = "default", save = TRUE, name = "default", unknowns = 2){


  INPUT <- list(x, ...)

  if(method == "birth" | method == "default"){

    if (!is.data.frame(criteria)){
      criteria <- read.csv(criteria, stringsAsFactors=FALSE)
    }

    for(i in seq(INPUT)){

      if(!is.data.frame(INPUT[[i]])){
        INPUT[[i]] <- read.csv(INPUT[[i]], sep = "\t", row.names = NULL, stringsAsFactors=FALSE)
      }

      colnames(INPUT[[i]]) <- c("first_child", "middle_child", "last_child", "date_birth",
                                "first_mother", "middle_mother", "last_mother", "date_death",
                                "date_due", "weight", "gest", "APGAR10", "street", "city",
                                "county", "state", "zip")


      INPUT[[i]][INPUT[[i]] == "-" | INPUT[[i]] == "UNKNOWN" | INPUT[[i]] == "" | INPUT[[i]] == "NA"] <- NA

      INPUT[[i]] <- transform(INPUT[[i]], weight = as.numeric(weight),
                                          gest = as.numeric(gest),
                                          APGAR10 = as.numeric(APGAR10),
                                          zip = as.numeric(zip))

      INPUT[[i]] <- INPUT[[i]][which(INPUT[[i]]$zip %in% criteria$zip), ]

      INPUT[[i]] <- INPUT[[i]][is.na(INPUT[[i]]$date_death), ]

      ###
      INPUT[[i]] <- INPUT[[i]][INPUT[[i]]$weight >= criteria$weight[1] | is.na(INPUT[[i]]$weight), ]

      INPUT[[i]] <- INPUT[[i]][INPUT[[i]]$gest >= criteria$gest[1] | is.na(INPUT[[i]]$gest), ]

      INPUT[[i]] <- INPUT[[i]][INPUT[[i]]$APGAR10 >= criteria$APGAR10[1] | is.na(INPUT[[i]]$APGAR10), ]

      if (unknowns == 3){
        INPUT[[i]] <- INPUT[[i]][!is.na(INPUT[[i]]$APGAR10) &
                                    !is.na(INPUT[[i]]$gest) &
                                    !is.na(INPUT[[i]]$weight), ]
      } else if (unknowns == 2){
        INPUT[[i]] <- INPUT[[i]][(!is.na(INPUT[[i]]$APGAR10) & !is.na(INPUT[[i]]$gest)) |
                                   (!is.na(INPUT[[i]]$weight) & !is.na(INPUT[[i]]$gest)) |
                                   (!is.na(INPUT[[i]]$APGAR10) & !is.na(INPUT[[i]]$weight)), ]

      } else if (unknowns == 1){
        INPUT[[i]] <- INPUT[[i]][!is.na(INPUT[[i]]$APGAR10) |
                                    !is.na(INPUT[[i]]$gest) |
                                    !is.na(INPUT[[i]]$weight), ]
      }

    }

    OUT <- list("","")
    OUT[[1]] <- do.call(rbind, INPUT)
    OUT[[1]] <- OUT[[1]][, -(18)]
    OUT[[2]] <- OUT[[1]][, -(4:12)]

    write.csv(OUT[[1]],"Names_LabEdit.csv", row.names = FALSE)
    write.csv(OUT[[2]],"Names_BMSEdit.csv", row.names = FALSE)

    return(OUT[[1]])

  } else if (method == "death"){


    if (!is.data.frame(criteria)){
        criteria <- read.csv(criteria, stringsAsFactors=FALSE)
      }

    deathdata <- INPUT[[1]]
    birthdata <- INPUT[[2]]

    if(!is.data.frame(deathdata)){
        deathdata <- read.csv(deathdata, row.names = NULL, stringsAsFactors=FALSE)
      }
    if(!is.data.frame(birthdata)){
        birthdata <- read.csv(birthdata, row.names = NULL, stringsAsFactors=FALSE)
      }

    deathdata[deathdata == "-" | deathdata == "UNKNOWN" | deathdata == "" | deathdata == "NA" | deathdata == "0UNK"] <- NA
    birthdata[birthdata == "-" | birthdata == "UNKNOWN" | birthdata == "" | birthdata == "NA"] <- NA

    colnames(deathdata) <- c("first_child", "middle_child", "last_child", "year_birth",
                             "age_year", "age_month", "date_death", "first_mother", "last_mother")
    colnames(birthdata) <- c("first_child", "middle_child", "last_child", "date_birth",
                             "first_mother", "middle_mother", "last_mother", "date_death",
                             "date_due", "weight", "gest", "APGAR10", "street", "city",
                             "county", "state", "zip")
    colnames(criteria) <- colnames(deathdata)


    year_birth <- birthdata$date_birth
    year_birth2 <- regmatches(year_birth, regexpr("\\/[^\\/]*$", year_birth))
    year_birth3 <- gsub("/","",year_birth2)
    year_birth4 <- paste0(20,year_birth3)
    birthdata$year_birth <- year_birth4
    birthdata$full_child <- paste(birthdata$first_child, birthdata$middle_child, birthdata$last_child)

    deathdata <- deathdata[deathdata$year_birth >= criteria$year_birth, ]
    deathdata$full_child <- paste(deathdata$first_child, deathdata$middle_child, deathdata$last_child)

    deaths <- deathdata[deathdata$full_child %in% birthdata$full_child, ]

    birthdata <- birthdata[!(birthdata$full_child %in% deaths$full_child &
                             birthdata$year_birth %in% deaths$year_birth), ]
    deathdata <- deathdata[!(deathdata$full_child %in% deaths$full_child &
                             deathdata$year_birth %in% deaths$year_birth), ]

    write.csv(birthdata,"Names_LabEdit.csv", row.names = FALSE)

    birthdata2 <- birthdata[, -(18:19)]
    birthdata2 <- birthdata2[, -(4:12)]
    write.csv(birthdata2, "Names_BMSEdit.csv", row.names = FALSE)

    write.csv(deathdata, "Names_deathdata.csv")


  } else if (method == "countIRB") {

  } else if (method == "countCPHS") {

  } else if (method == "request") {

  } else if (method == "returns"){

  }



}


