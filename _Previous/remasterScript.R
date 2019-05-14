#### Creating the pigeonverse ----
# Basically just following the simple instructions from mkearney's awesome pkgverse
devtools::install_github("mkearney/pkgverse")


# for the actual packages
devtools::install_github("NourAl-Zaghloul/pigeontools")
devtools::install_github("NourAl-Zaghloul/PigeonAnalyzeR")
devtools::install_github("NourAl-Zaghloul/PigeonExperimentR")
devtools::install_github("NourAl-Zaghloul/PigeonExploreR")
devtools::install_github("NourAl-Zaghloul/PigeonTidyR")
devtools::install_github("NourAl-Zaghloul/PigeonWeirdR")

pigeontools <- c("PigeonExploreR", "PigeonAnalyzeR", "PigeonTidyR", "PigeonExperimentR", "PigeonWeirdR")
dir.create("~/packages")

pkgverse::pkgverse("pigeontools", pigeontools,
                   keep = "~/packages",
                   use = c("readme_rmd", "rstudio", "testthat", "git"),
                   install_if = TRUE)
