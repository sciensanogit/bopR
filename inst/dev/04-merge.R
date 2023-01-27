##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  04-merge.R
##  Purpose: load all and save as package data
##  Author: R HACKATHON
##  Date: 23-01-2023
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## read in all .rds files
ls <-
  list.files(path = "inst/extdata/",
             pattern = "BE_POP",
             full.names = TRUE)

dta <- sapply(ls, readRDS)
names(dta) <- tools::file_path_sans_ext(basename(names(dta)))

## save as global environment
list2env(dta,globalenv())

## save data into package data
usethis::use_data(BE_POP, BE_POP_COMTY, BE_POP_MUNTY, BE_POP_PROJ,
                  BE_POP_PROJ_ARRD, BE_POP_PROJ_COMTY, BE_POP_PROJ_PROV,
                  BE_POP_PROJ_RGN, BE_POP_PROV, BE_POP_RGN,
                  overwrite = TRUE,
                  compress = "xz", version = 2)
