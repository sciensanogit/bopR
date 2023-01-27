##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  03-spma-midpopulation.R
##  Purpose: load all and save as package data
##  Author: R HACKATHON
##  Date: 23-01-2023
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## load packages
library(tidyverse)
library(utils)
library(openxlsx)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## SPMA mid-population ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## read in all statbel .rds files
ls <-
  list.files(path = "inst/extdata/",
             pattern = "BE_POP",
             full.names = TRUE)

BE_POP <- sapply(ls, readRDS)
names(BE_POP) <- tools::file_path_sans_ext(basename(names(BE_POP)))

## loop over all datasets and calculate mid-year population
## e.g. midyear 2020 = mean(2020, 2021)

SPMA <- list()

for (i in seq_along(BE_POP)) {
  print(paste0(i,"/",length(BE_POP)))
  ## get min and max year
  y.min <- min(as.numeric(BE_POP[[i]]$YEAR))
  y.max <- max(as.numeric(BE_POP[[i]]$YEAR))
  for (y in y.min:(y.max-1)) {
    print(paste0(i,"/",length(BE_POP), ": year = ",y))
    ## subset subsequent years
    tmp.y <- subset(BE_POP[[i]], YEAR == c(y))
    tmp.yplus1 <- subset(BE_POP[[i]], YEAR == c(y+1))
    ## merge both datasets
    tmp.merge <- full_join(tmp.y, select(tmp.yplus1, -YEAR), by = c("SEX", "AGE", "AGE5", "AGE10"))
    tmp.merge <- tmp.merge %>%
      rowwise() %>%
      mutate(POPULATION = mean(POPULATION.x, POPULATION.y)) %>%
      ungroup() %>%
    ## remove population.x and population.y
      select(-POPULATION.x, -POPULATION.y)
    ## save in list
    ## .. name
    name.i <- gsub(pattern = "BE_POP", x = names(BE_POP)[i], replacement = "SPMA")
    ## .. save
    SPMA[[name.i]] <- tmp.merge
  }
}

## save as global environment
list2env(dta,globalenv())

## save data into package data
usethis::use_data(BE_POP, BE_POP_COMTY, BE_POP_MUNTY, BE_POP_PROJ,
                  BE_POP_PROJ_ARRD, BE_POP_PROJ_COMTY, BE_POP_PROJ_PROV,
                  BE_POP_PROJ_RGN, BE_POP_PROV, BE_POP_RGN,
                  overwrite = TRUE,
                  compress = "xz", version = 2)
