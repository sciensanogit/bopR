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

BE_POP <- lapply(ls, readRDS)
names(BE_POP) <- tools::file_path_sans_ext(basename((ls)))

## loop over all datasets and calculate mid-year population
## e.g. midyear 2020 = mean(2020, 2021)

SPMA <- list()

for (i in seq_along(BE_POP)) {
  # print(paste0(i,"/",length(BE_POP)))
  ## get min and max year
  y.min <- min(as.numeric(BE_POP[[i]]$YEAR))
  y.max <- max(as.numeric(BE_POP[[i]]$YEAR))
  ## save results
  tmp <- list()

  for (y in y.min:(y.max-1)) {
    print(paste0(i,"/",length(BE_POP), ": year = ",y))
    ## subset subsequent years
    tmp.y <- subset(BE_POP[[i]], YEAR == c(y))
    tmp.yplus1 <- subset(BE_POP[[i]], YEAR == c(y+1))
    ## merge both datasets
    tmp.merge <- full_join(tmp.y, select(tmp.yplus1, -YEAR),
                           by = c("REFNIS" ,"SEX", "AGE", "AGE5", "AGE10", "DESCR_DE",
                                  "DESCR_EN", "DESCR_FR", "DESCR_NL"))
    tmp.merge$YEAR <- y
    tmp.merge$POPULATION.x[is.na(tmp.merge$POPULATION.x)] <- 0
    tmp.merge$POPULATION.y[is.na(tmp.merge$POPULATION.y)] <- 0

    tmp.merge <- tmp.merge %>%
      rowwise() %>%
      mutate(POPULATION = (POPULATION.x + POPULATION.y)/2) %>%
      ungroup() %>%
    ## remove population.x and population.y
      select(-POPULATION.x, -POPULATION.y)

    tmp[[as.character(y)]] <- tmp.merge

  }
  ## list to dataframe
  tmp <- bind_rows(tmp)
  ## save in list
  ## .. name
  name.i <- gsub(pattern = "BE_POP", x = names(BE_POP)[i], replacement = "SPMA")
  ## .. save
  SPMA[[name.i]] <- tmp
}

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## SAVE the result ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

saveRDS(BE_POP, "inst/extdata/BELPOPLIST.rds")
saveRDS(SPMA, "inst/extdata/SPMALIST.rds")
