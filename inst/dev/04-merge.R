##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  04-merge.R
##  Purpose: load all and save as package data
##  Author: R HACKATHON
##  Date: 23-01-2023
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## laod packages
library(tidyverse)

## read in all .rds files
ls.BE_POP <- readRDS("inst/extdata/BELPOPLIST.rds")
ls.SPMA <- readRDS("inst/extdata/SPMALIST.rds")

## final data management steps
ls.BE_POP <- map(ls.BE_POP,. %>% mutate_at(c("YEAR", "POPULATION"), as.numeric))
ls.SPMA <- map(ls.SPMA,. %>% mutate_at(c("YEAR", "POPULATION"), as.numeric))

str(ls.BE_POP$BE_POP)
str(ls.SPMA$SPMA)

## save as global environment
list2env(ls.BE_POP, globalenv())
list2env(ls.SPMA, globalenv())

## create some basic plots

BE_POP %>%
  filter(SEX == "MF", AGE == "ALL") %>%
  group_by(YEAR) %>%
  summarise(POPULATION = sum(POPULATION)) %>%
  ggplot() +
  geom_line(aes(x = YEAR, y = POPULATION)) +
  ylim(0,12000000) +
  theme_bw()

BE_POP_PROJ %>%
  filter(SEX == "MF", AGE == "ALL") %>%
  group_by(YEAR) %>%
  summarise(POPULATION = sum(POPULATION)) %>%
  ggplot() +
  geom_line(aes(x = YEAR, y = POPULATION)) +
  ylim(0,14000000) +
  theme_bw()

SPMA %>%
  filter(SEX == "MF", AGE == "ALL") %>%
  group_by(YEAR) %>%
  summarise(POPULATION = sum(POPULATION)) %>%
  ggplot() +
  geom_line(aes(x = YEAR, y = POPULATION)) +
  ylim(0,12000000) +
  theme_bw()

SPMA_PROJ %>%
  filter(SEX == "MF", AGE == "ALL") %>%
  group_by(YEAR) %>%
  summarise(POPULATION = sum(POPULATION)) %>%
  ggplot() +
  geom_line(aes(x = YEAR, y = POPULATION)) +
  ylim(0,14000000) +
  theme_bw()

## save data into package data
usethis::use_data(BE_POP, BE_POP_COMTY, BE_POP_MUNTY, BE_POP_PROJ,
                  BE_POP_PROJ_ARRD, BE_POP_PROJ_COMTY, BE_POP_PROJ_PROV,
                  BE_POP_PROJ_RGN, BE_POP_PROV, BE_POP_RGN,
                  SPMA, SPMA_COMTY, SPMA_MUNTY, SPMA_PROJ, SPMA_PROJ_ARRD,
                  SPMA_PROJ_COMTY, SPMA_PROJ_PROV, SPMA_PROJ_RGN, SPMA_PROV,
                  SPMA_RGN,
                  overwrite = TRUE,
                  compress = "xz", version = 2)

