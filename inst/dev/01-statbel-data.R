##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  01-statbel-data.R
##  Purpose: load and update statbel population data
##  Author: R HACKATHON
##  Date: 23-01-2023
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## INFO
## This script will unpack and read the different population datasets by Statbel,
## and will aggregate them according to different geospatial levels (MNTY, ARRD,
## PROV, RGN, and COUNTRY)
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## load packages
library(tidyverse)
library(utils)
library(openxlsx)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BELGIUM DATA STATBEL ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## .. COMBINE RAW DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## read-in and extract data
ls <-
  list.files(path = "inst/extdata/",
             pattern = ".zip",
             full.names = TRUE)
dta <- tibble()
i <- 1

for (fzip in ls) {
  print(paste0(i, "/", length(ls)))
  ## extract year
  year <-
    unlist(stringr::str_split(tools::file_path_sans_ext(basename(fzip)), pattern = "_"))
  year <- year[length(year)]
  ## check file
  fname <- unzip(fzip, list = TRUE)$Name
  ## unzip
  unzip(zipfile = fzip)
  ## read in the data
  ftmp <-
    read.table(
      file = paste0(fname),
      sep = "|",
      header = TRUE
    )
  ## merge dta
  ftmp$CD_YEAR <- year
  dta <- bind_rows(dta, ftmp)
  ## remove file
  rm(ftmp)
  file.remove(paste0(fname))
  ## add +1
  i <- i + 1
}

## save the combined result
saveRDS(object = dta,
        file = sprintf("inst/extdata/statbel-data-combined-%s-%s.rds", min(dta$CD_YEAR), max(dta$CD_YEAR)))

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## .. LOAD DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## load prepared data with population data
dta <- readRDS("inst/extdata/statbel-data-combined-2009-2022.rds")
## read-in data for NUTS names and NIS
nuts <- openxlsx::read.xlsx("inst/extdata/TU_COM_NUTS_LAU-20190101.xlsx")

## check the structure of the data
head(dta)
str(dta)
unique(dta$CD_YEAR)
sort(unique(dta$CD_AGE))
unique(dta$CD_SEX)
unique(dta$TX_RGN_DESCR_NL)
unique(dta$TX_RGN_DESCR_FR)
unique(dta$CD_RGN_REFNIS)
head(dta)
names(dta)

## check province for brussels
head(
  subset(
    dta,
    subset = TX_RGN_DESCR_NL == "Brussels Hoofdstedelijk Gewest",
    select = TX_PROV_DESCR_NL))

## .. replace empty province by region of BXL
dta$CD_PROV_REFNIS[dta$TX_RGN_DESCR_NL == "Brussels Hoofdstedelijk Gewest"] <- 4000

## check munty (empty refnis)
which(is.na(dta$CD_MUNTY_REFNIS))
# --> solution
## merge CD_REFNIS and CD_MUNTY_REFNIS
dta$CD_MUNTY_REFNIS <-
  coalesce(dta$CD_MUNTY_REFNIS, dta$CD_REFNIS)

## extract variables
dta <-
  subset(
    dta,
    select = c(
      "CD_MUNTY_REFNIS",
      "CD_DSTR_REFNIS",
      "CD_PROV_REFNIS",
      "CD_RGN_REFNIS",
      "CD_AGE",
      "CD_SEX",
      "CD_YEAR",
      "MS_POPULATION"
    )
  )

nuts <-
  subset(
    nuts,
    select = c(
      "CD_MUNTY_REFNIS",
      "TX_DESCR_DE",
      "TX_DESCR_EN",
      "TX_DESCR_FR",
      "TX_DESCR_NL"
    )
  )

nuts <- rename(nuts, CD_REFNIS = CD_MUNTY_REFNIS)
nuts$CD_REFNIS <- as.numeric(nuts$CD_REFNIS)

## rename variables
dta <- rename(dta,
              CD_ARRD_REFNIS = CD_DSTR_REFNIS)

## .. remove CD_, MS_ and TX_
colnames(dta) <-
  gsub(pattern = "CD_|TX_|MS_",
       x = colnames(dta),
       replacement = "")

colnames(nuts) <-
  gsub(pattern = "CD_|TX_|MS_",
       x = colnames(nuts),
       replacement = "")

## .. create age-groups
## .. .. 5-year age band
age <-
  c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
    "70-74", "75-79", "80-84", "85-89", "90-94", "95+")
agei <- c(0, 1, seq(5, 95, 5), Inf)
dta$AGE5 <- cut(dta$AGE, breaks = agei, labels = age, include.lowest = TRUE, right = FALSE)

## .. .. 10-year age band
age <-
  c("<10", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79",
    "80-89", "90+")
agei <- c(0, seq(10, 90, 10), Inf)
dta$AGE10 <- cut(dta$AGE, breaks = agei, labels = age, include.lowest = TRUE, right = FALSE)

## .. create factors
dta$AGE <- factor(dta$AGE)
dta$SEX <- factor(dta$SEX, c("M", "F"))

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## .. AGGREGATE DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## aggregate by different geographical levels
## .. MUNTY
munty_age_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS +
    # ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX + AGE + AGE5 + AGE10,
  FUN = sum
)

munty_age <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS +
    # ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    AGE + AGE5 + AGE10,
  FUN = sum
)
munty_age$SEX <- "MF"

munty_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS +
    # ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX,
  FUN = sum
)
munty_sex$AGE <- "ALL"
munty_sex$AGE5 <- "ALL"
munty_sex$AGE10 <- "ALL"

munty <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS,
    # ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR,
    FUN = sum
)
munty$SEX <- "MF"
munty$AGE <- "ALL"
munty$AGE5 <- "ALL"
munty$AGE10 <- "ALL"

BE_POP_MUNTY <- bind_rows(munty, munty_age, munty_age_sex, munty_sex)
BE_POP_MUNTY <- rename(BE_POP_MUNTY, REFNIS = MUNTY_REFNIS)

## .. .. map description
BE_POP_MUNTY <- left_join(BE_POP_MUNTY, nuts, by = "REFNIS")
sum(is.na(BE_POP_MUNTY$DESCR_EN))

## .. ARRD
arrd_age_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX + AGE + AGE5 + AGE10,
  FUN = sum
)

arrd_age <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    AGE + AGE5 + AGE10,
  FUN = sum
)
arrd_age$SEX <- "MF"

arrd_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX,
  FUN = sum
)
arrd_sex$AGE <- "ALL"
arrd_sex$AGE5 <- "ALL"
arrd_sex$AGE10 <- "ALL"

arrd <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS,
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR,
    FUN = sum
)
arrd$SEX <- "MF"
arrd$AGE <- "ALL"
arrd$AGE5 <- "ALL"
arrd$AGE10 <- "ALL"

BE_POP_ARRD <- bind_rows(arrd, arrd_age, arrd_age_sex, arrd_sex)
BE_POP_ARRD <- rename(BE_POP_ARRD, REFNIS = ARRD_REFNIS)

## .. .. map description
BE_POP_ARRD <- left_join(BE_POP_ARRD, nuts, by = "REFNIS")
sum(is.na(BE_POP_ARRD$DESCR_EN))

## .. PROV
prov_age_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX + AGE + AGE5 + AGE10,
  FUN = sum
)

prov_age <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    AGE + AGE5 + AGE10,
  FUN = sum
)
prov_age$SEX <- "MF"

prov_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX,
  FUN = sum
)
prov_sex$AGE <- "ALL"
prov_sex$AGE5 <- "ALL"
prov_sex$AGE10 <- "ALL"

prov <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS,
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR,
    FUN = sum
)
prov$SEX <- "MF"
prov$AGE <- "ALL"
prov$AGE5 <- "ALL"
prov$AGE10 <- "ALL"

BE_POP_PROV <- bind_rows(prov, prov_age, prov_age_sex, prov_sex)
BE_POP_PROV <- rename(BE_POP_PROV, REFNIS = PROV_REFNIS)

## .. .. map description
BE_POP_PROV <- left_join(BE_POP_PROV, nuts, by = "REFNIS")
sum(is.na(BE_POP_PROV$DESCR_EN))

## .. RGN
rgn_age_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS + SEX + AGE + AGE5 + AGE10,
  FUN = sum
)

rgn_age <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS + AGE + AGE5 + AGE10,
  FUN = sum
)
rgn_age$SEX <- "MF"

rgn_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS + SEX,
  FUN = sum
)
rgn_sex$AGE <- "ALL"
rgn_sex$AGE5 <- "ALL"
rgn_sex$AGE10 <- "ALL"

rgn <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS,
  FUN = sum
)
rgn$SEX <- "MF"
rgn$AGE <- "ALL"
rgn$AGE5 <- "ALL"
rgn$AGE10 <- "ALL"

BE_POP_RGN <- bind_rows(rgn, rgn_age, rgn_age_sex, rgn_sex)
BE_POP_RGN <- rename(BE_POP_RGN, REFNIS = RGN_REFNIS)

## .. .. map description
BE_POP_RGN <- left_join(BE_POP_RGN, nuts, by = "REFNIS")
sum(is.na(BE_POP_RGN$DESCR_EN))

## .. COMMUNITIES
## .. .. define the communities
dta$COMTY_REFNIS <- dta$RGN_REFNIS
dta$COMTY_REFNIS[dta$MUNTY_REFNIS %in% c(63001,63012,63087,63013,63023,63040,63048,63061,63067)] <- 9999 ## invent code

## .. .. add row to nuts
colnames(nuts)
nuts.gc <- tibble(
  REFNIS = 9999,
  DESCR_DE = "Deutschsprachige Gemeinschaft",
  DESCR_EN = "German-speaking Community",
  DESCR_FR = 'Communauté germanophone',
  DESCR_NL = 'Duitstalige Gemeenschap'
)
nuts <- bind_rows(nuts, nuts.gc)

## .. .. aggregate by new community levels
COMTY_age_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + COMTY_REFNIS + SEX + AGE + AGE5 + AGE10,
  FUN = sum
)

COMTY_age <- aggregate(
  data = dta,
  POPULATION ~ YEAR + COMTY_REFNIS + AGE + AGE5 + AGE10,
  FUN = sum
)
COMTY_age$SEX <- "MF"

COMTY_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + COMTY_REFNIS + SEX,
  FUN = sum
)
COMTY_sex$AGE <- "ALL"
COMTY_sex$AGE5 <- "ALL"
COMTY_sex$AGE10 <- "ALL"

COMTY <- aggregate(
  data = dta,
  POPULATION ~ YEAR + COMTY_REFNIS,
  FUN = sum
)
COMTY$SEX <- "MF"
COMTY$AGE <- "ALL"
COMTY$AGE5 <- "ALL"
COMTY$AGE10 <- "ALL"

BE_POP_COMTY <- bind_rows(COMTY, COMTY_age, COMTY_age_sex, COMTY_sex)
BE_POP_COMTY <- rename(BE_POP_COMTY, REFNIS = COMTY_REFNIS)

## .. .. map description
BE_POP_COMTY <- left_join(BE_POP_COMTY, nuts, by = "REFNIS")
sum(is.na(BE_POP_COMTY$DESCR_EN))

## .. BEL
be_age_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + SEX + AGE + AGE5 + AGE10,
  FUN = sum
)

be_age <- aggregate(
  data = dta,
  POPULATION ~ YEAR + AGE + AGE5 + AGE10,
  FUN = sum
)
be_age$SEX <- "MF"

be_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + SEX,
  FUN = sum
)
be_sex$AGE <- "ALL"
be_sex$AGE5 <- "ALL"
be_sex$AGE10 <- "ALL"

be <- aggregate(
  data = dta,
  POPULATION ~ YEAR,
  FUN = sum
)
be$SEX <- "MF"
be$AGE <- "ALL"
be$AGE5 <- "ALL"
be$AGE10 <- "ALL"

BE_POP <- bind_rows(be, be_age, be_age_sex, be_sex)
BE_POP$REFNIS <- 100
BE_POP$DESCR_DE <- "Belgien"
BE_POP$DESCR_EN <- "Belgium"
BE_POP$DESCR_NL <- "België"
BE_POP$DESCR_FR <- "Belgique"

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## SAVE DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## check if aggregation was correct (all number should be the same)
be %>% group_by(YEAR) %>% summarise(POPULATION = sum(POPULATION))
arrd %>% group_by(YEAR) %>% summarise(POPULATION = sum(POPULATION))
COMTY %>% group_by(YEAR) %>% summarise(POPULATION = sum(POPULATION))
munty %>% group_by(YEAR) %>% summarise(POPULATION = sum(POPULATION))
rgn %>% group_by(YEAR) %>% summarise(POPULATION = sum(POPULATION))


## save the results
saveRDS(BE_POP, "inst/extdata/BE_POP.rds")
saveRDS(BE_POP_MUNTY, "inst/extdata/BE_POP_MUNTY.rds")
saveRDS(BE_POP_PROV, "inst/extdata/BE_POP_PROV.rds")
saveRDS(BE_POP_RGN, "inst/extdata/BE_POP_RGN.rds")
saveRDS(BE_POP_COMTY, "inst/extdata/BE_POP_COMTY.rds")
