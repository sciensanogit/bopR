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

###
# ZIPFILES (from 2009)
###

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

###
# XLSX (before 2009)
###

## read-in and extract data
ls <-
  list.files(path = "inst/extdata/before-2009/",
             pattern = ".xlsx",
             full.names = TRUE)
dta.xlsx <- tibble()
i <- 1

for (fxlsx in ls) {
  print(paste0(i, "/", length(ls)))
  ## read in the data
  ftmp <-
    openxlsx::read.xlsx(
      xlsxFile = fxlsx,
      sheet = 3
    )
  ## rename column with year
  colnames(ftmp) <-
    gsub(pattern = "Population.on.January.1st.", x = colnames(ftmp), replacement = "")

  ## cbind data
  if (i == 1) {
    dta.xlsx <- ftmp
  } else {
    dta.xlsx <- full_join(dta.xlsx, ftmp)
  }

  ## add +1
  i <- i + 1
}

## check if colnames of dta.xlsx are unique
TRUE %in% duplicated(colnames(dta.xlsx))

## read-in data for NUTS names and NIS
nuts <- openxlsx::read.xlsx("inst/extdata/TU_COM_NUTS_LAU-20190101.xlsx")

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

## restructure
## .. years
dta.xlsx <- dta.xlsx %>%
  pivot_longer(cols = matches("1|2", perl = TRUE), names_to = "CD_YEAR") %>%
  rename(
    "MS_POPULATION" = "value",
    "CD_AGE" = "Age",
    "CD_SEX" = "Gender"
    ) %>%
  select(c(-All.Ages, -Age.Class, -Mens.and.womens))

## .. add community
dta.xlsx <-
  left_join(dta.xlsx, nuts, by = c("Municipality" = "TX_DESCR_DE"))
dta.xlsx$Community <- dta.xlsx$Region
dta.xlsx$Community[dta.xlsx$CD_REFNIS %in% c(63001,63012,63087,63013,63023,63040,63048,63061,63067)] <- "German-speaking Community"

## .. only keep community variable
dta.xlsx <- dta.xlsx[c("CD_AGE", "CD_SEX", "Region", "Province", "Administrative.District",
           "Municipality", "Community", "CD_YEAR", "MS_POPULATION")]

## .. regions
dta.xlsx <- dta.xlsx %>%
  pivot_longer(cols = c("Community" ,"Region", "Province", "Administrative.District", "Municipality"),
               values_to = "TX_DESCR_EN", names_to = "LEVEL")

## refactor
dta.xlsx$LEVEL <-
  factor(
    dta.xlsx$LEVEL,
    levels = unique(dta.xlsx$LEVEL),
    labels = c("TX_COMTY_DESCR", "TX_RGN_DESCR", "TX_PROV_DESCR", "TX_ADM_DSTR_DESCR", "TX_MUNTY_DESCR"))

dta.xlsx$CD_SEX <-
  factor(
    dta.xlsx$CD_SEX,
    levels = unique(dta.xlsx$CD_SEX),
    labels = c("F", "M")
  )

dta.xlsx$CD_AGE <-
  factor(
    dta.xlsx$CD_AGE,
    levels = unique(dta.xlsx$CD_AGE),
    labels = 0:100
  )

## join data with nuts
dta.xlsx <-
  left_join(dta.xlsx, nuts)

## aggregate the data
dta.xlsx <- aggregate(
  MS_POPULATION ~ CD_AGE + CD_SEX + CD_YEAR + LEVEL + TX_DESCR_EN +
    CD_REFNIS + TX_DESCR_DE + TX_DESCR_FR + TX_DESCR_NL,
  data = dta.xlsx, FUN = sum)


## save the combined result
saveRDS(object = dta.xlsx,
        file = sprintf("inst/extdata/statbel-data-pre2009-combined-%s-%s.rds", min(dta.xlsx$CD_YEAR), max(dta.xlsx$CD_YEAR)))

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## .. LOAD DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## load prepared data with population data
dta <- readRDS("inst/extdata/statbel-data-combined-2009-2022.rds")
dta.pre2019 <- readRDS("inst/extdata/statbel-data-pre2009-combined-1992-2009.rds")

## read-in data for NUTS names and NIS
nuts <- openxlsx::read.xlsx("inst/extdata/TU_COM_NUTS_LAU-20190101.xlsx")

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

###
# DTA
###

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

## rename variables
dta <- rename(dta,
              CD_ARRD_REFNIS = CD_DSTR_REFNIS)

###
# DTA.PRE2019
###

## change structure AGE
dta.pre2019$CD_AGE <- as.numeric(as.character(dta.pre2019$CD_AGE))

## check if BRUSSELS in provinces
unique(subset(dta.pre2019, subset = LEVEL == "TX_PROV_DESCR")$TX_DESCR_EN)

## .. add brussels to provinces
unique(subset(dta.pre2019, subset = LEVEL == "TX_RGN_DESCR")$TX_DESCR_EN)
dta.pre2019.bxl <-
  subset(dta.pre2019, subset = TX_DESCR_EN == "Brussels-Capital region")
dta.pre2019.bxl$LEVEL <- "TX_PROV_DESCR"
dta.pre2019 <-
  bind_rows(dta.pre2019, dta.pre2019.bxl)

## .. rename/change some vars
dta.pre2019$CD_YEAR <- as.numeric(dta.pre2019$CD_YEAR)

###
# ALL
###

## .. remove CD_, MS_ and TX_
colnames(dta) <-
  gsub(pattern = "CD_|TX_|MS_",
       x = colnames(dta),
       replacement = "")

colnames(nuts) <-
  gsub(pattern = "CD_|TX_|MS_",
       x = colnames(nuts),
       replacement = "")

colnames(dta.pre2019) <-
  gsub(pattern = "CD_|TX_|MS_",
       x = colnames(dta.pre2019),
       replacement = "")

## .. create age-groups
## .. .. 5-year age band
age <-
  c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
    "70-74", "75-79", "80-84", "85-89", "90-94", "95+")
agei <- c(0, 1, seq(5, 95, 5), Inf)
dta$AGE5 <- cut(dta$AGE, breaks = agei, labels = age, include.lowest = TRUE, right = FALSE)
dta.pre2019$AGE5 <- cut(dta.pre2019$AGE, breaks = agei, labels = age, include.lowest = TRUE, right = FALSE)

## .. .. 10-year age band
age <-
  c("<10", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79",
    "80-89", "90+")
agei <- c(0, seq(10, 90, 10), Inf)
dta$AGE10 <- cut(dta$AGE, breaks = agei, labels = age, include.lowest = TRUE, right = FALSE)
dta.pre2019$AGE10 <- cut(dta.pre2019$AGE, breaks = agei, labels = age, include.lowest = TRUE, right = FALSE)

## .. create factors
dta$AGE <- factor(dta$AGE)
dta.pre2019$AGE <- factor(dta.pre2019$AGE)
dta$SEX <- factor(dta$SEX, c("M", "F"))
dta.pre2019$SEX <- factor(dta.pre2019$SEX, c("M", "F"))

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## .. AGGREGATE DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## aggregate by different geographical levels
####
## .. MUNTY
####
munty_age_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS +
    # ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX + AGE + AGE5 + AGE10,
  FUN = sum
)
munty_age_sex.2 <- subset(
  dta.pre2019,
  subset = YEAR < 2009 & LEVEL == "TX_MUNTY_DESCR")

munty_age.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS +
    # ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    AGE + AGE5 + AGE10,
  FUN = sum
)
munty_age.2 <- aggregate(
  data = munty_age_sex.2,
  POPULATION ~ YEAR + REFNIS +
    AGE + AGE5 + AGE10 + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

munty_age.1$SEX <- "MF"
munty_age.2$SEX <- "MF"

munty_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS +
    # ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX,
  FUN = sum
)
munty_sex.2 <- aggregate(
  data = munty_age_sex.2,
  POPULATION ~ YEAR + REFNIS +
    SEX + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

munty_sex.1$AGE <- "ALL"
munty_sex.1$AGE5 <- "ALL"
munty_sex.1$AGE10 <- "ALL"

munty_sex.2$AGE <- "ALL"
munty_sex.2$AGE5 <- "ALL"
munty_sex.2$AGE10 <- "ALL"

munty.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS,
    # ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR,
    FUN = sum
)
munty.2 <- aggregate(
  data = munty_age_sex.2,
  POPULATION ~ YEAR + REFNIS + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

munty.1$SEX <- "MF"
munty.1$AGE <- "ALL"
munty.1$AGE5 <- "ALL"
munty.1$AGE10 <- "ALL"

munty.2$SEX <- "MF"
munty.2$AGE <- "ALL"
munty.2$AGE5 <- "ALL"
munty.2$AGE10 <- "ALL"

BE_POP_MUNTY.1 <- bind_rows(munty.1, munty_age.1, munty_age_sex.1, munty_sex.1)
BE_POP_MUNTY.1 <- rename(BE_POP_MUNTY.1, REFNIS = MUNTY_REFNIS)

## .. .. map description
BE_POP_MUNTY.1 <- left_join(BE_POP_MUNTY.1, nuts, by = "REFNIS")
BE_POP_MUNTY.1$YEAR <- as.numeric(BE_POP_MUNTY.1$YEAR)

## .. .. add with pre 2019
BE_POP_MUNTY.2 <- bind_rows(munty.2, munty_age.2, munty_age_sex.2, munty_sex.2)
BE_POP_MUNTY.2 <- drop_na(BE_POP_MUNTY.2)
BE_POP_MUNTY <- bind_rows(BE_POP_MUNTY.1, BE_POP_MUNTY.2)

sum(is.na(BE_POP_MUNTY$DESCR_EN))

####
## .. ARRD
####
arrd_age_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX + AGE + AGE5 + AGE10,
  FUN = sum
)

arrd_age_sex.2 <- subset(
  dta.pre2019,
  subset = YEAR < 2009 & LEVEL == "TX_ADM_DSTR_DESCR")

arrd_age.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    AGE + AGE5 + AGE10,
  FUN = sum
)
arrd_age.2 <- aggregate(
  data = arrd_age_sex.2,
  POPULATION ~ YEAR + REFNIS +
    AGE + AGE5 + AGE10 + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

arrd_age.1$SEX <- "MF"
arrd_age.2$SEX <- "MF"

arrd_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS +
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX,
  FUN = sum
)
arrd_sex.2 <- aggregate(
  data = arrd_age_sex.2,
  POPULATION ~ YEAR + REFNIS +
    SEX + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

arrd_sex.1$AGE <- "ALL"
arrd_sex.1$AGE5 <- "ALL"
arrd_sex.1$AGE10 <- "ALL"

arrd_sex.2$AGE <- "ALL"
arrd_sex.2$AGE5 <- "ALL"
arrd_sex.2$AGE10 <- "ALL"

arrd.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS,
    # PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR,
    FUN = sum
)
arrd.2 <- aggregate(
  data = arrd_age_sex.2,
  POPULATION ~ YEAR + REFNIS + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

arrd.1$SEX <- "MF"
arrd.1$AGE <- "ALL"
arrd.1$AGE5 <- "ALL"
arrd.1$AGE10 <- "ALL"

arrd.2$SEX <- "MF"
arrd.2$AGE <- "ALL"
arrd.2$AGE5 <- "ALL"
arrd.2$AGE10 <- "ALL"

BE_POP_ARRD.1 <- bind_rows(arrd.1, arrd_age.1, arrd_age_sex.1, arrd_sex.1)
BE_POP_ARRD.1 <- rename(BE_POP_ARRD.1, REFNIS = ARRD_REFNIS)

## .. .. map description
BE_POP_ARRD.1 <- left_join(BE_POP_ARRD.1, nuts, by = "REFNIS")
BE_POP_ARRD.1$YEAR <- as.numeric(BE_POP_ARRD.1$YEAR)

## .. .. add with pre 2019
BE_POP_ARRD.2 <- bind_rows(arrd.2, arrd_age.2, arrd_age_sex.2, arrd_sex.2)
BE_POP_ARRD.2 <- drop_na(BE_POP_ARRD.2)
BE_POP_ARRD <- bind_rows(BE_POP_ARRD.1, BE_POP_ARRD.2)

sum(is.na(BE_POP_ARRD$DESCR_EN))

####
## .. PROV
####
prov_age_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX + AGE + AGE5 + AGE10,
  FUN = sum
)
prov_age_sex.2 <- subset(
  dta.pre2019,
  subset = YEAR < 2009 & LEVEL == "TX_PROV_DESCR")

prov_age.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    AGE + AGE5 + AGE10,
  FUN = sum
)
prov_age.2 <- aggregate(
  data = prov_age_sex.2,
  POPULATION ~ YEAR + REFNIS +
    AGE + AGE5 + AGE10 + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

prov_age.1$SEX <- "MF"
prov_age.2$SEX <- "MF"

prov_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS +
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX,
  FUN = sum
)
prov_sex.2 <- aggregate(
  data = prov_age_sex.2,
  POPULATION ~ YEAR + REFNIS +
    SEX + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

prov_sex.1$AGE <- "ALL"
prov_sex.1$AGE5 <- "ALL"
prov_sex.1$AGE10 <- "ALL"

prov_sex.2$AGE <- "ALL"
prov_sex.2$AGE5 <- "ALL"
prov_sex.2$AGE10 <- "ALL"

prov.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS,
    # RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR,
    FUN = sum
)
prov.2 <- aggregate(
  data = prov_age_sex.2,
  POPULATION ~ YEAR + REFNIS + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

prov.1$SEX <- "MF"
prov.1$AGE <- "ALL"
prov.1$AGE5 <- "ALL"
prov.1$AGE10 <- "ALL"

prov.2$SEX <- "MF"
prov.2$AGE <- "ALL"
prov.2$AGE5 <- "ALL"
prov.2$AGE10 <- "ALL"

BE_POP_PROV.1 <- bind_rows(prov.1, prov_age.1, prov_age_sex.1, prov_sex.1)
BE_POP_PROV.1 <- rename(BE_POP_PROV.1, REFNIS = PROV_REFNIS)

## .. .. map description
BE_POP_PROV.1 <- left_join(BE_POP_PROV.1, nuts, by = "REFNIS")
BE_POP_PROV.1$YEAR <- as.numeric(BE_POP_PROV.1$YEAR)

## .. .. add with pre 2019
BE_POP_PROV.2 <- bind_rows(prov.2, prov_age.2, prov_age_sex.2, prov_sex.2)
BE_POP_PROV.2 <- drop_na(BE_POP_PROV.2)
BE_POP_PROV <- bind_rows(BE_POP_PROV.1, BE_POP_PROV.2)

sum(is.na(BE_POP_PROV$DESCR_EN))

####
## .. RGN
####
rgn_age_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS + SEX + AGE + AGE5 + AGE10,
  FUN = sum
)
rgn_age_sex.2 <- subset(
  dta.pre2019,
  subset = YEAR < 2009 & LEVEL == "TX_RGN_DESCR", select = -LEVEL)

rgn_age.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS + AGE + AGE5 + AGE10,
  FUN = sum
)
rgn_age.2 <- aggregate(
  data = rgn_age_sex.2,
  POPULATION ~ YEAR + REFNIS +
    AGE + AGE5 + AGE10 + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

rgn_age.1$SEX <- "MF"
rgn_age.2$SEX <- "MF"

rgn_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS + SEX,
  FUN = sum
)
rgn_sex.2 <- aggregate(
  data = rgn_age_sex.2,
  POPULATION ~ YEAR + REFNIS +
    SEX + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

rgn_sex.1$AGE <- "ALL"
rgn_sex.1$AGE5 <- "ALL"
rgn_sex.1$AGE10 <- "ALL"

rgn_sex.2$AGE <- "ALL"
rgn_sex.2$AGE5 <- "ALL"
rgn_sex.2$AGE10 <- "ALL"

rgn.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS,
  FUN = sum
)
rgn.2 <- aggregate(
  data = rgn_age_sex.2,
  POPULATION ~ YEAR + REFNIS + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

rgn.1$SEX <- "MF"
rgn.1$AGE <- "ALL"
rgn.1$AGE5 <- "ALL"
rgn.1$AGE10 <- "ALL"

rgn.2$SEX <- "MF"
rgn.2$AGE <- "ALL"
rgn.2$AGE5 <- "ALL"
rgn.2$AGE10 <- "ALL"

BE_POP_RGN.1 <- bind_rows(rgn.1, rgn_age.1, rgn_age_sex.1, rgn_sex.1)
BE_POP_RGN.1 <- rename(BE_POP_RGN.1, REFNIS = RGN_REFNIS)

## .. .. map description
BE_POP_RGN.1 <- left_join(BE_POP_RGN.1, nuts, by = "REFNIS")
BE_POP_RGN.1$YEAR <- as.numeric(BE_POP_RGN.1$YEAR)

## .. .. add with pre 2019
BE_POP_RGN.2 <- bind_rows(rgn.2, rgn_age.2, rgn_age_sex.2, rgn_sex.2)
BE_POP_RGN.2 <- drop_na(BE_POP_RGN.2)
BE_POP_RGN <- bind_rows(BE_POP_RGN.1, BE_POP_RGN.2)

sum(is.na(BE_POP_RGN$DESCR_EN))

####
## .. COMMUNITIES
####
## .. .. define the communities
## .. .. (a) from 2009
dta$COMTY_REFNIS <- dta$RGN_REFNIS
dta$COMTY_REFNIS[dta$MUNTY_REFNIS %in% c(63001,63012,63087,63013,63023,63040,63048,63061,63067)] <- 9999 ## invent code
## .. .. (b) before 2009
dta.pre2019.comty <-
  subset(
    dta.pre2019,
    subset = YEAR < 2009 & LEVEL == "TX_COMTY_DESCR", select = -LEVEL)
dta.pre2019.comty$REFNIS[dta.pre2019.comty$DESCR_EN == "German-speaking Community"] <- 9999 ## invent code
dta.pre2019.comty$DESCR_DE[dta.pre2019.comty$REFNIS %in% c(63001,63012,63087,63013,63023,63040,63048,63061,63067)] <- "Deutschsprachige Gemeinschaft"
dta.pre2019.comty$DESCR_EN[dta.pre2019.comty$REFNIS %in% c(63001,63012,63087,63013,63023,63040,63048,63061,63067)] <- "German-speaking Community"
dta.pre2019.comty$DESCR_FR[dta.pre2019.comty$REFNIS %in% c(63001,63012,63087,63013,63023,63040,63048,63061,63067)] <- 'Communauté germanophone'
dta.pre2019.comty$DESCR_NL[dta.pre2019.comty$REFNIS %in% c(63001,63012,63087,63013,63023,63040,63048,63061,63067)] <- 'Duitstalige Gemeenschap' ## invent code

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
COMTY_age_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + COMTY_REFNIS + SEX + AGE + AGE5 + AGE10,
  FUN = sum
)
COMTY_age_sex.2 <- dta.pre2019.comty

COMTY_age.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + COMTY_REFNIS + AGE + AGE5 + AGE10,
  FUN = sum
)
COMTY_age.2 <- aggregate(
  data = COMTY_age_sex.2,
  POPULATION ~ YEAR + REFNIS +
    AGE + AGE5 + AGE10 + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

COMTY_age.1$SEX <- "MF"
COMTY_age.2$SEX <- "MF"

COMTY_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + COMTY_REFNIS + SEX,
  FUN = sum
)
COMTY_sex.2 <- aggregate(
  data = COMTY_age_sex.2,
  POPULATION ~ YEAR + REFNIS +
    SEX + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

COMTY_sex.1$AGE <- "ALL"
COMTY_sex.1$AGE5 <- "ALL"
COMTY_sex.1$AGE10 <- "ALL"

COMTY_sex.2$AGE <- "ALL"
COMTY_sex.2$AGE5 <- "ALL"
COMTY_sex.2$AGE10 <- "ALL"

COMTY.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + COMTY_REFNIS,
  FUN = sum
)
COMTY.2 <- aggregate(
  data = COMTY_age_sex.2,
  POPULATION ~ YEAR + REFNIS + DESCR_EN + DESCR_NL + DESCR_FR + DESCR_DE,
  FUN = sum
)

COMTY.1$SEX <- "MF"
COMTY.1$AGE <- "ALL"
COMTY.1$AGE5 <- "ALL"
COMTY.1$AGE10 <- "ALL"

COMTY.2$SEX <- "MF"
COMTY.2$AGE <- "ALL"
COMTY.2$AGE5 <- "ALL"
COMTY.2$AGE10 <- "ALL"

BE_POP_COMTY.1 <- bind_rows(COMTY.1, COMTY_age.1, COMTY_age_sex.1, COMTY_sex.1)
BE_POP_COMTY.1 <- rename(BE_POP_COMTY.1, REFNIS = COMTY_REFNIS)

## .. .. map description
BE_POP_COMTY.1 <- left_join(BE_POP_COMTY.1, nuts, by = "REFNIS")
BE_POP_COMTY.1$YEAR <- as.numeric(BE_POP_COMTY.1$YEAR)

## .. .. add with pre 2019
BE_POP_COMTY.2 <- bind_rows(COMTY.2, COMTY_age.2, COMTY_age_sex.2, COMTY_sex.2)
BE_POP_COMTY.2 <- drop_na(BE_POP_COMTY.2)
BE_POP_COMTY <- bind_rows(BE_POP_COMTY.1, BE_POP_COMTY.2)
sum(is.na(BE_POP_COMTY$DESCR_EN))

####
## .. BEL
####
be_age_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + SEX + AGE + AGE5 + AGE10,
  FUN = sum
)
be_age_sex.2 <-
  aggregate(
    data = dta.pre2019,
    POPULATION ~ YEAR + SEX + AGE + AGE5 + AGE10,
    FUN = sum,
    subset = YEAR < 2009 & LEVEL == "TX_RGN_DESCR"
  )

be_age.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + AGE + AGE5 + AGE10,
  FUN = sum
)
be_age.2 <-
  aggregate(
    data = dta.pre2019,
    POPULATION ~ YEAR + AGE + AGE5 + AGE10,
    FUN = sum,
    subset = YEAR < 2009 & LEVEL == "TX_RGN_DESCR"
  )

be_age.1$SEX <- "MF"
be_age.2$SEX <- "MF"

be_sex.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR + SEX,
  FUN = sum
)
be_sex.2 <-
  aggregate(
    data = dta.pre2019,
    POPULATION ~ YEAR + SEX,
    FUN = sum,
    subset = YEAR < 2009 & LEVEL == "TX_RGN_DESCR"
  )

be_sex.1$AGE <- "ALL"
be_sex.1$AGE5 <- "ALL"
be_sex.1$AGE10 <- "ALL"

be_sex.2$AGE <- "ALL"
be_sex.2$AGE5 <- "ALL"
be_sex.2$AGE10 <- "ALL"

be.1 <- aggregate(
  data = dta,
  POPULATION ~ YEAR,
  FUN = sum
)
be.2 <-
  aggregate(
    data = dta.pre2019,
    POPULATION ~ YEAR,
    FUN = sum,
    subset = YEAR < 2009 & LEVEL == "TX_RGN_DESCR"
  )

be.1$SEX <- "MF"
be.1$AGE <- "ALL"
be.1$AGE5 <- "ALL"
be.1$AGE10 <- "ALL"

be.2$SEX <- "MF"
be.2$AGE <- "ALL"
be.2$AGE5 <- "ALL"
be.2$AGE10 <- "ALL"

BE_POP.1 <- bind_rows(be.1, be_age.1, be_age_sex.1, be_sex.1)
BE_POP.1$YEAR <- as.numeric(BE_POP.1$YEAR)
BE_POP.2 <- bind_rows(be.2, be_age.2, be_age_sex.2, be_sex.2)
BE_POP <- bind_rows(BE_POP.1, BE_POP.2)
BE_POP$REFNIS <- 100
BE_POP$DESCR_DE <- "Belgien"
BE_POP$DESCR_EN <- "Belgium"
BE_POP$DESCR_NL <- "België"
BE_POP$DESCR_FR <- "Belgique"

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## SAVE DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## check if aggregation was correct (all number should be the same)
BE_POP %>% filter(AGE == "ALL", SEX == "MF") %>% group_by(YEAR) %>% summarise(POPULATION = sum(POPULATION))
BE_POP_COMTY %>%  filter(AGE == "ALL", SEX == "MF") %>% group_by(YEAR) %>% summarise(POPULATION = sum(POPULATION))
BE_POP_MUNTY %>%  filter(AGE == "ALL", SEX == "MF") %>% group_by(YEAR) %>% summarise(POPULATION = sum(POPULATION))
BE_POP_ARRD %>%  filter(AGE == "ALL", SEX == "MF") %>% group_by(YEAR) %>% summarise(POPULATION = sum(POPULATION))
BE_POP_RGN %>%  filter(AGE == "ALL", SEX == "MF") %>% group_by(YEAR) %>% summarise(POPULATION = sum(POPULATION))


## save the results
saveRDS(BE_POP, "inst/extdata/BE_POP.rds")
saveRDS(BE_POP_MUNTY, "inst/extdata/BE_POP_MUNTY.rds")
saveRDS(BE_POP_PROV, "inst/extdata/BE_POP_PROV.rds")
saveRDS(BE_POP_RGN, "inst/extdata/BE_POP_RGN.rds")
saveRDS(BE_POP_COMTY, "inst/extdata/BE_POP_COMTY.rds")
