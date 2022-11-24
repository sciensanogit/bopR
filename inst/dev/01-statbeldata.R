##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  01-main.R
##  Purpose: load and update statbel population data
##  Author: R HACKATHON
##  Date: 14-06-2022
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
## .. READ DATA ####
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
  unzip(zipfile = fzip, exdir = "inst/extdata/")
  ## read in the data
  ftmp <-
    read.table(
      file = paste0("inst/extdata/", fname),
      sep = "|",
      header = TRUE
    )
  ## merge dta
  ftmp$CD_YEAR <- year
  dta <- bind_rows(dta, ftmp)
  ## remove file
  rm(ftmp)
  file.remove(paste0("inst/extdata/", fname))
  ## add +1
  i <- i + 1
}

## save the combined result
saveRDS(object = dta, file = "inst/extdata/statbel_data_combined.RDS")

## load prepared data
dta <- readRDS("inst/extdata/statbel_data_combined.RDS")

## check the dta
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
dta$TX_PROV_DESCR_NL[dta$TX_RGN_DESCR_NL == "Brussels Hoofdstedelijk Gewest"] <- "Brussels Hoofdstedelijk Gewest"
dta$TX_PROV_DESCR_FR[dta$TX_RGN_DESCR_NL == "Brussels Hoofdstedelijk Gewest"] <- "Région de Bruxelles-Capitale"
dta$CD_PROV_REFNIS[dta$TX_RGN_DESCR_NL == "Brussels Hoofdstedelijk Gewest"] <- 4000

## check munty
unique(dta$TX_MUNTY_DESCR_NL)
which(is.na(dta$TX_MUNTY_DESCR_NL)) ## some are NA (replace by "Unknown")
which(is.na(dta$CD_MUNTY_REFNIS)) ## some are NA (replace by 99999)
dta$CD_MUNTY_REFNIS[is.na(dta$CD_MUNTY_REFNIS)] <- 99999
dta$TX_MUNTY_DESCR_NL[is.na(dta$TX_MUNTY_DESCR_NL)] <- "Unknown"
dta$TX_MUNTY_DESCR_FR[is.na(dta$TX_MUNTY_DESCR_FR)] <- "Unknown"

## merge CD_REFNIS and CD_MUNTY_REFNIS
dta$CD_MUNTY_REFNIS <-
  coalesce(dta$CD_MUNTY_REFNIS, dta$CD_MUNTY_REFNIS)
dta$TX_MUNTY_DESCR_NL <-
  coalesce(dta$TX_MUNTY_DESCR_NL, dta$TX_DESCR_NL)
dta$TX_MUNTY_DESCR_FR <-
  coalesce(dta$TX_MUNTY_DESCR_FR, dta$TX_DESCR_FR)

## extract variables
dta <-
  subset(
    dta,
    select = c(
      "CD_MUNTY_REFNIS",
      "TX_MUNTY_DESCR_NL",
      "TX_MUNTY_DESCR_FR",
      "CD_DSTR_REFNIS",
      "TX_ADM_DSTR_DESCR_NL",
      "TX_ADM_DSTR_DESCR_FR",
      "CD_PROV_REFNIS",
      "TX_PROV_DESCR_NL",
      "TX_PROV_DESCR_FR",
      "CD_RGN_REFNIS",
      "TX_RGN_DESCR_NL",
      "TX_RGN_DESCR_FR",
      "CD_AGE",
      "CD_SEX",
      "CD_YEAR",
      "MS_POPULATION"
    )
  )

## rename variables
dta <- rename(dta,
              TX_ARRD_DESCR_NL = TX_ADM_DSTR_DESCR_NL,
              TX_ARRD_DESCR_FR = TX_ADM_DSTR_DESCR_FR,
              CD_ARRD_REFNIS = CD_DSTR_REFNIS)

## .. remove CD_, MS_ and TX_
colnames(dta) <-
  gsub(pattern = "CD_|TX_|MS_",
       x = colnames(dta),
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

## translate of non-ASCII characters
dta$MUNTY_DESCR_NL <-
  stringi::stri_trans_general(dta$MUNTY_DESCR_NL, "latin-ascii")
dta$MUNTY_DESCR_FR <-
  stringi::stri_trans_general(dta$MUNTY_DESCR_FR, "latin-ascii")
dta$ARRD_DESCR_NL <-
  stringi::stri_trans_general(dta$ARRD_DESCR_NL, "latin-ascii")
dta$ARRD_DESCR_FR <-
  stringi::stri_trans_general(dta$ARRD_DESCR_FR, "latin-ascii")
dta$PROV_DESCR_NL <-
  stringi::stri_trans_general(dta$PROV_DESCR_NL, "latin-ascii")
dta$PROV_DESCR_FR <-
  stringi::stri_trans_general(dta$PROV_DESCR_FR, "latin-ascii")
dta$RGN_DESCR_NL <-
  stringi::stri_trans_general(dta$RGN_DESCR_NL, "latin-ascii")
dta$RGN_DESCR_FR <-
  stringi::stri_trans_general(dta$RGN_DESCR_FR, "latin-ascii")

## add abbrevation for region
dta$RGN_ABBR <- factor(dta$RGN_DESCR_NL,
                       c("Brussels Hoofdstedelijk Gewest",
                         "Vlaams Gewest",
                         "Waals Gewest"),
                       c("BR", "FL", "WA"))

which(is.na(dta$RGN_ABBR))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## .. AGGREGATE DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## calculate summaries
## .. MUNTY
munty_age_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS + MUNTY_DESCR_NL + MUNTY_DESCR_FR +
    ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX + AGE + AGE5 + AGE10,
  FUN = sum
)

munty_age <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS + MUNTY_DESCR_NL + MUNTY_DESCR_FR +
    ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    AGE + AGE5 + AGE10,
  FUN = sum
)
munty_age$SEX <- "MF"

munty_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS + MUNTY_DESCR_NL + MUNTY_DESCR_FR +
    ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX,
  FUN = sum
)
munty_sex$AGE <- "ALL"
munty_sex$AGE5 <- "ALL"
munty_sex$AGE10 <- "ALL"

munty <- aggregate(
  data = dta,
  POPULATION ~ YEAR + MUNTY_REFNIS + MUNTY_DESCR_NL + MUNTY_DESCR_FR +
    ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR,
    FUN = sum
)
munty$SEX <- "MF"
munty$AGE <- "ALL"
munty$AGE5 <- "ALL"
munty$AGE10 <- "ALL"

BE_POP_MUNTY <- bind_rows(munty, munty_age, munty_age_sex, munty_sex)

## .. ARRD
arrd_age_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX + AGE + AGE5 + AGE10,
  FUN = sum
)

arrd_age <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    AGE + AGE5 + AGE10,
  FUN = sum
)
arrd_age$SEX <- "MF"

arrd_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX,
  FUN = sum
)
arrd_sex$AGE <- "ALL"
arrd_sex$AGE5 <- "ALL"
arrd_sex$AGE10 <- "ALL"

arrd <- aggregate(
  data = dta,
  POPULATION ~ YEAR + ARRD_REFNIS + ARRD_DESCR_NL + ARRD_DESCR_FR +
    PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR,
    FUN = sum
)
arrd$SEX <- "MF"
arrd$AGE <- "ALL"
arrd$AGE5 <- "ALL"
arrd$AGE10 <- "ALL"

BE_POP_ARRD <- bind_rows(arrd, arrd_age, arrd_age_sex, arrd_sex)

## .. PROV
prov_age_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX + AGE + AGE5 + AGE10,
  FUN = sum
)

prov_age <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    AGE + AGE5 + AGE10,
  FUN = sum
)
prov_age$SEX <- "MF"

prov_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR +
    SEX,
  FUN = sum
)
prov_sex$AGE <- "ALL"
prov_sex$AGE5 <- "ALL"
prov_sex$AGE10 <- "ALL"

prov <- aggregate(
  data = dta,
  POPULATION ~ YEAR + PROV_REFNIS + PROV_DESCR_NL + PROV_DESCR_FR +
    RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR,
    FUN = sum
)
prov$SEX <- "MF"
prov$AGE <- "ALL"
prov$AGE5 <- "ALL"
prov$AGE10 <- "ALL"

BE_POP_PROV <- bind_rows(prov, prov_age, prov_age_sex, prov_sex)

## .. RGN
rgn_age_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR + SEX + AGE + AGE5 + AGE10,
  FUN = sum
)

rgn_age <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR + AGE + AGE5 + AGE10,
  FUN = sum
)
rgn_age$SEX <- "MF"

rgn_sex <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR + SEX,
  FUN = sum
)
rgn_sex$AGE <- "ALL"
rgn_sex$AGE5 <- "ALL"
rgn_sex$AGE10 <- "ALL"

rgn <- aggregate(
  data = dta,
  POPULATION ~ YEAR + RGN_REFNIS + RGN_DESCR_NL + RGN_DESCR_FR + RGN_ABBR,
  FUN = sum
)
rgn$SEX <- "MF"
rgn$AGE <- "ALL"
rgn$AGE5 <- "ALL"
rgn$AGE10 <- "ALL"

BE_POP_RGN <- bind_rows(rgn, rgn_age, rgn_age_sex, rgn_sex)

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

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## PROJECTIONS ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## .. READ DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## read-in and extract data
ls <-
  list.files(path = "inst/extdata/",
             pattern = ".xlsx",
             full.names = TRUE)
dta <- list()
i <- 1

for (fxlsx in ls) {
  print(paste0(i, "/", length(ls)))
  ## read in the data
  ftmp <- list()
  ftmp$wd <- openxlsx::loadWorkbook(fxlsx)
  ftmp$sheets <- openxlsx::sheets(ftmp$wd)[-1]
  ftmp$dta <- tibble()

  ## loop over different sheets
  for (sh in seq_along(ftmp$sheets)) {
    ftmp$xlsx <-
      openxlsx::read.xlsx(
        xlsxFile = ftmp$wd,
        sheet = ftmp$sheets[sh],
        startRow = 4,
        colNames = TRUE
      )
    ## delete NA rows at column2 and column1
    id <- is.na(ftmp$xlsx[,2]) | is.na(ftmp$xlsx[,1])
    ## remove these rows
    ftmp$xlsx <- ftmp$xlsx[!id,]
    ## check uniques in age and add sex
    colnames(ftmp$xlsx)[1] <- "AGE"
    ftmp$xlsx$AGE <- as.numeric(gsub(pattern = " jaar| jaar en meer", replacement = "", x = ftmp$xlsx$AGE))

    ftmp$xlsx$SEX <- rep(c("MF", "M", "F"), each = length(unique(ftmp$xlsx$AGE)))
    ## add region
    ftmp$xlsx$REGION <- ftmp$sheets[sh]
    ## pivot_longer
    ftmp$xlsx <- pivot_longer(ftmp$xlsx, cols = c(-SEX, -AGE, -REGION), names_to = "YEAR", values_to = "POPULATION")
    ## add agegroups
    ## .. .. 5-year age band
    age <-
      c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
        "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69",
        "70-74", "75-79", "80-84", "85-89", "90-94", "95+")
    agei <- c(0, 1, seq(5, 95, 5), Inf)
    ftmp$xlsx$AGE5 <- cut(ftmp$xlsx$AGE, breaks = agei, labels = age, include.lowest = TRUE, right = FALSE)

    ## .. .. 10-year age band
    age <-
      c("<10", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79",
        "80-89", "90+")
    agei <- c(0, seq(10, 90, 10), Inf)
    ftmp$xlsx$AGE10 <- cut(ftmp$xlsx$AGE, breaks = agei, labels = age, include.lowest = TRUE, right = FALSE)
    ## merge dataset
    ftmp$dta <- bind_rows(ftmp$dta, ftmp$xlsx)
  }

  ## save dta
  dta[[i]] <- ftmp$dta
  ## remove file
  rm(ftmp)
  ## add +1
  i <- i + 1
}

## check the dta
str(dta)
head(dta[[1]])
head(dta[[2]])
head(dta[[3]])
head(dta[[4]])

## translate of non-ASCII characters
dta[[1]]$REGION <-
  stringi::stri_trans_general(dta[[1]]$REGION, "latin-ascii")
dta[[2]]$REGION <-
  stringi::stri_trans_general(dta[[2]]$REGION, "latin-ascii")
dta[[3]]$REGION <-
  stringi::stri_trans_general(dta[[3]]$REGION, "latin-ascii")
dta[[4]]$REGION <-
  stringi::stri_trans_general(dta[[4]]$REGION, "latin-ascii")

## add abbrevation for region
unique(dta[[1]]$REGION)
unique(dta[[2]]$REGION)
unique(dta[[3]]$REGION)
unique(dta[[4]]$REGION)

dta[[4]]$REGION <- factor(dta[[4]]$REGION,
                       c("Brussels Hoofdstedelijk Gewest",
                         "Vlaams Gewest",
                         "Waals Gewest incl. Duitst. Gem."),
                       c("BR", "FL", "WA"))

dta[[2]]$REGION <- factor(dta[[2]]$REGION,
                          c("Belgie"),
                          c("BE"))
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## .. AGGREGATE DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## calculate summaries
## .. ARRD
arrd_age <- dta[[1]]
arrd_age$AGE <- as.character(arrd_age$AGE)
arrd_sex <- aggregate(
  data = arrd_age,
  POPULATION ~ YEAR + REGION + SEX,
  FUN = sum
)
arrd_sex$AGE <- "ALL"
arrd_sex$AGE5 <- "ALL"
arrd_sex$AGE10 <- "ALL"

BE_POP_PROJ_ARRD <- bind_rows(arrd_age, arrd_sex)

## .. PROV
prov_age <- dta[[3]]
prov_age$AGE <- as.character(prov_age$AGE)
prov_sex <- aggregate(
  data = prov_age,
  POPULATION ~ YEAR + REGION + SEX,
  FUN = sum
)
prov_sex$AGE <- "ALL"
prov_sex$AGE5 <- "ALL"
prov_sex$AGE10 <- "ALL"

BE_POP_PROJ_PROV <- bind_rows(prov_age, prov_sex)

## .. RGN
rgn_age <- dta[[4]]
rgn_age$AGE <- as.character(rgn_age$AGE)
rgn_sex <- aggregate(
  data = rgn_age,
  POPULATION ~ YEAR + REGION + SEX,
  FUN = sum
)
rgn_sex$AGE <- "ALL"
rgn_sex$AGE5 <- "ALL"
rgn_sex$AGE10 <- "ALL"

BE_POP_PROJ_RGN <- bind_rows(rgn_age, rgn_sex)

## .. BEL
be_age <- dta[[2]]
be_age$AGE <- as.character(be_age$AGE)
be_sex <- aggregate(
  data = be_age,
  POPULATION ~ YEAR + REGION + SEX,
  FUN = sum
)
be_sex$AGE <- "ALL"
be_sex$AGE5 <- "ALL"
be_sex$AGE10 <- "ALL"

BE_POP_PROJ <- bind_rows(be_age, be_sex)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## SAVE DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## check if aggregation was correct (all number should be the same)
aggregate(be, POPULATION ~ YEAR, FUN = sum) # reference
aggregate(munty, POPULATION ~ YEAR, FUN = sum)
aggregate(prov, POPULATION ~ YEAR, FUN = sum)
aggregate(rgn, POPULATION ~ YEAR, FUN = sum)
subset(BE_POP_PROJ, subset = SEX == "MF" & AGE == "ALL", select = c("YEAR", "POPULATION"))


