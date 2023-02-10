##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  02-federal-planning-bureau-data.R
##  Purpose: load and update federal planning bureau population data
##  Author: R HACKATHON
##  Date: 23-01-2023
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## INFO
## This script will unpack and read the different population datasets by FPB,
## and will aggregate them according to different geospatial levels (RGN,
## and COUNTRY)
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## load packages
library(tidyverse)
library(utils)
library(openxlsx)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## PROJECTIONS ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## .. READ DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## read-in data for NUTS names and NIS
nuts <- openxlsx::read.xlsx("inst/extdata/TU_COM_NUTS_LAU-20190101-PROJ.xlsx")
nuts <- nuts[c("TX_FPB", "CD_MUNTY_REFNIS", "TX_DESCR_DE", "TX_DESCR_EN", "TX_DESCR_FR", "TX_DESCR_NL")]

## read-in and extract data
ls <-
  list.files(path = "inst/extdata/",
             pattern = "nl.xlsx",
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
    ## only complete cases
    ftmp$xlsx <- drop_na(ftmp$xlsx)
    ## check uniques in age and add sex
    colnames(ftmp$xlsx)[1] <- "AGE"
    ftmp$xlsx$AGE <- as.numeric(gsub(pattern = " jaar| jaar en meer", replacement = "", x = ftmp$xlsx$AGE))
    ## add sex
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
  dta[[tools::file_path_sans_ext(basename(fxlsx))]] <- ftmp$dta
  ## remove file
  rm(ftmp)
  ## add +1
  i <- i + 1
}

## check the dta
str(dta)
head(dta$popbelnl) ## Belgium
head(dta$popregnl) ## Region
head(dta$popcomnl) ## Communities
head(dta$popprovnl) ## Province
head(dta$poparrnl) ## Arrondisments

## translate of non-ASCII characters
# dta[[1]]$REGION <-
#   stringi::stri_trans_general(dta[[1]]$REGION, "latin-ascii")
# dta[[2]]$REGION <-
#   stringi::stri_trans_general(dta[[2]]$REGION, "latin-ascii")
# dta[[3]]$REGION <-
#   stringi::stri_trans_general(dta[[3]]$REGION, "latin-ascii")
# dta[[4]]$REGION <-
#   stringi::stri_trans_general(dta[[4]]$REGION, "latin-ascii")
# dta[[5]]$REGION <-
#   stringi::stri_trans_general(dta[[5]]$REGION, "latin-ascii")

## add abbrevation for region
unique(dta[[1]]$REGION)
unique(dta[[2]]$REGION)
unique(dta[[3]]$REGION)
unique(dta[[4]]$REGION)
unique(dta[[5]]$REGION)

# unique(dta$popregnl$REGION)
# dta$popregnl$REGION <- factor(dta$popregnl$REGION,
#                        c("Brussels Hoofdstedelijk Gewest",
#                          "Vlaams Gewest",
#                          "Waals Gewest incl. Duitst. Gem."),
#                        c("BR", "FL", "WA"))
#
# unique(dta$popbelnl$REGION)
# dta$popbelnl$REGION <- factor(dta$popbelnl$REGION,
#                           c("Belgie"),
#                           c("BE"))
#
# ## german community
# unique(dta$popcomnl$REGION)
# dta$popcomnl$REGION <- factor(dta$popcomnl$REGION,
#                               c("Duitst. Gemeenchap"),
#                               c("GC"))

###
# Calculate projections by community
###
## .. merge region with community and substract GC numbers
tmp.region.FR <- subset(dta$popregnl, REGION == "Waals Gewest incl. Duitst. Gem.")
tmp.region.BRFL <- subset(dta$popregnl, REGION != "Waals Gewest incl. Duitst. Gem.")
tmp.comm.GC <- dta$popcomnl

tmp.comm.FR <- full_join(
  tmp.region.FR,
  tmp.comm.GC,
  by = c("AGE", "SEX", "YEAR", "AGE5", "AGE10")
  )

## .. calculate and add
tmp.comm.FR$POPULATION <- tmp.comm.FR$POPULATION.x - tmp.comm.FR$POPULATION.y
tmp.comm.FR$REGION <- "Waals Gewest"

## .. select
tmp.comm.FR <- subset(tmp.comm.FR, select = c("AGE", "SEX", "REGION", "YEAR", "AGE5", "AGE10", "POPULATION"))

## .. merge
tmp.comm <- bind_rows(
  tmp.region.BRFL,
  tmp.comm.FR,
  tmp.comm.GC
  )

unique(tmp.comm$REGION)

## overwrite
dta$popcomnl <- tmp.comm

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## .. AGGREGATE DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## calculate summaries
## .. ARRD
arrd_age <- dta$poparrnl
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

## .. .. check unique values
BE_POP_PROJ_ARRD$REGION[BE_POP_PROJ_ARRD$REGION != "Brussels Hoofdstedelijk Gewest"] <- paste0("A. ", BE_POP_PROJ_ARRD$REGION[BE_POP_PROJ_ARRD$REGION != "Brussels Hoofdstedelijk Gewest"])
BE_POP_PROJ_ARRD$REGION[BE_POP_PROJ_ARRD$REGION == "Brussels Hoofdstedelijk Gewest"] <- "A. Brussel-Hoofdstad"
unique(BE_POP_PROJ_ARRD$REGION)

## .. .. merge with nuts for projection
BE_POP_PROJ_ARRD <- left_join(BE_POP_PROJ_ARRD, nuts, by = c("REGION" = "TX_FPB"))

## .. .. rename
BE_POP_PROJ_ARRD <- rename(
  BE_POP_PROJ_ARRD,
  REFNIS = CD_MUNTY_REFNIS,
  DESCR_EN = TX_DESCR_EN,
  DESCR_NL = TX_DESCR_NL,
  DESCR_FR = TX_DESCR_FR,
  DESCR_DE = TX_DESCR_DE)
BE_POP_PROJ_ARRD <- select(BE_POP_PROJ_ARRD, -REGION)

## .. PROV
prov_age <- dta$popprovnl
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

## .. .. check unique values
unique(BE_POP_PROJ_PROV$REGION)

## .. .. merge with nuts for projection
BE_POP_PROJ_PROV <- left_join(BE_POP_PROJ_PROV, nuts, by = c("REGION" = "TX_FPB"))

## .. .. rename
BE_POP_PROJ_PROV <- rename(
  BE_POP_PROJ_PROV,
  REFNIS = CD_MUNTY_REFNIS,
  DESCR_EN = TX_DESCR_EN,
  DESCR_NL = TX_DESCR_NL,
  DESCR_FR = TX_DESCR_FR,
  DESCR_DE = TX_DESCR_DE)
BE_POP_PROJ_PROV <- select(BE_POP_PROJ_PROV, -REGION)

## .. RGN
rgn_age <- dta$popregnl
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

## .. .. check unique values
unique(BE_POP_PROJ_RGN$REGION)

## .. .. merge with nuts for projection
BE_POP_PROJ_RGN <- left_join(BE_POP_PROJ_RGN, nuts, by = c("REGION" = "TX_FPB"))

## .. .. rename
BE_POP_PROJ_RGN <- rename(
  BE_POP_PROJ_RGN,
  REFNIS = CD_MUNTY_REFNIS,
  DESCR_EN = TX_DESCR_EN,
  DESCR_NL = TX_DESCR_NL,
  DESCR_FR = TX_DESCR_FR,
  DESCR_DE = TX_DESCR_DE)
BE_POP_PROJ_RGN <- select(BE_POP_PROJ_RGN, -REGION)

## .. BEL
be_age <- dta$popbelnl
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

## .. .. check unique values
unique(BE_POP_PROJ$REGION)

## .. .. merge with nuts for projection
BE_POP_PROJ$REFNIS <- 100
BE_POP_PROJ$DESCR_DE <- "Belgien"
BE_POP_PROJ$DESCR_EN <- "Belgium"
BE_POP_PROJ$DESCR_NL <- "België"
BE_POP_PROJ$DESCR_FR <- "Belgique"

## .. .. rename
BE_POP_PROJ <- select(BE_POP_PROJ, -REGION)

## .. COMTY
comty_age <- dta$popcomnl
comty_age$AGE <- as.character(comty_age$AGE)
comty_sex <- aggregate(
  data = comty_age,
  POPULATION ~ YEAR + REGION + SEX,
  FUN = sum
)
comty_sex$AGE <- "ALL"
comty_sex$AGE5 <- "ALL"
comty_sex$AGE10 <- "ALL"

BE_POP_PROJ_COMTY <- bind_rows(comty_age, comty_sex)

## .. .. check unique values
unique(BE_POP_PROJ_COMTY$REGION)

## .. .. add row to nuts
nuts.gc <- tibble(
  TX_FPB = "Duitst. Gemeenchap",
  CD_MUNTY_REFNIS = "9999",
  TX_DESCR_DE = "Deutschsprachige Gemeinschaft",
  TX_DESCR_EN = "German-speaking Community",
  TX_DESCR_FR = 'Communauté germanophone',
  TX_DESCR_NL = 'Duitstalige Gemeenschap'
)
nuts <- bind_rows(nuts, nuts.gc)

## .. .. rename
BE_POP_PROJ_COMTY$REGION[BE_POP_PROJ_COMTY$REGION == "Waals Gewest"] <- "Waals Gewest incl. Duitst. Gem."

## .. .. merge with nuts for projection
BE_POP_PROJ_COMTY <- left_join(BE_POP_PROJ_COMTY, nuts, by = c("REGION" = "TX_FPB"))

## .. .. rename
BE_POP_PROJ_COMTY <- rename(
  BE_POP_PROJ_COMTY,
  REFNIS = CD_MUNTY_REFNIS,
  DESCR_EN = TX_DESCR_EN,
  DESCR_NL = TX_DESCR_NL,
  DESCR_FR = TX_DESCR_FR,
  DESCR_DE = TX_DESCR_DE)
BE_POP_PROJ_COMTY <- select(BE_POP_PROJ_COMTY, -REGION)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## SAVE DATA ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## check if aggregation was correct (all number should be the same)
# aggregate(BE_POP_PROJ, POPULATION ~ YEAR, FUN = sum) # reference
# aggregate(BE_POP_PROJ_RGN, POPULATION ~ YEAR, FUN = sum)
# aggregate(BE_POP_PROJ_COMTY, POPULATION ~ YEAR, FUN = sum)
# aggregate(BE_POP_PROJ_PROV, POPULATION ~ YEAR, FUN = sum)
# aggregate(BE_POP_PROJ_ARRD, POPULATION ~ YEAR, FUN = sum)

## save the results
saveRDS(BE_POP_PROJ, "inst/extdata/BE_POP_PROJ.rds")
saveRDS(BE_POP_PROJ_RGN, "inst/extdata/BE_POP_PROJ_RGN.rds")
saveRDS(BE_POP_PROJ_PROV, "inst/extdata/BE_POP_PROJ_PROV.rds")
saveRDS(BE_POP_PROJ_COMTY, "inst/extdata/BE_POP_PROJ_COMTY.rds")
saveRDS(BE_POP_PROJ_ARRD, "inst/extdata/BE_POP_PROJ_ARRD.rds")

































# ####################################  AIM   ####################################
# # download file from both stabel and bureau du plan
# ####################################  FUNCTIONS ##################################
# # function for removing the first line of a DF
# removeFirstLine <- function(df)
# {
#   names(df) <- as.character(df[1,])
#   df <- df[-1,]
#   rownames(df) <- df[,1]
#   df <- df[,-1]
#   return(df)
# }
#
# # function for formatting the FPB list
# formatFPB <- function(df)
# {
#   begins <- grep("Copyright",df[,1]) + 1
#   ends <- grep("110 ans et plus",df[,1])
#
#   genderTablesNames <- c("MF","M","F")
#
#   listGenderTables <-lapply(1:(length(ends)),function(x){df[begins[x] : ends[x],]})
#
#   listGenderTables <- lapply(listGenderTables,removeFirstLine)
#
#   names(listGenderTables) <- genderTablesNames
#
#   return(listGenderTables)
# }
# # function for removing the last element of a vector
# findLast <- function(vec)
# {
#   return(vec[length(vec)])
# }
#
# # function for checking if all packages are installed
# # if not, install them
#
# pkgCheck<-function(...) {
#   libs<-unlist(list(...))
#   req<-unlist(lapply(libs,require,character.only=TRUE))
#   need<-libs[req==FALSE]
#   if(length(need)>0){
#     install.packages(need)
#     lapply(need,require,character.only=TRUE)
#   }
# }
# ####################################  PACKAGES #################################
# # packaghes needed
# pcks <- c("rio","openxlsx","data.table",'readxl',"pbapply")
# # laod/install packages
# pkgCheck(pcks)
#
# ####################################  STATBEL###################################
# #
# # # define stabel URL
# # year <- 2011:2022
# # STATBEL <- list()
# #
# # for (y in year) {
# #   ## load and combine data
# #   if (y < 2018) {
# #     url <- paste0("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking/sectoren/TF_POP_STRUCT_SECTORS_",y,".xlsx")
# #     STATBEL[[as.character(y)]] <- rio::import(url)
# #   } else if (y == 2019){
# #     url <- paste0("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking/sectoren/OPEN%20DATA_SECTOREN_", y,".xlsx")
# #     STATBEL[[as.character(y)]] <- rio::import(url)
# #   } else {
# #     url <- paste0("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking/sectoren/OPENDATA_SECTOREN_", y,".xlsx")
# #     STATBEL[[as.character(y)]] <- rio::import(url)
# #   }
# # }
# #
# # STATBEL <- data.table::rbindlist(STATBEL, idcol = "YEAR", fill = TRUE)
# #
# # # merge variables with typos
# # STATBEL$`OPPERVLAKTE IN HM²` <-
# #   dplyr::coalesce(STATBEL$`OPPERVLAKTE IN HM²`, STATBEL$`OPPERVLAKKTE IN HM²`)
# # STATBEL$TOTAL <- dplyr::coalesce(STATBEL$POPULATION, STATBEL$TOTAL)
# #
# # nchar(STATBEL[[1,3]])
# #
# # # select variables
# # STATBEL[c("YEAR", "CD_REFNIS", "CD_SECTOR", "", "")]
#
# ####################################  Bureau du Plan############################
# # first import the arrondissements
# urlArround <- "https://statbel.fgov.be/sites/default/files/Over_Statbel_FR/Nomenclaturen/REFNIS_2019.xls"
# # get and format arrondissement names
# ARR <- rio::import(urlArround)
# ARROND <- ARR[grep("Arrondissement",ARR[,2]),2]
# ARROND <- strsplit(ARROND," ")
# ARROND <- unlist(lapply(ARROND,findLast))
# ARROND <- gsub("d'","",ARROND)
# # nasty manual changes
# ARROND[grep("Bruxelles",ARROND)] <- "Région de Bruxelles-Capitale"
# ARROND[grep("Louvière",ARROND)] <- "La Louvière"
#
# # get the FPB url
# urlBureau <- "https://www.plan.be/databases/demog/pop/poparrfr.xlsx"
# # load all the arrondissement in alist
# FPB <- pblapply(ARROND , function(x){rio::import(urlBureau,which = x)})
# # # for saefety
# # saveRDS(FPB,"data/FPB.Rds")
# # FPB <- readRDS('data/FPB.Rds')
# # format each arrondissemrnt
# FPB <- lapply(seq_along(FPB),function(x){formatFPB(FPB[[x]])})
# names(FPB)<- ARROND
#
# saveRDS(FPB,"inst/extdata/FPB.RDS")
# saveRDS(ARROND,"inst/extdata/ARROND.RDS")
