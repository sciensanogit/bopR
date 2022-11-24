##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## INFO
## This script will download datasets with statistical sectors from Statbel website
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## sources all the needed functions
funcs <- list.files("codes/functions/")
invisible(lapply(paste0("codes/functions/",funcs), source))

## packages needed
pcks <- c("rio","openxlsx","data.table",'readxl',"pbapply")

## load/install packages
pkgCheck(pcks)

##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## BELGIUM DATA STATBEL ####
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##

year <- 2011:2022
STATBEL <- list()

for (y in year) {
  ## load and combine data
  if (y < 2018) {
    url <- paste0("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking/sectoren/TF_POP_STRUCT_SECTORS_",y,".xlsx")
    STATBEL[[as.character(y)]] <- rio::import(url)
  } else if (y == 2019){
    url <- paste0("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking/sectoren/OPEN%20DATA_SECTOREN_", y,".xlsx")
    STATBEL[[as.character(y)]] <- rio::import(url)
  } else {
    url <- paste0("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking/sectoren/OPENDATA_SECTOREN_", y,".xlsx")
    STATBEL[[as.character(y)]] <- rio::import(url)
  }
}

STATBEL <- data.table::rbindlist(STATBEL, idcol = "YEAR", fill = TRUE)

# merge variables with typos
STATBEL$`OPPERVLAKTE IN HM²` <-
  dplyr::coalesce(STATBEL$`OPPERVLAKTE IN HM²`, STATBEL$`OPPERVLAKKTE IN HM²`)
STATBEL$TOTAL <- dplyr::coalesce(STATBEL$POPULATION, STATBEL$TOTAL)

# Clean sector name
for (i in 1:nrow(STATBEL))
{
  if (nchar(STATBEL[[i,3]]) == 4) {

    STATBEL[[i,3]] <- paste(STATBEL[[i,2]], STATBEL[[i,3]], sep = "")
  }
}

# select variables
STATBEL[c("YEAR", "CD_REFNIS", "CD_SECTOR", "TOTAL", "DT_STR_SECTOR", "DT_STOP_SECTOR", "OPPERVLAKTE IN HM²",
          "TX_DESCR_SECTOR_NL", "TX_DESCR_SECTOR_FR", "TX_DESCR_NL", "TX_DESCR_FR")]
