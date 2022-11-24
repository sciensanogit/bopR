####################################  AIM   ####################################
# download file from both stabel and bureau du plan
####################################  FUNCTIONS ##################################
# function for removing the first line of a DF
removeFirstLine <- function(df)
{
  names(df) <- as.character(df[1,])
  df <- df[-1,]
  rownames(df) <- df[,1]
  df <- df[,-1]
  return(df)
}

# function for formatting the FPB list
formatFPB <- function(df)
{
  begins <- grep("Copyright",df[,1]) + 1
  ends <- grep("110 ans et plus",df[,1])

  genderTablesNames <- c("MF","M","F")

  listGenderTables <-lapply(1:(length(ends)),function(x){df[begins[x] : ends[x],]})

  listGenderTables <- lapply(listGenderTables,removeFirstLine)

  names(listGenderTables) <- genderTablesNames

  return(listGenderTables)
}
# function for removing the last element of a vector
findLast <- function(vec)
{
  return(vec[length(vec)])
}

# function for checking if all packages are installed
# if not, install them

pkgCheck<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}
####################################  PACKAGES #################################
# packaghes needed
pcks <- c("rio","openxlsx","data.table",'readxl',"pbapply")
# laod/install packages
pkgCheck(pcks)

####################################  STATBEL###################################
#
# # define stabel URL
# year <- 2011:2022
# STATBEL <- list()
#
# for (y in year) {
#   ## load and combine data
#   if (y < 2018) {
#     url <- paste0("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking/sectoren/TF_POP_STRUCT_SECTORS_",y,".xlsx")
#     STATBEL[[as.character(y)]] <- rio::import(url)
#   } else if (y == 2019){
#     url <- paste0("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking/sectoren/OPEN%20DATA_SECTOREN_", y,".xlsx")
#     STATBEL[[as.character(y)]] <- rio::import(url)
#   } else {
#     url <- paste0("https://statbel.fgov.be/sites/default/files/files/opendata/bevolking/sectoren/OPENDATA_SECTOREN_", y,".xlsx")
#     STATBEL[[as.character(y)]] <- rio::import(url)
#   }
# }
#
# STATBEL <- data.table::rbindlist(STATBEL, idcol = "YEAR", fill = TRUE)
#
# # merge variables with typos
# STATBEL$`OPPERVLAKTE IN HM²` <-
#   dplyr::coalesce(STATBEL$`OPPERVLAKTE IN HM²`, STATBEL$`OPPERVLAKKTE IN HM²`)
# STATBEL$TOTAL <- dplyr::coalesce(STATBEL$POPULATION, STATBEL$TOTAL)
#
# nchar(STATBEL[[1,3]])
#
# # select variables
# STATBEL[c("YEAR", "CD_REFNIS", "CD_SECTOR", "", "")]

####################################  Bureau du Plan############################
# first import the arrondissements
urlArround <- "https://statbel.fgov.be/sites/default/files/Over_Statbel_FR/Nomenclaturen/REFNIS_2019.xls"
# get and format arrondissement names
ARR <- rio::import(urlArround)
ARROND <- ARR[grep("Arrondissement",ARR[,2]),2]
ARROND <- strsplit(ARROND," ")
ARROND <- unlist(lapply(ARROND,findLast))
ARROND <- gsub("d'","",ARROND)
# nasty manual changes
ARROND[grep("Bruxelles",ARROND)] <- "Région de Bruxelles-Capitale"
ARROND[grep("Louvière",ARROND)] <- "La Louvière"

# get the FPB url
urlBureau <- "https://www.plan.be/databases/demog/pop/poparrfr.xlsx"
# load all the arrondissement in alist
FPB <- pblapply(ARROND , function(x){rio::import(urlBureau,which = x)})
# # for saefety
# saveRDS(FPB,"data/FPB.Rds")
# FPB <- readRDS('data/FPB.Rds')
# format each arrondissemrnt
FPB <- lapply(seq_along(FPB),function(x){formatFPB(FPB[[x]])})
names(FPB)<- arrs
