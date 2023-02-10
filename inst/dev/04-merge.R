##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##  04-merge.R
##  Purpose: load all and save as package data
##  Author: R HACKATHON
##  Date: 23-01-2023
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## read in all .rds files
BE_POP <- readRDS("inst/extdata/BELPOPLIST.rds")
SPMA <- readRDS("inst/extdata/SPMALIST.rds")

## save as global environment
list2env(BE_POP, globalenv())
list2env(SPMA, globalenv())

## save data into package data
usethis::use_data(BE_POP, BE_POP_COMTY, BE_POP_MUNTY, BE_POP_PROJ,
                  BE_POP_PROJ_ARRD, BE_POP_PROJ_COMTY, BE_POP_PROJ_PROV,
                  BE_POP_PROJ_RGN, BE_POP_PROV, BE_POP_RGN,
                  SPMA, SPMA_COMTY, SPMA_MUNTY, SPMA_PROJ, SPMA_PROJ_ARRD,
                  SPMA_PROJ_COMTY, SPMA_PROJ_PROV, SPMA_PROJ_RGN, SPMA_PROV,
                  SPMA_RGN,
                  overwrite = TRUE,
                  compress = "xz", version = 2)

