BE_POP <- readRDS("inst/extdata/statbel_population_belgium.RDS")
BE_POP_MUNTY <- readRDS("inst/extdata/statbel_population_municipality.RDS")
BE_POP_PROV <- readRDS("inst/extdata/statbel_population_province.RDS")
BE_POP_RGN <- readRDS("inst/extdata/statbel_population_region.RDS")
BE_POP_COMTY <- readRDS("inst/extdata/statbel_population_community.RDS")
BE_POP_SECTOR <- readRDS("inst/extdata/statbel_data_sector.RDS")
BE_PROJ_ARROND <- readRDS("inst/extdata/ARROND.RDS")
FPB <- readRDS("inst/extdata/FPB.RDS")

## save data into package data
usethis::use_data(BE_POP_MUNTY, BE_PROJ_ARROND, BE_POP_PROV, BE_POP_RGN,
                  BE_POP,
                  BE_POP_SECTOR,
                  BE_PROJ_ARROND,FPB,
                  overwrite = TRUE,
                  compress = "xz", version = 2)
