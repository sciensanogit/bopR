#' @name BE_POP
#' @aliases SPMA BE_POP_MUNTY BE_POP_COMTY BE_POP_ARRD BE_POP_PROV BE_POP_RGN SPMA_MUNTY SPMA_COMTY SPMA_ARRD SPMA_PROV SPMA_RGN
#' @title Population datasets by location, sex and age within Belgium extracted from Open Data at Statistics Belgium.
#' @description Population datasets by location, sex and age within Belgium extracted from Open Data at Statistics Belgium. Namely:
#'
#' \itemize{
#' \item BE_POP_MUNTY: data.frame with data at the level of the municipality
#' \item BE_POP_COMTY: data.frame with data at the level of the community
#' \item BE_POP_ARRD: data.frame with data at the level of the district
#' \item BE_POP_PROV: data.frame with data at the level of the province
#' \item BE_POP_RGN: data.frame with data at the level of the region
#' \item BE_POP: data.frame with data at the level of the whole of Belgium
#' }
#'
#' Mark that Brussels is not considered as a province but for convenience, the province level of Brussels is set to the region information.
#'
#' The data contains the following elements which were available at different levels.
#' \itemize{
#' \item REFNIS: NIS-code for then region
#' \item DESCR_DE: description of the region in German
#' \item DESCR_NL: description of the region in Dutch
#' \item DESCR_EN: description of the region in English
#' \item DESCR_FR: description of the region in French
#' \item YEAR: Population reference year on the 1st of January
#' \item SEX: Sex of the population subgroup ("M" for men, "F" for women and "MF" for both)
#' \item AGE: Age of the population subgroup (exact age)
#' \item AGE5: Age of the population subgroup (by 5-year age band)
#' \item AGE10: Age of the population subgroup (by 10-year age band)
#' \item POPULATION: Number of inhabitants in Belgium on the 1st of January
#' }
#'
#' @docType data
#' @source \url{https://statbel.fgov.be/en/themes/population}
#' @references \url{http://statbel.fgov.be/nl/statistieken/opendata/datasets/tools}
#' @examples
#' \dontrun{
#' data(BE_POP_MUNTY)
#' class(BE_POP_MUNTY)
#' str(BE_POP_MUNTY)
#' }
NULL

#' @name BE_POP_PROJ
#' @aliases SPMA_PROJ BE_POP_PROJ_ARRD BE_POP_PROJ_COMTY BE_POP_PROJ_PROV BE_POP_PROJ_RGN SPMA_PROJ_ARRD SPMA_PROJ_COMTY SPMA_PROJ_PROV SPMA_PROJ_RGN
#' @title Population projections by location, sex and age within Belgium extracted from Open Data at Planbureau Belgium.
#' @description Population projections by location, sex and age within Belgium extracted from Open Data at Planbureau Belgium.
#'
#' \itemize{
#' \item BE_POP_PROJ_ARRD: data.frame with data at the level of the district
#' \item BE_POP_PROJ_COMTY: data.frame with data at the level of the community
#' \item BE_POP_PROJ_PROV: data.frame with data at the level of the province
#' \item BE_POP_PROJ_RGN: data.frame with data at the level of the region
#' \item BE_POP_PROJ: data.frame with data at the level of the whole of Belgium
#' }
#'
#' The data contains the following elements which were available at different levels.
#' \itemize{
#' \item REFNIS: NIS-code for then region
#' \item DESCR_DE: description of the region in German
#' \item DESCR_NL: description of the region in Dutch
#' \item DESCR_EN: description of the region in English
#' \item DESCR_FR: description of the region in French
#' \item YEAR: Population reference year on the 1st of January
#' \item SEX: Sex of the population subgroup ("M" for men, "F" for women and "MF" for both)
#' \item AGE: Age of the population subgroup (exact age)
#' \item AGE5: Age of the population subgroup (by 5-year age band)
#' \item AGE10: Age of the population subgroup (by 10-year age band)
#' \item POPULATION: Number of inhabitants in Belgium on the 1st of January
#' }
#'
#' @docType data
#' @source \url{https://www.plan.be/databases/}
#' @references \url{https://www.plan.be/databases/}
#' @examples
#' \dontrun{
#' data(BE_POP_PROJ)
#' class(BE_POP_PROJ)
#' str(BE_POP_PROJ)
#' }
NULL








#' @name BE_POP_SECTOR
#' @aliases
#' @title Population datasets by location at the level of the statistical sector within Belgium extracted from Open Data at Statistics Belgium.
#' @description Population datasets by location at the level of the statistical sector within Belgium extracted from Open Data at Statistics Belgium. Namely:
#'
#' \itemize{
#' \item BE_POP_SECTOR: data.frame with data at the level of the whole of Belgium
#' }
#'
#'
#' The data contains the following elements which were available at different levels.
#' \itemize{
#' \item YEAR: Population reference year on the 1st of January
#' \item CD_REFNIS: code of the municipality
#' \item CD_SECTOR: code of the statistical sector (NIS9)
#' \item TOTAL: Number of inhabitants in the statistical sector on the 1st of January
#' \item DT_STRT_SECTOR: date of creation of the statistical sector
#' \item DT_SIOP_SECTOR: date stop statistical sector
#' \item OPPERVLAKTE IN HM²: Surface area of the statistical sector
#' \item TX_DESCR_SECTOR_NL: name of the statistical sector (Dutch)
#' \item TX_DESCR_SECTOR_FR: name of the statistical sector (French)
#' \item TX_DESCR_NL: name of the municipality (Dutch)
#' \item TX_DESCR_FR: name of the municipality (French)

#' }
#'
#' @docType data
#' @source \url{https://statbel.fgov.be/en/themes/population}
#' @references \url{http://statbel.fgov.be/nl/statistieken/opendata/datasets/tools}
#' @examples
#' \dontrun{
#' data(BE_POP_SECTOR)
#' class(BE_POP_SECTOR)
#' str(BE_POP_SECTOR)
#' data(BE_POP_SECTOR)
#' class(BE_POP_SECTOR)
#' str(BE_POP_SECTOR)
#' }
NULL







