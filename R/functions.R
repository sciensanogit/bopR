#' Extract data from the Belgian Federal Plan Bureau
#'
#' `selectData` extrart data from a list based on four parameters: arrondissement,
#' gender, year and age
#'
#' @param arr Name of the arrondissement. The list of arrondissement is provided
#' in the object ARROND
#' @param gender Gender for which you want the data - should be in "MF" -
#' Male and Female, "F"- female or "M"-male
#' @param ageGroup Age fro whihc the data is requested - from 0 to 110
#' - optionnal
#' @param year Year for which the data is requested
#' @returns a df (if only arr and gender provide), a vector (if three parameters are provided)
#' or a single value ( if all four parameters are provided)
#' @examples
#' selectData("Charleroi","MF")
#' selectData("Charleroi","MF", year = 1995)
#' selectData("Charleroi","MF", ageGroup = 15)
#' selectData("Charleroi","MF" , year = 1995, ageGroup = 15)
#' @export
selectData <- function(arr,gender,year = NULL,ageGroup = NULL, ARROND = ARROND, FPB = FPB)
{
  if(!(arr %in% ARROND))
  {
    cat("The selected arrondissement is not a real arrondissement! Try again !\n")
  }
  else
  {
    if(!(gender %in% c("MF","F","M")))
    {
      cat("The selected gender doesn't exist! Try again!\n")
    }
    else{
      # both are supplied
      if(!is.null(year) & !is.null(ageGroup))
      {
        if(year < 1992 | year > 2071 | ageGroup < 0 | ageGroup > 110)
        {
          cat("Either the year is outside the range of provided years or people cannot have that age!\n")
        }
        else
        {
          colpos <- which(names(FPB[[arr]][[gender]]) == as.character(year))
          rowpos <- grep(paste0("^",ageGroup, " ans"),rownames(FPB[[arr]][[gender]]) )
          return(FPB[[arr]][[gender]][rowpos,colpos])
        }
      }
      # only year is supplied
      if(!is.null(year) & is.null(ageGroup))
      {
        if(year < 1992 | year > 2071)
        {
          cat("The year is outside the range of provided years!\n")
        }
        else
        {
          colpos <- which(names(FPB[[arr]][[gender]]) == as.character(year))
          return(FPB[[arr]][[gender]][,colpos])
        }
      }
      # only ageGroup is supplied
      if(is.null(year) & !is.null(ageGroup))
      {
        if(ageGroup < 0 | ageGroup > 110)
        {
          cat("People cannot be that young or that old!\n")
        }
        else
        {
          rowpos <- grep(paste0("^",ageGroup, " ans"),rownames(FPB[[arr]][[gender]]) )
          return(FPB[[arr]][[gender]][rowpos,])
        }
      }
      # both year and ageGroup are null
      if(is.null(year) & is.null(ageGroup))
      {
        return(FPB[[arr]][[gender]])
      }
    }
  }
}
