
# function for selecting Arrondissement - gender and optionally year and age group
selectData <- function(arr,gender,year = NULL,ageGroup = NULL)
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
