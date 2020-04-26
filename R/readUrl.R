
#' Function with try-cath to read a raw csv table from url
#'
#' @param url desired url to raw format data
#'
#' @return csv table as a dataframe
#' @export # add function to package NAMESPACE
readUrl <- function(url) {
  out <- tryCatch(
    {
      message("Retrieving data")
      readr::read_csv(url(url))
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(cond)
      # Return value in case of error
      return(NA)
      # TODO: in case of error, read latest downloaded data
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(cond)
      # Return value in case of warning
      # Since the data causes error we have to accept it!
      return(readr::read_csv(url(url)))
    },
    finally={
      message(paste("Processed URL:", url))
      message("Data red in")
    }
  )
  return(out)
}
