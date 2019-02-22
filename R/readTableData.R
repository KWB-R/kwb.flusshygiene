# readTableData ----------------------------------------------------------------

#' Read Data for ODM Tables
#'
#' Read data for ODM tables from CSV files stored in the package
#'
#' @param sourcedir path to input directory
#' @return list of data frames
#'
#' @export
#'
readTableData <- function
(
  sourcedir = system.file("extdata", "ODM", package = "kwb.flusshygiene")
)
{
  filenames <- dir(sourcedir, "\\.csv$")

  tabledata <- lapply(
    file.path(sourcedir, filenames),
    FUN = utils::read.csv,
    stringsAsFactors = FALSE
  )

  names(tabledata) <- gsub("\\.csv$", "", filenames)

  tabledata
}