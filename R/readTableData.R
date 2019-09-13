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
  file_names <- dir(sourcedir, "\\.csv$")

  full_names <- file.path(sourcedir, file_names)

  table_data <- lapply(full_names, utils::read.csv, stringsAsFactors = FALSE)

  stats::setNames(table_data, gsub("\\.csv$", "", file_names))
}
