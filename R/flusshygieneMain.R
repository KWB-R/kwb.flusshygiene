# get_paths --------------------------------------------------------------------
#' Get List of Paths used in the Flusshygiene Project
#'
#' @param resolve if \code{TRUE} (default) path placeholders are resolved
#' @param \dots arguments passed to \code{\link[kwb.utils]{resolve}} if
#'   \code{resolve} is \code{TRUE}
#'
#' @export
#' @importFrom kwb.utils safePath desktop readDictionary resolve
#'
#' @examples
#' \dontrun{
#' paths <- get_paths()
#'
#' # Paths to the different work package folders
#' paths$ap2
#' paths$ap3
#' paths$ap4
#'
#' # What tables are contained in the ODM database?
#' kwb.db::hsTables(paths$odm)
#'
#' # Get all Flusshygiene data into one data frame
#' data <- kwb.ogre.model::get_lab_values(paths$odm)
#' }
#'
get_paths <- function(resolve = TRUE, ...)
{
  file <- "R_Development/RScripts/Flusshygiene/config/pathDictionary.txt"

  file <- kwb.utils::safePath(kwb.utils::desktop(), file)

  paths <- kwb.utils::readDictionary(file)

  if (resolve) {

    kwb.utils::resolve(paths, ...)

  } else {

    paths
  }
}

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