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
  dev_dir_1 <- file.path(kwb.utils::desktop(), "R_Development")
  dev_dir_2 <- file.path("~/R-Development")

  dev_dir <- ifelse(file.exists(dev_dir_1), dev_dir_1, dev_dir_2)

  path <- "RScripts/Flusshygiene/config/pathDictionary.txt"

  file <- kwb.utils::safePath(dev_dir, path)

  paths <- kwb.utils::readDictionary(file)

  if (resolve) {

    kwb.utils::resolve(paths, ...)

  } else {

    paths
  }
}
