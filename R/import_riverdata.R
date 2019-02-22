# import_riverdata ------------------------------------------------------------
#'
#' Read existing River Data
#'
#' Read existing, preprocessed csv files with first column datetime and other
#' columns variable information.
#'
#' @param path character-string to a DATA_preprocessed_csv directory
#'
#' @return Returns a list of data.frames containing the river data
#' @export
#' @importFrom readr read_csv
#'

import_riverdata <- function(path)
{
  csv_files <- dir(path, "\\.csv", full.names = TRUE)

  names(csv_files) <- gsub("\\.csv", "", tolower(basename(csv_files)))

  line1 <- lapply(csv_files, function(x) {

    scan(x, what = character(), sep = ",", nlines = 1, quiet = T)
  })

  types <- lapply(line1, function(x) {

    paste0(c("T", rep("d", length(x) - 1)), collapse = "")
  })

  result <- lapply(names(csv_files), function(x) {

    readr::read_csv(csv_files[x], col_types = types[[x]])
  })

  stats::setNames(result, names(csv_files))
}
