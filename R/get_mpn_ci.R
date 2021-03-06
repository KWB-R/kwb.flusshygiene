# get_mpn_ci -------------------------------------------------------------------
#' Get MPN Confidence Intervals for E.Coli
#'
#' Lookup laboratory tables for MPN values for E.Coli to get upper and lower
#' 0.95 confidence interval for the given values. If value is not directly
#' found in table it will be generated by interpolating nearest neighbors.
#'
#' @param e.coli numeric. A vector for e.coli values
#' @return A data.frame with 3 columns: e.coli, lo, up
#' @export
#' @importFrom dplyr bind_rows arrange
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' print(get_mpn_ci(c(15,30,35,60,61,71,120,1959,25000,369990)))
#' }
#'
get_mpn_ci <- function(e.coli)
{
  path <- paste0(get_paths()$dir.riverdata, "/MPN_tables")

  # Helper function to read text file
  read <- function(file) utils::read.table(file, header = TRUE, sep = " ")

  mpn_2 <- read(file = paste0(path, "/MPN_2dilutions_delim.txt"))
  mpn_4 <- read(file = paste0(path, "/MPN_4dilutions_delim.txt"))

  # mpn_2: mpn = bacteria/100ml
  # mpn_4: mpn*d*100 = bacteria/100ml , with d = 10
  mpn_4[, 4:6] <- mpn_4[, 4:6] * 1000

  mpn <- dplyr::bind_rows(mpn_2[, 3:5], mpn_4[, 4:6])

  lookup_mpn <- function (x) {

    is_in <- mpn$MPN == x

    if (any(is_in)) {

      ind <- which(is_in)
      res <- as.numeric(mpn[ind[1],])

    } else {

      mpn_sorted <- dplyr::arrange(mpn, .data$MPN)
      ind <- findInterval(x, mpn_sorted$MPN)
      df <- mpn_sorted[c(ind, ind + 1), ]

      # Helper function to predict
      rounded_prediction <- function(formula) {
        round(stats::predict(
          stats::lm(formula, data = df), newdata = data.frame(MPN = x)
        ))
      }

      res <- c(x, rounded_prediction(LO ~ MPN), rounded_prediction(UP ~ MPN))
    }

    stats::setNames(res, c("e.coli", "lo", "up"))
  }

  as.data.frame(t(vapply(e.coli, lookup_mpn, numeric(3))))
}
