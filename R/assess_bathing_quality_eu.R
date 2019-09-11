#' Bathing quality assessment (EU)
#'
#' Computes the quality assessment according to european bathing directive
#' 2006/7/EC from E.Coli values. The four possible quality levels are:
#' excellent, good, sufficient and poor
#'
#' @param e.coli A numeric vector with e.coli values
#' @param log logical. Are the values log-values?
#'
#'
#' @return Returns a single factor with all quality levels
#' @export

assess_bathing_quality_eu <- function(e.coli, log = TRUE)
{
  e.coli <- stats::na.omit(e.coli)

  if (length(e.coli) < 16) {
    warning("Less than 16 e.coli elements. EU directive does need 16 at least.")
  }

  if (mean(e.coli) > 15) {
    log <- FALSE
  }

  if (! log) {
    e.coli <- log10(e.coli)
  }

  # sd uses nominator n - 1 which leads an unbiased estimate of the population distribution
  sigma <- stats::sd(e.coli)
  mu <- mean(e.coli)

  q90 <- 1.282 * sigma + mu
  q95 <- 1.650 * sigma + mu

  qualities <- c("excellent", "good", "sufficient", "poor")

  index <- if (q95 < log10(500)) {
    1
  } else if (q95 < log10(1000)) {
    2
  } else if (q90 < log10(900) & q95 < log10(1000)) {
    3
  } else {
    4
  }

  factor(qualities[index], levels = qualities, ordered = TRUE)
}
