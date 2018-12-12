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

assess_bathing_quality_eu <- function
(
  e.coli,
  log = TRUE
)
{
  e.coli <- na.omit(e.coli)
  if (length(e.coli) < 16)
    warning("Less than 16 e.coli elements. EU directive does need 16 at least.")
  if (15 < mean(e.coli))
    log <- FALSE
  if (!log)
    e.coli <- log10(e.coli)
  mu <- mean(e.coli)
  sigma <- sd(e.coli) # sd uses nominator n-1 which leads to higher sigmas
  q90 <- 1.282 * sigma + mu
  q95 <- 1.65  * sigma + mu

  levels <- c("excellent", "good", "sufficient", "poor")

  if(q95 < log10(500))
    return(ordered("excellent", levels = levels))
  if(q95 < log10(1000))
    return(ordered("good", levels = levels))
  if(q90 < log10(900))
    return(ordered("sufficient", levels = levels))
  return(ordered("poor", levels = levels))
}