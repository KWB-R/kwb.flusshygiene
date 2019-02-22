# correlation_scatterplot ------------------------------------------------------
#'
#' Scatterplotmatrix of similar Variables to E.Coli
#'
#' Takes similar named variables and produces a matrix of scatterplots and their
#' correlation coefficients to E.Coli.
#'
#' @param df data.frame with data for e.coli and chosen variables in lagdays
#' @param \dots Arguments passed to \code{stats::cor}
#'
#' @return Plotting function. Returns a plot.
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr select starts_with
#' @importFrom tidyr gather
#'

correlation_scatterplot<- function(df, ...)
{
  cor_values <- correlation_values(df, ...)

  df_history <- tidyr::gather(df, "lagday", "n", -1)

  df_history$lagday <- factor(
    df_history$lagday, levels = unique(df_history$lagday)
  )

  ylab <- "bacteria (cfu/100ml) (MPN)"

  if (grepl("^log", names(df)[1])) {

    ylab <- paste(ylab, "- log10")
  }

  variable <- sub("^(log_)?([a-z]{1,3})_.*", "\\2", names(df)[2])

  xlabs <-list(
    r = "Niederschlag (mm)",
    q = "Durchfluss (m\u00b3/s)",
    ka = "Kl\u00e4rwerksabluss (m\u00b3/d)",
    rad = "Globalstrahlung (W/m\u00b2)"
  )

  xlab <- xlabs[[variable]]

  if (grepl("^log", names(df)[2])) {

    xlab <- paste(xlab, "- log10")
  }

  y <- df_history[,1]

  x_max <- max(df_history$n, na.rm = TRUE) * 0.8
  y_min <- min(df_history[, 1], na.rm = TRUE) * 1.25

  ggplot2::ggplot(data = df_history, ggplot2::aes_string(x = "n", y = "y")) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ lagday) +
    ggplot2::geom_label(data = cor_values, ggplot2::aes_string(
      label = "cor_values", x = "x_max", y = "y_min"
    )) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = xlab, y = ylab)
}

# correlation_values -----------------------------------------------------------
#'
#' \code{correlation_values}: computes the correlation coefficients of
#' a data.frame with \code{names(df) == c("datum", "e.coli", variables)}
#'
#' @describeIn correlation_scatterplot Internal function
#' @return Returns correlation values.
#' @export
#'
#' @examples
#' \donttest{correlation_values(data.frame(datum = rep("egal",10), e.coli = 1:10, var = 1:10), variable = "var")}
#'

correlation_values <- function(df, ...)
{
  cor_values <- unlist(lapply(df[,-1], function(y) {

    stats::cor(x = df[,1], y = y, use = "pairwise.complete.obs", ...)
  }))

  data.frame(
    cor_values = round(cor_values, 4),
    lagday = names(cor_values)
  )
}
