# plot_rain_overview -----------------------------------------------------------
#'
#' Plot Monthly Rain Summary
#'
#' Creates a plot with a monthly summary overview over the different rain sites
#'
#' @param df A data frame with different rain gauges.
#'
#' @return Returns a plot
#'
#' @export
#' @importFrom dplyr select ends_with mutate group_by summarise_
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom lubridate month
#'

plot_rain_overview <- function(df){
  site_names <- gsub("_day", "", (gsub("r_", "", names(df)[-1])))
  names(df)[-1] <- site_names
  df <- dplyr::mutate(df , mean = rowMeans(df[, -1], na.rm = T))
  df <- dplyr::group_by(df, month = lubridate::month(.data$datum, T))
  n_years <- length(unique(format(df$datum, "%Y")))

  df2 <- dplyr::summarise_(df, .dots = stats::setNames(paste0(
    "sum(",c(site_names, "mean"),", na.rm = T)/", n_years), c(site_names,"mean")))
  df2 <- tidyr::gather(df2, "site", "Niederschlag", -1)
  df2 <- dplyr::mutate(df2, site = factor(.data$site, levels = c(site_names,"mean")))

  ggplot2::ggplot(data = df2, ggplot2::aes_string(x = "month", y = "Niederschlag")) +
    ggplot2::geom_bar(stat = "identity", fill = "steelblue") +
    ggplot2::facet_wrap( ~ site) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1),
          text = ggplot2::element_text(size = 15)) +
    ggplot2::xlab("Jahresverlauf") +
    ggplot2::ggtitle(paste0(c("Durchschnittlicher Monatsniederschlag",
                     format(range(df$datum, na.rm = T), "%Y")), collapse = " - "))
}