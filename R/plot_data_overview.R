# plot_data_overview -----------------------------------------------------------
#'
#' Plot Data Timeline Overview
#' 
#' Creates a plot with segments or points of the data availability.
#' 
#' @param riverdata A list of hygiene and physical data of the river
#' @param type Either "segment" or "point" for more precise information
#' 
#' @return Returns a plot
#' 
#' @export
#' @importFrom dplyr bind_rows select contains group_by summarise
#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom lubridate as_date
#' 

plot_data_overview <- function (riverdata, type = "segment") {
  stopifnot(type == "segment" | type == "point")
  overview <- dplyr::bind_rows(lapply(riverdata, function(df){
    df <- dplyr::select(df, - dplyr::contains("max"))
    df2 <- stats::na.omit(tidyr::gather(df, "site", "n", -1))
    df2 <- dplyr::group_by(df2, site)
    if(type == "point") 
      return(df2)
    return(dplyr::summarise(df2, start = range(datum)[1], end = range(datum)[2]))
  }), .id = "Typ")
  
  p <- ggplot2::ggplot(overview, ggplot2::aes(y = site, color = Typ)) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1), 
                   text = ggplot2::element_text(size = 15)) + 
    ggplot2::labs(title = "Datenverf\u00fcgbarkeit", x = "Zeitraum", 
         y = "Variable und Standort") + 
    ggplot2::scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
    ggplot2::scale_y_discrete(limits = rev(levels(factor(overview$site))))
  if(type == "segment") 
    p + ggplot2::geom_segment(size = 4, ggplot2::aes(x = lubridate::as_date(start), 
                                   xend = lubridate::as_date(end), 
                                   yend = site))
  else 
    p + ggplot2::geom_point(size = 2, alpha = 0.5, ggplot2::aes(x = lubridate::as_date(datum)))
}