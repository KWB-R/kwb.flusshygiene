# plot_q_overview --------------------------------------------------------------
#'
#' Plot Flowing Conditions
#' 
#' Creates a plot with the standard flowing conditions over the year. The data 
#' of all years will be taken into account.
#'  
#' @param q_df The data.frame with 2 columns: datum and q
#' 
#' @return Returns a plot
#' 
#' @export
#' @importFrom dplyr mutate group_by summarise
#' @import ggplot2 
#' @importFrom tidyr gather
#' @importFrom lubridate yday month
#' 

plot_q_overview <- function (q_df){
  df <- dplyr::mutate(q_df, yday = lubridate::yday(datum))
  df <- tidyr::gather(df[,-1], "site", "q", - yday)
  df <- dplyr::group_by(df, yday, site)
  df <- dplyr::summarise(df, q_min = min(q,na.rm = T), q_max = max(q,na.rm = T), 
                         q_median = stats::median(q, na.rm = T))
  
  
  ggplot2::ggplot(df, ggplot2::aes(x = yday,y = q_median)) + 
    ggplot2::facet_wrap(~ site) + 
    ggplot2::geom_line() + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q_min,ymax = q_max),alpha = 0.4) + 
    ggplot2::scale_x_continuous(breaks = seq(1,331, by = 30), 
                                labels = unique(lubridate::month(q_df$datum,T))) + 
    ggplot2::scale_y_log10(minor_breaks = c(seq(2:9), seq(20,90,10), 
                                            seq(200,900,100), 
                                            2000, 3000, 4000, 5000)) + 
    ggplot2::coord_trans(y = "log10") + 
    ggplot2::labs(y = "Durchfluss [m\u00b3/s]", x = "Jahresverlauf (DoY)")
}