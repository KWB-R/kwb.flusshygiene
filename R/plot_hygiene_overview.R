# plot_hygiene_overview --------------------------------------------------------
#'
#' Plot Hygiene Overview
#'
#' Creates a plot with three graphs: Histogramm of all e.coli values, a density
#' curve of the last 16 values, and a boxplot of all values again
#'
#' @param hygiene_df A data.frame with the hygiene data of a given river
#'
#' @return Returns a plot
#'
#' @export
#' @importFrom lubridate month
#'

plot_hygiene_overview <- function (hygiene_df){

  hygiene_df$sommer <- lubridate::month(hygiene_df$datum) %in% 5:9
  log_e.coli <- log10(subset(hygiene_df$e.coli, hygiene_df$sommer))
  coef <- stats::coef(MASS::fitdistr(utils::tail(log_e.coli, 16), "normal"))

  graphics::par(mfrow = c(3,1), mar = c(2,4,3.5,2))

  graphics::hist(log_e.coli, prob = T, breaks = seq(0, 5, 0.25), ylim = c(0,1),
       right = F, ylab = "", main="Verteilung E.Coli", col = "#E69F00")
  graphics::text(0.5, 0.5, labels = paste("n =", length(log_e.coli)))
  graphics::curve(function(x) stats::dnorm(x, mean = mean(log_e.coli),
                               sd = stats::sd(log_e.coli)), add = T)
  graphics::box()

  graphics::curve(function(x) stats::dnorm(x, mean = coef[1], sd = coef[2]), ylab = "", xlim = c(0,5))
  perz90 <- round(10^(coef[1]+1.282*coef[2]),0)
  perz95 <- round(10^(coef[1]+1.65*coef[2]),0)
  graphics::text(0.5, 0.5, labels = "n = 16")
  graphics::text(4, 0.4, labels = paste("90. Perz =", perz90, "cfu/100mL"))
  graphics::text(4, 0.2, labels = paste("95. Perz =", perz95, "cfu/100mL"))

  graphics::par(mar = c(3.5,4,3.5,2))

  graphics::boxplot(log_e.coli, horizontal = T, main = "", ylim = c(0,5), col = "#E69F00")
  graphics::points(mean(log_e.coli), 1, pch = 18)
  graphics::text(0.5, 1, labels = paste("n =", length(log_e.coli)))
  graphics::mtext("bacteria log10(cfu/100ml) (MPN)",side = 1, line = 2)

  graphics::par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)
}