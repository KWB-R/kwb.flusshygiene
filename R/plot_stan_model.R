# plot_stan_model --------------------------------------------------------------
#'
#' Plot Model Prediction with Quality Assessment
#'
#' Plots a sample of posterior predictions and means. Furthermore colours an
#' hygiene quality assessment as background (see EU Bathing Water Directive)
#' Dark blue means excellent quality.
#' Steelblue means good quality.
#' Yellow means sufficient quality.
#' Red means insufficient quality.
#'
#' @param timestamp POSIX. The x-axis timestamp
#' @param predict ppd. The posterior prediction of the model
#' @param linpred ppd. The linpred (predicted means) of the model
#' @param log logical. Is E.Coli log01-transformed?
#' @param q90 numeric. The 90. percentile of predict.
#' @param q95 numeric. The 95. precentile of predict.
#' @param nlines numeric. How many lines for posterion predictions?
#' @param nlinesCenter numeric. How many lines for predicted means?
#' @param ... Further parameters for plot.default
#'
#' @return Plotting function. Returns a plot.
#' @export
#'

plot_stan_model <- function(
  timestamp, predict, linpred, log = FALSE, q90, q95, nlines = 250,
  nlinesCenter = 100, ...
){

  if (log) {

    ylabtext <- "lg(E.coli MPN) (cfu/100ml)"
    q95 <- 10^q95
    q90 <- 10^q90
  }

  if (! log) {

    ylabtext <- "E.coli MPN (cfu/100ml)"
  }

  # empty plot
  graphics::plot(timestamp, predict[1, ], type = "n", ..., ylab = ylabtext)

  # colors
  Fh <- grDevices::rgb(  0,  86, 110,  20, maxColorValue = 255)
  FH <- grDevices::rgb(  0,  86, 110, 255, maxColorValue = 255)
  G  <- grDevices::rgb(190, 190, 193,  20, maxColorValue = 255)

  assessment <- list(
    excellent = FH,
    good = "steelblue",
    sufficient = grDevices::rgb(255, 215, 0, 200, maxColorValue = 255),
    bad = grDevices::rgb(139, 0, 0, 200, maxColorValue = 255)
  )

  # classification
  for (j in seq_along(q95)) {

    if (q90[j] > 900) {

      graphics::rect(
        xleft = timestamp[j], xright = timestamp[j + 1],density = -1,
        border = NA, ybottom = 0, ytop = 100000, col = assessment$bad
      )
    }

    if (q90[j] < 900 & q95[j] > 1000) {

      graphics::rect(
        xleft = timestamp[j], xright = timestamp[j + 1], ybottom = 0,
        border = NA, ytop = 100000, col = assessment$sufficient
      )
    }

    if (q95[j] < 1000 & q95[j] > 500) {

      graphics::rect(
        xleft = timestamp[j], xright = timestamp[j + 1], ybottom = 0,
        ytop = 100000, border = NA, col = assessment$good
      )
    }

    if (q95[j] < 500) {

      graphics::rect(
        xleft = timestamp[j], xright = timestamp[j +1 ], ybottom = 0,
        ytop = 100000, border = NA, col = assessment$excellent
      )
    }

  }

  # Plotinhalt
  for (i in sample(seq_len(nrow(predict)), size = nlines)) {

    graphics::lines(timestamp , predict[i, ], col = G, pch = 19)
  }

  for (i in sample(seq_len(nrow(linpred)), size = nlinesCenter)) {

    graphics::lines(timestamp, linpred[i,], col = Fh, pch = 19)
  }

  graphics::legend(
    "topright", lwd = c(3,3), col = c(FH,"grey"), lty = c(1,1),
    legend = c("Modellierter Mittelwert", "Vorhersageintervall")
  )
}
