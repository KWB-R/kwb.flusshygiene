# predict_quality --------------------------------------------------------------
#'
#' Predict Hygiene Quality
#'
#' Main function for invoking and object handling. E.Coli hygiene models will
#' be used to predict hygiene quality on differnt scopes.
#'
#' @param model stan_lm. A model of e.coli concentration in given river
#' @param river_dir character. Path to river-data for up-to-date predictions.
#' @param variables character. A vector with all variables used in the model
#' @param newdata data.frame with physical data used in the model
#' @param output character. \code{"season"} will return a list with prediction,
#' \code{"latest"} will return console output
#'
#' @return Returns a list of physical data and prediction and linpred from model
#' @export
#' @importFrom rstanarm posterior_predict
#' @importFrom rstanarm posterior_linpred
#'

predict_quality <- function(model, river_dir, output = "season")
{
  variables <- names(stats::model.frame(model))[-1]

  newdata <- get_newdata(variables, river_dir)

  if (output == "latest") {

    return(print_latest(model, newdata))
  }

  season <- get_latest_season(newdata)

  prediction <- list(
    season = season,
    predict = rstanarm::posterior_predict(model, newdata = season),
    linpred = rstanarm::posterior_linpred(model, newdata = season)
  )

  plot_predicted_quality(model, prediction)

  prediction
}

# get_newdata ------------------------------------------------------------------
#'
#' Read new data for the model variables.
#' @describeIn predict_quality Internal Usage
#'
#' @return Returns a data.frame with the merged data found
#' @export
#'

get_newdata <- function(variables, river_dir)
{
  riverdata <- import_riverdata(paste0(river_dir, "/DATA_preprocessed_csv/"))

  typedata <- riverdata[! grepl("hygiene", names(riverdata))]

  # any data log-transformed?
  prefix <- sub("^(log_)?([a-z]{1,3})_.*", "\\2", variables)

  log_prefix <- unique(prefix[grepl("^log", variables)])

  data_prefix <- sub("^([a-z]{1,3})_.*", "\\1", names(typedata))

  log <- c(log, data_prefix %in% log_prefix)

  names(log) <- names(typedata)

  typedata2 <- mapply(

    FUN = function (df, y) {

      if (! y) {

        return(df)
      }

      df[, -1] <- lapply(df[, -1], function(x) {

        log10(x + 1)
      })

      names(df)[-1] <- paste0("log_", names(df)[-1])

      df

    },
    typedata, log[-1]
  )

  newdata_list <- list()

  unrolled_list <- unroll_physical_data(typedata2)

  for (df in unrolled_list) {

    if (any(select_x <- names(df) %in% variables)) {

      newdata_list[[max(names(df)[select_x])]] <- df[, c(TRUE, select_x[-1])]
    }
  }

  newdata <- newdata_list[[1]]

  for (df in newdata_list[-1]) {

    newdata <- merge(newdata, df, by = "datum")
  }

  newdata
}

# print_latest -----------------------------------------------------------------
#'
#' Print the latest posterior_prediction
#'
#' @describeIn predict_quality Internal Usage
#' @export
#'

print_latest <- function(model, newdata)
{
  latest_input <- utils::tail(stats::na.omit(newdata), n = 1)

  prediction = rstanarm::posterior_predict(model, newdata = latest_input)

  q90 <- stats::quantile(prediction, probs = 0.95)
  q95 <- stats::quantile(prediction, probs = 0.90)

  cat(paste0("90. Perzentil: ", q90, "\n"))
  cat(paste0("95. Perzentil: ", q95, "\n"))

  prediction
}

# get_latest_season ------------------------------------------------------------
#'
#' Search for the latest bathing season in a data.frame
#'
#' @describeIn predict_quality Internal Usage
#' @export
#' @importFrom lubridate month year
#'

get_latest_season <- function(newdata)
{
  is_summer <- lubridate::month(newdata$datum) %in% 5:9

  is_season <- lubridate::year(newdata$datum) ==
    lubridate::year(utils::tail(newdata$datum[is_summer], n = 1))

  stats::na.omit(newdata[is_season & is_summer, ])
}

# plot_predicted_quality -------------------------------------------------------
#'
#' Plot Quality
#'
#' Window function for \link{plot_stan_model}
#'
#' @param model stan.lm model for the river
#' @param prediction list of season, ppd of predcit and ppd of means
#' @param ... Further parameter passed to plot.default
#'
#' @return Plotting function. Returns a plot.
#' @export
#' @importFrom lubridate year
#'

plot_predicted_quality <- function(model, prediction, ...)
{
  seasonyear <- prediction$season

  predictyear <- prediction$predict

  linpredyear <- prediction$linpred

  q95 <- apply(predictyear, 2, stats::quantile, probs = 0.95)
  q90 <- apply(predictyear, 2, stats::quantile, probs = 0.90)

  log <- any(grepl("log", deparse(model$formula)))

  ylim <-  c(0, ifelse(log, 5, 5000))

  xlab <- paste0("Badesaison ", lubridate::year(seasonyear$datum[1]))

  plot_stan_model(
    timestamp = seasonyear$datum, predict = predictyear, linpred = linpredyear,
    q90 = q90, q95 = q95, ylim = ylim, xlab = xlab, cex = 1.5, log = log
  )
}
