# river_model_prediction -------------------------------------------------------
#'
#' Programm for Model Handling and Prediction
#'
#' This function is the front-end for model search on the server, model building
#' with existing data, or prediction with new data. It invokes all other
#' functions and handles their objects. It is a main function.
#'
#' @param river character. The desired river, like "isar".
#' @param river_dir character. Path to server and river directory
#'
#' @return (invisible) The data.frame returned by the prediction plus a date
#' column for easy plotting.
#'
#' @export
#'
#' @examples
#' \donttest{
#' river_model_prediction(river = "isar")
#' }
#'

river_model_prediction <- function(river)
{
  stopifnot(length(river) == 1 && class(river) == "character")

  paths <- get_paths()

  is_valid_model <- FALSE

  while (! is_valid_model) {

    river_dir <- paths[[river]]

    rivermodels <- search_existing_models(river_dir)

    is_valid_user_input <- FALSE

    while (! is_valid_user_input) {

      user_selection <- choose_model(rivermodels)

      if (user_selection == "exit") {

        return(print("Thank you for using this tool"))
      }

      if (user_selection == "new") {

        build_new_model(river)
      }

      if (grepl("ERROR", user_selection)) {

        cat(user_selection)

      } else {

        is_valid_user_input <- TRUE
      }

    }

    which_model <- as.numeric(user_selection)

    chosen_model <- rivermodels[[which_model]]

    if (any(class(chosen_model) == "lm")) {

      is_valid_model <- TRUE
    }
  }

  invisible(predict_quality(chosen_model, river_dir))
}

# search_existing_models -------------------------------------------------------
#'
#' Search river-directory for existing
#' models. These are binary r object files within a model-directory
#' (e.g. "model_binary/isar2_stan.Rdata"). Internal function
#' @describeIn river_model_prediction directory searching
#'
#' @return Returns a list with the existing models for that river
#' (empty if no model was found).
#' @export
#'
#' @examples
#' \donttest{
#' serverpath <- "//poseidon/projekte$/SUW_Department/Projects/FLUSSHYGIENE/Data-Work packages/Daten"
#' river_dir <- search_existing_river_dir(river = "isar", server = serverpath)
#' search_existing_models(river_dir = river_dir)}
#'

search_existing_models <- function(river_dir)
{
  if (is.null(river_dir)) {

    return(list())
  }

  if (! any(grepl("model", dir(river_dir)))) {

    return(list())
  }

  model_dir <- paste0(river_dir, "/DATA_model_binary")

  for (model in dir(model_dir, full.names = TRUE)) {

    load(model)
  }

  river <- tolower(utils::tail(unlist(strsplit(river_dir, "/")), n = 1))

  rivermodels <- lapply(ls()[grepl(river, ls())], FUN = function(x) {

    stats::setNames(list(get(x)), x)
  })

  unlist(rivermodels, recursive = FALSE)
}
