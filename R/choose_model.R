# choose_model -----------------------------------------------------------------
#'
#' Set User Input for a List of Models
#'
#' Choose model asks the user inside the console for input. Options are Exit,
#' New Model, or one of a list of existing models. If no integer number was
#' presented by the user an ERROR message will be created but no ERROR will be
#' thrown. This way this can be inserted inside a loop.
#'
#' @param rivermodels A list of named models
#'
#' @return Returns the user input as character vector, or an ERROR message.
#'
#' @export
#'
#' @examples
#' \donttest{choose_model(list())}
#' \donttest{choose_model(list(fake_river_model = 1))}
#'

choose_model <- function(rivermodels)
{
  prompt <- "Choose one option: "
  p_exit <- "Exit: \t\t 'exit' \n"
  p_new <- "New Model: \t 'new'"
  p_models <- ""

  if (length(rivermodels) > 0) {

    for (i in seq_along(rivermodels)) {

      p_models <- paste0(
        p_models, " \n", names(rivermodels[i]), paste0(": \t '", i, "'")
      )
    }
  }

  cat(paste0(p_exit, p_new, p_models))

  input <- readline(prompt = prompt)

  if (input == "exit" || input == "new") {

    return(input)
  }

  if (! grepl("^\\d$", input)) {

    return("ERROR: could not read input \n")
  }

  if (! (as.numeric(input) %in% seq_along(rivermodels))) {

    return("ERROR: Number invalid \n")
  }


  input
}
