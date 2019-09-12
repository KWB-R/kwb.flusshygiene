# build_model ------------------------------------------------------------------
#'
#' Build a Model for E.Coli
#'
#' Functions for modelbuilding \\n
#' \code{build_model}: takes the riverdata, handles the other functions and
#' invokes \code{\link[rstanarm]{stan_lm}}
#' @details Build the model from hygiene data and physical data like flow, rain,
#'  wwtp. Asks for user input to select variables. Computes the data.frame with
#'  data for hygiene and chosen variables and creates a formula of the form:
#' \code{Q*(K + R)} while multiple Qs will be multiplied, multiple Ks and Rs
#' will be added.
#'
#'
#' @param riverdata a list with riverdata (hygiene + physical data)
#' @param variables character. Selected variables for the model
#' @param with_interaction logical. Formula with interactions? Default set to
#'   TRUE
#'
#' @return Returns a model of the riverdata.
#' @export
#'
#' @importFrom rstanarm stan_lm R2
#'

build_model <- function(
  riverdata,
  variables = ask_for_variables(riverdata),
  with_interaction = TRUE
)
{
  model_riverdata <- process_model_riverdata(riverdata, variables)

  model_formula <- create_formula(variables, with_interaction)

  do.call(rstanarm::stan_lm, list(
    model_formula, data = model_riverdata,
    prior = rstanarm::R2(location = 0.8),
    adapt_delta = 0.99999
  ))
}

# ask_for_variables ------------------------------------------------------------
#'
#' \code{ask_for_variables}: asks the user for variable selection. Possible
#' variables are all variables in riverdata.
#'
#' @describeIn build_model Internal function. Quite time consuming
#' @return Returns a character-vector with the chosen model variables
#'

ask_for_variables <- function(riverdata)
{
  # prepare data
  hygiene_df <- riverdata[[grep("hygiene", names(riverdata))]]
  typedata <- riverdata[! grepl("hygiene", names(riverdata))]

  # e.coli log-transformed?
  message(
    "You now have to choose the variables for your model E.Coli ~ predictors"
  )

  yn <- readline(prompt = "E.Coli as log10? (y/n): ")

  log <- (yn == "y")

  # general selection and log-transformation
  for (type in names(typedata)) {

    yn <- readline(prompt = paste0("Include ", type, "? (y/n): "))

    if (yn != "y") {

      typedata <- subset(typedata, subset = names(typedata) != type)

    } else {

      # variable log-transformed?
      yn <- readline(prompt =  paste(type,"as log10? (y/n): "))

      log <- c(log, (yn == "y"))
    }
  }

  # change log-transformations in data
  names(log) <- c("e.coli", names(typedata))

  if (log[["e.coli"]]) {

    hygiene_df$log_e.coli <- log10(hygiene_df$e.coli)

    hygiene_df <- subset(hygiene_df, select = c("datum", "log_e.coli"))

  } else {

    hygiene_df <- subset(hygiene_df, select = c("datum","e.coli"))
  }

  log_transform <- function (df, y) {

    if (! y) {

      return(df)
    }

    df[,-1] <- lapply(df[, -1], function(x) log10(x + 1))

    names(df)[-1] <- paste0("log_", names(df)[-1])

    df
  }

  typedata2 <- if (length(typedata) > 1) {

    mapply(FUN = log_transform, typedata, log[-1])

  } else {

    lapply(typedata, log_transform, y = log[-1])
  }

  time_x <- hygiene_df$datum

  # basic choosing (with number of complete observations)
  basic <- character(0)

  for (df in typedata2) {

    # possible predictors
    variables <- names(df)[-1]

    for (colname in variables) {

      n_obs <- sum(! is.na(subset(df, subset = df$datum %in% time_x)[[colname]]))

      message <- paste0(colname, ": ", n_obs, "/", length(time_x), "? (y/n): ")

      yn <- readline(prompt = message)

      if (yn != "y") {

        variables[variables == colname] <- NA
      }
    }

    basic <- c(basic, variables[! is.na(variables)])
  }

  if (length(basic) == 0) {

    stop("Need more parameters for modelling.", call. = FALSE)
  }

  # choose distance of temporal lag
  days <- as.numeric(readline(
    prompt = "Calculate lagdays up to how many days?: "
  ))

  stopifnot(any(days == 1:10))

  method <- readline(
    prompt = "Which correlation method? (pearson, kendall, spearman): "
  )

  stopifnot(method %in% c("pearson", "kendall", "spearman"))

  # choose lagdays
  expand <- character(0)

  for (df in typedata2) {

    for (colname in names(df)[-1]) {

      if (colname %in% basic) {

        # unroll lagdays of chosen variables
        unrolled_df <- unroll_lagdays(
          subset(df, select = c("datum", colname)), n = days
        )

        df2 <- merge(hygiene_df, unrolled_df, by = "datum")[, -1]

        # print scatterplots
        print(correlation_scatterplot(df2, method))

        # possible lagdays
        lagdays <- names(df2)[-1]

        for (var in lagdays) {

          yn <- readline(prompt = paste0(var, "?: (y/n)"))

          if (yn != "y") {

            lagdays[lagdays == var] <- NA
          }
        }

        expand <- c(expand, lagdays[! is.na(lagdays)])
      }
    }
  }

  message("The final model predictors are:")
  cat(paste0(expand, collapse = " \n"))

  if (length(expand) < 3) {

    warning("Less than 3 predictors chosen, stan_lm needs 3 at least", call. = F)
  }

  c(names(hygiene_df)[2], expand)
}

# process_model_riverdata ------------------------------------------------------
#'
#' \code{process_model_riverdata}: builds a data.frame out of the given
#' variables as char-string. This data.frame can be passed to the data argument
#' of a model function alongside with \code{eval(create_formula(variables))} to
#' the formula argument. Interaction char-strings get omitted automatically.
#'
#' @describeIn build_model Internal usage
#' @return Returns a data.frame with data for hygiene and chosen variables
#' @export
#'
#' @examples
#' \dontrun{variables <- c("e.coli","q_havel",...)
#' lm(formula = eval(create_formula(variables)),
#' data = process_model_riverdata(riverdata, variables))}
#'

process_model_riverdata <- function(riverdata, variables)
{
  hygiene_df <- riverdata[[grep("hygiene", names(riverdata))]]
  typedata <- riverdata[! grepl("hygiene", names(riverdata))]

  variables <- variables[! grepl(":", variables)]

  # e.coli log-transformed?
  log <- grepl("^log", variables[1])

  # any data log-transformed?
  prefix <- sub("^(log_)?([a-z]{1,3})_.*", "\\2", variables[-1])
  log_prefix <- unique(prefix[grepl("^log", variables[-1])])
  data_prefix <- sub("^([a-z]{1,3})_.*", "\\1", names(typedata))
  log <- c(log, data_prefix %in% log_prefix)

  names(log) <- c("e.coli", names(typedata))

  if (log[["e.coli"]]) {
    hygiene_df$log_e.coli <- log10(hygiene_df$e.coli)
    model_list <- list(hygiene = subset(hygiene_df, select = c("datum","log_e.coli")))
  } else {
    model_list <- list(hygiene = subset(hygiene_df, select = c("datum","e.coli")))
  }

  typedata2 <- purrr:map2(.f = function (df, y) {
    if(!y) return(df)
    df[,-1] <- lapply(df[,-1], function(x) {log10(x + 1)})
    names(df)[-1] <- paste0("log_", names(df)[-1])
    return(df)
  }, .x = typedata, .y = log[-1])

  unrolled_typedata <- unroll_physical_data(typedata2)

  for (df in unrolled_typedata){
    if(any(select_x <- names(df) %in% variables)){
      model_list[[max(names(df)[select_x])]] <- df[,c(TRUE, select_x[-1])]
    }
  }

  model_df <- model_list[[1]]
  for (df in model_list[-1])
    model_df <- merge(model_df, df, by = "datum")

  return(model_df)
}

# create_formula ---------------------------------------------------------------
#'
#' \code{create_formula}: constructs and parses a formula from given variables
#' Builds a formula out of the given variables with the form
#' \code{e.coli ~ q * (ka + r)}
#'
#' @param variables names of variables
#' @param with_interaction logical. Formula with interactions? Default set to
#'   TRUE
#' @describeIn build_model Internal usage
#' @return Returns parsed model-formula. (Like model$formula)
#' @export
#'
#' @examples
#' create_formula(c("log_e.coli","q_havel","ka_ruhleben","r_berlin"))
#' create_formula(c("e.coli","r_mitte","r_charlottenburg","r_spandau"))
#'
create_formula <- function(variables, with_interaction = FALSE)
{
  ziel <- variables[grep("e.coli", variables)]

  rest <- setdiff(variables, ziel)

  q_vars <- rest[grep("^(log_)?q_", rest)]

  i_vars <- rest[grep("^i_", rest)]

  sd_vars <- rest[grep("^sd_", rest)]

  rest_vars <- setdiff(rest, c(q_vars, i_vars, sd_vars))

  if (length(q_vars) > 0 && with_interaction) {

    formula_string <- paste0(collapse = "", c(
      ziel,
      "~",
      paste0(q_vars, collapse = "*"),
      "*(",
      paste0(rest_vars, collapse = "+"),
      ")"
    ))

    if (length(i_vars) + length(sd_vars) > 0) {

      to_add <- paste0(i_vars, sd_vars, collapse = "+")
      formula_string <- paste(formula_string, to_add, sep = "+")
    }

  } else {

    formula_string <- paste0(
      collapse = "", ziel, "~", paste0(rest, collapse = "+")
    )
  }

  stats::as.formula(formula_string)
}

# present_model ---------------------------------------------------------------
# dev: present_model <- function # show model features for decision making