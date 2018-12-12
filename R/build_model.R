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
#'
#' @return Returns a model of the riverdata.
#' @export
#'
#' @importFrom rstanarm stan_lm R2
#'

build_model <- function(riverdata, variables = ask_for_variables(riverdata)){
  model_riverdata <- process_model_riverdata(riverdata, variables)

  model_formula <- create_formula(variables)

  model <- do.call(rstanarm::stan_lm, list(model_formula, data = model_riverdata,
                                           prior = rstanarm::R2(location = 0.8),
                                           adapt_delta = 0.99999))
  return(model)
}

# ask_for_variables ------------------------------------------------------------
#'
#' \code{ask_for_variables}: asks the user for variable selection. Possible
#' variables are all variables in riverdata.
#'
#' @describeIn build_model Internal function. Quite time consuming
#' @return Returns a character-vector with the chosen model variables
#'

ask_for_variables <- function(riverdata){
  # prepare data
  hygiene_df <- riverdata[[grep("hygiene", names(riverdata))]]
  typedata <- riverdata[!grepl("hygiene", names(riverdata))]

  # e.coli log-transformed?
  message("You now have to choose the variables for your model E.Coli ~ predictors")
  yn <- readline(prompt = "E.Coli as log10? (y/n): ")
  if (yn == "y") {log <- T} else {log <- F}

  # general selection and log-transformation
  for (type in names(typedata)){
    yn <- readline(prompt = paste0("Include ", type, "? (y/n): "))
    if (yn != "y") {
      typedata <- subset(typedata, subset = names(typedata) != type)
    } else {
      # variable log-transformed?
      yn <- readline(prompt =  paste(type,"as log10? (y/n): "))
      if (yn == "y") {log <- c(log,T)} else {log <- c(log,F)}
    }
  }

  # change log-transformations in data
  names(log) <- c("e.coli", names(typedata))
  if (log[["e.coli"]]) {
    hygiene_df$log_e.coli <- log10(hygiene_df$e.coli)
    hygiene_df <- subset(hygiene_df, select = c("datum","log_e.coli"))
  } else {
    hygiene_df <- subset(hygiene_df, select = c("datum","e.coli"))
  }

  typedata2 <- mapply(function (df, y) {
    if(!y) return(df)
    df[,-1] <- lapply(df[,-1], function(x) {log10(x + 1)})
    names(df)[-1] <- paste0("log_", names(df)[-1])
    return(df)
  }, typedata, log[-1])

  time_x <- hygiene_df$datum

  # basic choosing (with number of complete observations)
  basic <- character(0)
  for (df in typedata2){
      # possible predictors
      variables <- names(df)[-1]

      for (colname in variables){
        n_obs <- sum(!is.na(subset(df, subset = df$datum %in% time_x)[[colname]]))

        message <- paste0(colname, ": ", n_obs, "/", length(time_x), "? (y/n): ")
        yn <- readline(prompt = message)
        if(yn != "y")
          variables[variables == colname] <- NA
      }
      basic <- c(basic, variables[!is.na(variables)])
  }

  if (length(basic) == 0) {
    stop("Need more parameters for modelling.", call. = F)
  }

  # choose distance of temporal lag
  days <- as.numeric(readline(prompt = "Calculate lagdays up to how many days?: "))
  stopifnot(any(days == 1:10))

  method <- readline(prompt = "Which correlation method? (pearson, kendall, spearman): ")
  stopifnot(method %in% c("pearson", "kendall", "spearman"))

  # choose lagdays
  expand <- character(0)
  for (df in typedata2){
    for (colname in names(df)[-1]){
      if (colname %in% basic) {
        # unroll lagdays of chosen variables
        unrolled_df <- unroll_lagdays(subset(df, select = c("datum", colname)), n = days)
        df2 <- merge(hygiene_df, unrolled_df, by = "datum")[,-1]
        # print scatterplots
        print(correlation_scatterplot(df2, method))
        # possible lagdays
        lagdays <- names(df2)[-1]
        for (var in lagdays){
          yn <- readline(prompt = paste0(var, "?: (y/n)"))
          if (yn != "y")
            lagdays[lagdays == var] <- NA
        }
        expand <- c(expand, lagdays[!is.na(lagdays)])
      }
    }
  }
  message("The final model predictors are:")
  cat(paste0(expand, collapse = " \n"))

  if (length(expand) < 3)
    warning("Less than 3 predictors chosen, stan_lm needs 3 at least", call. = F)

  return(c(names(hygiene_df)[2],expand))
}

# process_model_riverdata ------------------------------------------------------
#'
#' \code{process_model_riverdata}: builds a data.frame out of the given
#' variables. This data.frame can be passed to the data argument of a
#' modelfunction alongside with \code{eval(create_formula(variables))} to
#' the formula argument
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

process_model_riverdata <- function(riverdata, variables){
  hygiene_df <- riverdata[[grep("hygiene", names(riverdata))]]
  typedata <- riverdata[!grepl("hygiene", names(riverdata))]

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

  typedata2 <- mapply(function (df, y) {
    if(!y) return(df)
    df[,-1] <- lapply(df[,-1], function(x) {log10(x + 1)})
    names(df)[-1] <- paste0("log_", names(df)[-1])
    return(df)
  }, typedata, log[-1])

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
#' @describeIn build_model Internal usage
#' @return Returns parsed model-formula. (Like model$formula)
#' @export
#'
#' @examples
#' create_formula(c("log_e.coli","q_havel","ka_ruhleben","r_berlin"))
#' create_formula(c("e.coli","r_mitte","r_charlottenburg","r_spandau"))
#'

create_formula <- function(variables){
  ziel <- variables[grep("e.coli", variables)]
  rest <- setdiff(variables, ziel)
  q_vars <- rest[grep("^(log_)?q_", rest)]
  rest_vars <- setdiff(rest, q_vars)

  if (length(q_vars)>0 && (yn <- readline(prompt="With interactions? (y/n): "))=="y") {
    ff <- as.formula(paste0(c(ziel, "~", paste0(q_vars,collapse="*"),
                                "*(", paste0(rest_vars,collapse="+"), ")"),
                              collapse = ""))
  } else {
    ff <- as.formula(paste0(ziel, "~", paste0(rest,collapse="+"), collapse=""))
  }
  return(ff)
}

# present_model ---------------------------------------------------------------
# dev: present_model <- function # show model features for decision making