# process_model_riverdata_hs ---------------------------------------------------
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

process_model_riverdata_hs <- function(riverdata, variables)
{
  stopifnot(is.list(riverdata))

  is_hygiene <- grepl("hygiene", names(riverdata))

  # We expect exactly one hygiene data frame
  stopifnot(sum(is_hygiene) == 1)

  # Extract the hygiene data frame
  hygiene_df <- riverdata[[is_hygiene]]

  # Keep all data frames except the hygiene data in a list "typedata"
  typedata <- riverdata[! is_hygiene]

  # Exclude variable names containing ":" (= interaction variables)
  variables <- variables[! grepl(":", variables)]

  # First variable is expected to be e.coli or log(e.coli)
  stopifnot(grepl("e\\.coli", variables[1]))

  # is e.coli log-transformed?
  is_log <- grepl("^log", variables[1])

  # any data log-transformed?
  prefix <- sub("^(log_)?([a-z]{1,3})_.*", "\\2", variables[-1])
  log_prefix <- unique(prefix[grepl("^log", variables[-1])])
  data_prefix <- sub("^([a-z]{1,3})_.*", "\\1", names(typedata))

  is_log <- c(is_log, data_prefix %in% log_prefix)

  names(is_log) <- c("e.coli", names(typedata))

  if (is_log[["e.coli"]]) {
    hygiene_df$log_e.coli <- log10(hygiene_df$e.coli)
  }

  e_coli_prefix <- ifelse(is_log[["e.coli"]], "log_", "")
  columns <- c("datum", paste0(e_coli_prefix, "e.coli"))

  model_list <- list(hygiene = kwb.utils::selectColumns(hygiene_df, columns))

  # Select log-transformed data frames except the first
  indices <- setdiff(which(is_log), 1L)

  typedata[indices] <- lapply(typedata[indices], function(df) {

    columns <- names(df)[-1]

    df[columns] <- lapply(df[columns], function(x) log10(x + 1))

    column_prefixes <- c("", "log_")

    stats::setNames(df, paste0(column_prefixes, names(df)))
  })

  unrolled_typedata <- unroll_physical_data(typedata)

  for (df in unrolled_typedata) {

    var_columns <- intersect(names(df), variables)

    if (length(var_columns) > 0) {

      model_list[[max(var_columns)]] <- df[, c("datum", var_columns)]
    }
  }

  # Merge all data frames by the date column
  kwb.utils::mergeAll(model_list, by = "datum")
}
