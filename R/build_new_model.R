# build_new_model --------------------------------------------------------------
#'
#' Create a new Model
#'
#' Main function for creation, object handling and saving of new models.
#' The models will be saved in a own subdirectory as r-objects as a side-effect.
#' The models will not be returned. They have to be loaded by other functions.
#'
#' @param river character. river you want to build a model on
#'
#' @return This function returns merely a message what happend.
#' @export
#'

build_new_model <- function(river){
  paths <- get_paths()

  river_dir <- grep(river, names(paths), value = T)
  if (length(river_dir) < 1)
    return(print(paste0("Could not find directory for ", river, " on server")))

  riverdata <- import_riverdata(paths[[river_dir[3]]])

  model <- build_model(riverdata)

  # present_model(model)
  print(summary(model))

  answer <- readline(prompt = "Do you want to save this model? (y/n): ")
  if (answer != "y")
    return(print("No new model created"))

  if(!any(grepl("model", river_dir)))
    dir.create(model_dir <- paste0(paths[[river_dir[2]]], "/DATA_model_binary"))
  else
    model_dir <- paths[[river_dir[4]]]

  modelcount <- length(dir(model_dir)) + 1
  modelname <- paste0(river, modelcount, "stan")
  assign(modelname, model)
  save(list = modelname, file = paste0(model_dir, "/", modelname, ".RData"))

  return(print(paste0("A new model has been created: ", modelname)))
}
