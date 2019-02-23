# get_dwd_urls_metadata --------------------------------------------------------

#' Get URLs to DWD Metadata
#'
#' @return list of character with each element representing a URL to a metadata
#'   file provided by Deutscher Wetterdienst (DWT), e.g. files
#'   "_Beschreibung_Stationen.txt" describing measurement stations.
#'
#' @export
#'
get_dwd_urls_metadata <- function()
{
  # Describe URLs in the form of a "grammar"
  url_grammar <- list(
    stationen = "<climate>/daily/<category>/<subdir><metafile_daily>",
    climate = "ftp://ftp-cdc.dwd.de/pub/CDC/observations_germany/climate",
    metafile_daily = "<category_prefix>_Tageswerte_Beschreibung_Stationen.txt"
  )

  # Mapping between <category_prefix> and <category>.
  # Note: The URL for category does not have a <subdir>
  categories <- list(
    KL = c("historical/", "kl"),
    Wa = c("historical/", "water_equiv"),
    RR = c("historical/", "more_precip"),
    ST = c("", "solar")
  )

  # Provide the prefixes of the categories. We will loop through them next
  prefixes <- names(categories)

  # Resolve the "stationen" entry from the grammar by setting the placeholders
  # according to the mapping in the list "categories"
  urls <- lapply(prefixes, function(prefix) {
    kwb.utils::resolve(
      "stationen",
      url_grammar,
      category = categories[[prefix]][2],
      subdir = categories[[prefix]][1],
      category_prefix = prefix
    )
  })

  # Name the list elements "stationen_<category>"
  names(urls) <- paste0("stationen_", tolower(sapply(categories, "[", 2)))

  # Return the list of URLs
  urls
}
