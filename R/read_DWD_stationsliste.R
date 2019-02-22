read_dwd_stationsliste <- function (path) {
  con_dwd <- file(path)
  dwd_chr <- readLines(con = con_dwd)
  close(con_dwd)

  header <- unlist(strsplit(dwd_chr[1], " "))
  dwd_chr <- dwd_chr[-c(1,2)]
  dwd <- data.frame(
    sub("(^.{5}).*", "\\1", dwd_chr), # id
    sub("^.{6}(.{8}).*", "\\1", dwd_chr), # von
    sub("^.{15}(.{8}).*", "\\1", dwd_chr), # bis
    sub("^.{35}(.{3}).*", "\\1", dwd_chr), # hoehe
    sub("^.{43}(.{7}).*", "\\1", dwd_chr), # breite
    sub("^.{53}(.{7}).*", "\\1", dwd_chr), # laenge
    sub("^.{61}(.{40}).*", "\\1", dwd_chr), # name
    sub("^.{102}(.{40}).*", "\\1", dwd_chr), # bundesland
    stringsAsFactors = F)

  names(dwd) <- header

  dwd$von <- as.Date(dwd$von_datum, format = "%Y%m%d")
  dwd$bis <- as.Date(dwd$bis_datum, format = "%Y%m%d")
  dwd$hoehe <- as.numeric(dwd$Stationshoehe)
  dwd$lat <- as.numeric(dwd$geoBreite)
  dwd$lon <- as.numeric(dwd$geoLaenge)

  return(dwd)
}

# dwd_rr <- read_dwd_stationsliste("scrape_internet/DWD_RR_Tageswerte_Beschreibung_Stationen.txt")
# dwd_kl <- read_dwd_stationsliste("scrape_internet/DWD_KL_Tageswerte_Beschreibung_Stationen.txt")
# dwd_st <- read_dwd_stationsliste("scrape_internet/DWD_ST_Tageswerte_Beschreibung_Stationen.txt")

find_neighbor <- function (lat, lon, dwd, n) {

  dwd$r <- 1.852*60*sqrt(((lat - dwd$lat))^2 + ((lon - dwd$lon)*cos(pi*lat/180))^2)

  dwd <- dplyr::filter(dwd, dwd$bis >= as.Date("2017-11-30"))

  dplyr::arrange(dwd, .data$r)[1:n, ]
}
#
# get_dwd_data_kl <- function (id, name, dir, per = "historical", from = "2004-01-01") {
#   link <- rdwd::selectDWD(id = id, res = "daily", var = "kl", per = per)
#   df <- rdwd::dataDWD(link, read = T, dir = dir, quiet = T)
#   df2 <- dplyr::transmute(df, datum = lubridate::ymd(MESS_DATUM), r = RSK, sd = SDK)
#   df3 <- dplyr::filter(df2, datum > lubridate::ymd("2004-01-01"))
#   names(df3)[-1] <- paste0(names(df3)[-1], "_", name)
#   return(df3)
# }
#
# get_dwd_data_rr <- function (id, name, dir, per = "historical", from = "2004-01-01") {
#   link <- rdwd::selectDWD(id = id, res = "daily", var = "more_precip", per = per)
#   df <- rdwd::dataDWD(link, read = T, dir = dir, quiet = T)
#   df2 <- dplyr::transmute(df, datum = lubridate::ymd(MESS_DATUM), r = RSK)
#   df3 <- dplyr::filter(df2, datum > lubridate::ymd("2004-01-01"))
#   names(df3)[-1] <- paste0(names(df3)[-1], "_", name)
#   return(df3)
# }
#
# get_dwd_data <- function (id, name, dir, var = "more_precip", per = "historical", from = "2004-01-01") {
#   link <- rdwd::selectDWD(id = id, res = "daily", var = var, per = per)
#   df <- rdwd::dataDWD(link, read = T, dir = dir, quiet = T)
#   df2 <- dplyr::transmute(df, datum = lubridate::ymd(MESS_DATUM), r = RSK,
#                           if(var == "kl"){sd = SDK})
#   df3 <- dplyr::filter(df2, datum > lubridate::ymd(from))
#   names(df3)[-1] <- paste0(names(df3)[-1], "_", name)
#   return(df3)
# }
