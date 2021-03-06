read_dwd_xls <- function(xls_path, variablename)
{
  # der DWD bezieht bei Stundendaten auf MEZ (UTC+1)
  df <- readxl::read_excel(
    path = xls_path, skip = 1,
    col_types = c("text","text"),
    col_names = c("time", "prcptext")
  )

  df <- dplyr::mutate(
    df,
    datum = lubridate::as_date(
      lubridate::ymd_h(df$time, tz = "Etc/GMT-1"), tz = "Etc/GMT-1"
    ),
    prcp = replace(as.numeric(df$prcptext), which(df$prcptext == "-999"), NA)
  )

  dplyr::summarise_(
    dplyr::group_by(df, df$datum),
    .dots = stats::setNames(nm = variablename, c(
      "mean(prcp, na.rm = T) * sum(!is.na(prcp))"
    ))
  )
}
