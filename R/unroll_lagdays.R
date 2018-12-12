# unroll_physical_data ---------------------------------------------------------
#'
#' Unroll Lagdays of Data
#'
#' Unrolls the lagdays of data.frames.
#'
#' @param physical_data list of river data (without hygiene)
#' @param df data.frame of 2 columns: datum and var
#' @param n numeric. unto to which day shall be lagged behind?
#'
#' @return Returns a list of data.frames for each variable. The data.frames
#' contain the unrolled lagdays (with maxday = 5, length(df) == 17)
#' @export
#'
#' @examples
#' df1 <- data.frame(datum = rep("egal", 25), var = 1:25)
#' df2 <- data.frame(datum = rep("egal", 25), var2 = 51:75, var3 = 101:125)
#' unroll_lagdays(df1)
#' summary(unroll_physical_data(list(var1 = df1, var2 = df2)))
#'

unroll_physical_data <- function(physical_data){
  unrolled_list <- lapply(physical_data, unroll_lagdays)
}

# unroll_lagdays ---------------------------------------------------------------
#'
#' \code{unroll_lagdays} is the mere unroll of a data.frame. See examples
#'
#' @describeIn unroll_physical_data Internal usage mostly
#' @export
#' @importFrom dplyr lag
#' @importFrom tibble tibble

unroll_lagdays <- function(df, n = 5){
  stopifnot(n > 0)
  df2 <- subset(df, select = -datum)

  selectvector <- c(rep(1, n), rep(NA, n-1))
  selectmatrix <- !is.na(unroll_vector2lagmatrix(selectvector, n)[-(1:2),])

  if (n > 3){
    for (i in (n-2):2){
      selectvector <- c(rep(1, i), rep(NA, n-1))
      selectmatrix <- unique(rbind(selectmatrix,
                                   !is.na(unroll_vector2lagmatrix(selectvector,
                                                                  n)[-(1:2),])))
    }
  }

  result <- tibble::tibble(datum = df$datum)

  for (icol in seq_along(df2)){
    df3 <- data.frame(matrix(NA, ncol = 1+n*(n+1)/2, nrow = nrow(df2)))

    mat1 <- unroll_vector2lagmatrix(df2[[icol]], n = n)
    mat2 <- apply(selectmatrix, 1, function(a) (mat1 %*% a)/sum(a))

    df3[,1] <- df2[[icol]]
    df3[,2:(n+1)] <- mat1
    df3[,-(1:(n+1))] <- mat2

    varname <- names(df2)[icol]
    varname_abs <- paste0(varname, "_abs_", 1:n)
    varname_mean <- paste0(varname, "_mean_",
                           apply(selectmatrix, 1, function(a)
                             paste(as.character(1:n)[a], collapse = "")))
    names(df3) <- c(varname, varname_abs, varname_mean)

    result <- cbind(result, df3)
  }

  return(result)
}

unroll_vector2lagmatrix <- function(x, n){
  mat <- matrix(0, ncol = n, nrow = length(x))
  for(i in 1:n)
    mat[,i] <- dplyr::lag(x, n = i)
  return(mat)
}
