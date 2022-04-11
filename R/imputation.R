#' @title impute_by_time
#' @description Impute missing values using a smoothing regression against time 
#'   with a General Additive Model (GAM)
#' @param y Response variable with missing values to be replaced (variable name 
#'   as a "quoted string")
#' @param l_gf List of two data frames containing data and qc codes. Default: l_gf
#' @param qc qc code to denote values imputed by this function, Default: 2
#' @param k Number of knots determining smoothness of fit; more knots gives a 
#'   more flexible (wiggly) fit, Default: -1 (= attempts to automatically optimise)
#' @return List of two data frames containing data and qc codes with imputed values.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#' l_gf <- list(df = df, df_qc = df_qc)
#' l_gf <- impute_by_time(y = "G", l_gf, k = 40)
#'  }
#' }
#' @rdname impute_by_time
#' @export 
impute_by_time <- function(y, l_gf = l_gf, qc = 2, k = -1, selection = TRUE){
  df    <- l_gf$df
  df_qc <- l_gf$df_qc
  
  v_y <- df[, y]
  datect_num <- as.numeric(df$DATECT)  ## !df_qry$
  hour       <- as.POSIXlt(df$DATECT)$hour
  
  m <- gam(v_y ~ s(datect_num, k = k, bs = "cr") +
                 s(hour, bs = "cc"),
    na.action = na.exclude #, data = df
  )
  v_pred <- predict(m, newdata = data.frame(datect_num, hour))

  # indices of values to change
  i_sel <- df_qc[, y] != 0 & selection

  df[, y][i_sel] <- v_pred[i_sel]
  # add code for each replaced value in the qc df
  df_qc[, y][i_sel] <- qc
  
  dft <- data.frame(date = df$date, y = df[, y], qc = df_qc[, y])
  p <- ggplot(dft, aes(date, y, colour = factor(qc)))
  p <- p + geom_point() + ylab(y)
  print(p)
  return(list(df = df, df_qc = df_qc))
}

#' @title impute_by_regn
#' @description Impute missing values using regression with a covariate
#' @param y Response variable with missing values to be replaced 
#'   (variable name as a "quoted string")
#' @param x Covariate to be used (name of a variable in the same data frame as 
#'   a "quoted string")
#' @param l_gf List of two data frames containing data and qc codes. Default: l_gf
#' @param qc qc code to denote values imputed by this function, Default: 3
#' @param fit Whether to fit a linear model or directly replace missing y with 
#'   x values, Default: TRUE
#' @return List of two data frames containing data and qc codes with imputed values.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#' l_gf <- list(df = df, df_qc = df_qc)
#' l_gf <- impute_by_regn(y = "SW_IN", x = "PPFD_IN",  l_gf)
#'  }
#' }
#' @rdname impute_by_regn
#' @export
impute_by_regn <- function(y, x, l_gf = l_gf, qc = 3, fit = TRUE, selection = TRUE){
  df    <- l_gf$df
  df_qc <- l_gf$df_qc
  v_y <- df[, y]
  v_x <- df[, x]
  if (fit){ # fit a linear regression
    m <- lm(v_y ~ v_x, na.action = na.exclude, data = df)
    v_pred <- predict(m, newdata = data.frame(v_x))
  } else {  # or just replace y with x
    v_pred <- v_x
  }
  
  # indices of values to change
  i_sel <- df_qc[, y] != 0 & selection
  
  df[, y][i_sel] <- v_pred[i_sel]
  # add code for each replaced value in the qc df
  df_qc[, y][i_sel] <- qc
   
  dft <- data.frame(date = df$date, y = df[, y], qc = df_qc[, y])
  p <- ggplot(dft, aes(date, y, colour = factor(qc)))
  p <- p + geom_point() + ylab(y)
  print(p)
  
  return(list(df = df, df_qc = df_qc))
}

#' @title impute_nightzero
#' @description Impute missing night-time values by setting them to zero
#' @param y Response variable with missing values to be replaced 
#'   (variable name as a "quoted string")
#' @param l_gf List of two data frames containing data and qc codes. Default: l_gf
#' @param qc qc code to denote values imputed by this function, Default: 4
#' @param lat Latitude of site location, Default: 55.792
#' @param lon Longitude of site location, Default: -3.243
#' @return List of two data frames containing data and qc codes with imputed values.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#' l_gf <- list(df = df, df_qc = df_qc)
#' l_gf <- impute_nightzero(y = "PPFD_IN",  l_gf)
#'  }
#' }
#' @rdname impute_nightzero
#' @export
impute_nightzero <- function(y, l_gf = l_gf, qc = 4, lat = 55.792, lon = -3.243, 
  selection = TRUE, date_field = "DATECT"){ 
  # lat/lon defaults to Auchencorth 
  df    <- l_gf$df
  df_qc <- l_gf$df_qc
  v_y <- df[, y]
  
  df$date <- df[, date_field]
  df <- cutData(df, type = "daylight", latitude = lat, longitude = lon)
  
  # indices of values to change
  i_sel <- df_qc[, y] != 0 & selection & df$daylight == "nighttime"
  
  df[, y]   [i_sel] <- 0
  # add code for each replaced value in the qc df
  df_qc[, y][i_sel] <- qc
  
  dft <- data.frame(date = df$date, y = df[, y], qc = df_qc[, y])
  p <- ggplot(dft, aes(date, y, colour = factor(qc)))
  p <- p + geom_point() + ylab(y)
  print(p)
  
  df$daylight <- NULL
  return(list(df = df, df_qc = df_qc))
}

#' @title impute_noneg
#' @description Impute (initially missing) negative values by setting them to 
#'   zero. This is mainly to correct other imputation methods when they erroneously 
#'   produce ngative values.
#' @param y Response variable with missing values to be replaced 
#'   (variable name as a "quoted string")
#' @param l_gf List of two data frames containing data and qc codes. Default: l_gf
#' @param qc qc code to denote values imputed by this function, Default: 5
#' @return List of two data frames containing data and qc codes with imputed values.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#' l_gf <- list(df = df, df_qc = df_qc)
#' l_gf <- impute_noneg(y = "PPFD_IN",  l_gf)
#'  }
#' }
#' @rdname impute_noneg
#' @export
impute_noneg <- function(y, l_gf = l_gf, qc = 5, selection = TRUE){ 
  df    <- l_gf$df
  df_qc <- l_gf$df_qc
  v_y <- df[, y]
  
  # indices of values to change
  i_sel <- df_qc[, y] != 0 & selection & df[, y] < 0
  
  # add code for each replaced value in the qc df
  df_qc[, y][i_sel] <- qc
  # and only then replace the values in the df 
  # bcos condition depends on these values
  df[, y]   [i_sel] <- 0
  
  dft <- data.frame(date = df$date, y = df[, y], qc = df_qc[, y])
  p <- ggplot(dft, aes(date, y, colour = factor(qc)))
  p <- p + geom_point() + ylab(y)
  print(p)
  return(list(df = df, df_qc = df_qc))
}


#' @title impute_zero
#' @description Impute missing values by setting them to zero.
#' @param y Response variable with missing values to be replaced 
#'   (variable name as a "quoted string")
#' @param l_gf List of two data frames containing data and qc codes. Default: l_gf
#' @param qc qc code to denote values imputed by this function, Default: 6
#' @return List of two data frames containing data and qc codes with imputed values.
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#' l_gf <- list(df = df, df_qc = df_qc)
#' l_gf <- impute_zero(y = "PPFD_IN",  l_gf)
#'  }
#' }
#' @rdname impute_zero
#' @export
impute_zero <- function(y, l_gf = l_gf, qc = 6, selection = TRUE){ 
  df    <- l_gf$df
  df_qc <- l_gf$df_qc
  v_y <- df[, y]

  # indices of values to change
  i_sel <- df_qc[, y] != 0 & selection

  df[, y]   [i_sel] <- 0
  # add code for each replaced value in the qc df
  df_qc[, y][i_sel] <- qc
  
  dft <- data.frame(date = df$date, y = df[, y], qc = df_qc[, y])
  p <- ggplot(dft, aes(date, y, colour = factor(qc)))
  p <- p + geom_point() + ylab(y)
  print(p)
  return(list(df = df, df_qc = df_qc))
}
