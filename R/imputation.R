# library(sinew)
# makeOxygen(pad_data)
"%!in%" <- Negate("%in%")

#' @title pad_data
#' @description Adds in any gaps in a data frame representing a time series
#' @param df A data frame
#' @param by Time interval of series, Default: '30 min'
#' @param date_field Column name for POSIX date/time variable in df, Default: 'DATECT'
#' @param v_dates A vector of POSIX date/times, potentially from another df, to match with it. Default: 'df$DATECT'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  df <- pad_data(df)
#'  }
#' }
#' @rdname pad_data
#' @export 
pad_data <- function(df, by = "30 min", date_field = "DATECT", v_dates = NULL){
  df <- as.data.frame(df)
  df$DATECT <- df[, date_field]
  first <- min(v_dates, na.rm = TRUE)
  last  <- max(v_dates, na.rm = TRUE)
  # make a dt with complete time series with interval "by"
  dt_date <- data.table(DATECT = seq.POSIXt(first, last, by = by))
  dt <- as.data.table(df)
  dt <- dt[dt_date, on = .(DATECT = DATECT)] 
  df <- as.data.frame(dt)
  return(df)
}

#' @title impute
#' @description Impute missing values using various methods
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
#' if(interactive()) {
#'  #EXAMPLE1
#' l_gf <- list(df = df, df_qc = df_qc)
#' l_gf <- impute(y = "SW_IN", x = "PPFD_IN",  l_gf)
#'  }
#' }
#' @rdname impute_by_regn
#' @export
impute <- function(y, l_gf = l_gf, method = "era5", qc_tokeep = 0,
  selection = TRUE, date_field = "DATECT", k = 40,
  fit = TRUE, x = NULL, df_era5 = NULL,
  lat = 55.792, lon = -3.243, plot_graph = TRUE
  ) {

  df_method <- data.frame(
    method = c(
      "missing",
      "time",
      "regn",
      "nightzero",
      "noneg",
      "zero",
      "era5"
    ),
    qc = c(1, 2, 3, 4, 5, 6, 7) # 0 = raw, 1 = missing
  )
  # saveRDS(df_method, file = here("data", "df_method.rds"))
  method <- match.arg(method, df_method$method)
  # get the qc code for the selected method
  qc <- df_method$qc[match(method, df_method$method)]

  df    <- l_gf$df
  df_qc <- l_gf$df_qc
  
  # df_qc[, y][which(i_sel)]
  # table(i_sel)
  # table(df[, y] < 0)
  # table(df_qc[, y])
  # indices of values to change
  # default
  # qc_tokeep typically set to 0, so this selects any other value (missing or imputed)
  # selection optionally adds those selected in the metdb app ggiraph plots
  # isel = TRUE  = missing, imputed (AND selected)
  # isel = FALSE = raw
  i_sel <- df_qc[, y] %!in% qc_tokeep & selection
  if (method == "noneg") i_sel <- i_sel & df[, y] < 0
  if (method == "nightzero") {
    df$date <- df[, date_field] # needs to be called "date" for openair functions
    df <- cutData(df, type = "daylight", latitude = lat, longitude = lon)
    i_sel <- i_sel & df$daylight == "nighttime"
    df$daylight <- NULL
    # if date is not the original variable name, delete it - we don't want an extra column
    if (date_field != "date") df$date <- NULL
  }
 
  # calculate replacement values depending on the method
  # if a constant zero
  if (method == "nightzero" | method == "noneg" | method == "zero") {
    df[, y]   [i_sel] <- 0
  } else if (method == "time") {
    v_date  <- df[, date_field]
    datect_num <- as.numeric(v_date)  ## !df_qry$
    hour       <- as.POSIXlt(v_date)$hour
    yday       <- as.POSIXlt(v_date)$yday
    n_yday     <- length(unique(yday))
    k_yday     <- as.integer(n_yday / 2)

    m <- gam(df[, y] ~ s(datect_num, k = k, bs = "cr") +
                   s(yday, k = k_yday, bs = "cr") +
                   s(hour, k = -1, bs = "cc"),
      na.action = na.exclude #, data = df
    )
    v_pred <- predict(m, newdata = data.frame(datect_num, hour, yday))
    df[, y][i_sel] <- v_pred[i_sel]
  } else if (method == "regn" | method == "era5") {
    if (method == "era5") {
      v_x <- df_era5[, y] # use ERA5 data
    } else {
      v_x <- df[, x]  # use x variable in the CEDA data
    }
    if (fit) {
      dft <- data.frame(y = df[, y], x = v_x)
      # exclude indices i_sel i.e. do not fit to those we are replacing
      dft$y[i_sel] <- NA
      m <- lm(y ~ x, data = dft, na.action = na.exclude)
      v_pred <- predict(m, newdata = dft)
    } else {  # or just replace y with x
      v_pred <- v_x
    }
    df[, y][i_sel] <- v_pred[i_sel]
  }
  
  # add code for each replaced value in the qc df
  df_qc[, y][i_sel] <- qc
  
  if (plot_graph) {                  
    dft <- data.frame(date = df[, date_field], y = df[, y], qc = df_qc[, y])
    p <- ggplot(dft, aes(date, y))
    #p <- p + geom_line()
    if (method == "era5") { # include era5 data in plot
      p <- p + geom_point(data = df_era5, 
        aes(x = df_era5[, date_field], y = df_era5[, y]), 
        colour = "black", size = 1)
    }
    p <- p + geom_point(aes(y = y, colour = factor(qc)), size = 1) + ylab(y)
    print(p)
  }
  return(list(df = df, df_qc = df_qc))
}


#' library(lintr)
#' lint(here("R/imputation.R"), linters = with_defaults(line_length_linter = line_length_linter(120)))

# p <- ggplot(dft, aes(x, y))
# if (method == "era5") { # include era5 data in plot
  # p <- p + geom_point(data = df_era5, 
    # aes(x = df_era5[, date_field], y = df_era5[, y]), 
    # colour = "black", size = 1)
# }
# p <- p + geom_point(aes(colour = factor(!i_sel)), size = 1)
# p