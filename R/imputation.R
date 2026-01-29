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
pad_data <- function(dt, by = "30 min", date_field = "DATECT", v_dates = NULL) {
  setDT(dt)
  if (is.null(v_dates)) {
    v_dates <- dt[, ..date_field][[1]]
  }
  first <- min(v_dates, na.rm = TRUE)
  last <- max(v_dates, na.rm = TRUE)
  # make a dt with complete time series with interval "by"
  dt_date <- data.table(DATECT = seq.POSIXt(first, last, by = by))
  dt <- dt[dt_date, on = .(DATECT = DATECT)]
  return(dt)
}

#' @title detect_gaps
#' @description Detects any gaps in a data frame representing a time series
#' @param df A data frame
#' @param by Time interval of series, Default: '30 min'
#' @param date_field Column name for POSIX date/time variable in df, Default: 'DATECT'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  gaps <- detect_gaps(l_logr$df)
#'  }
#' }
#' @rdname detect_gaps
#' @export
detect_gaps <- function(dt, expected_interval = 30, date_field = "DATECT") {
  setDT(dt)
  v_dates <- dt[, ..date_field][[1]]
  dt[, date_curr := v_dates]
  dt[, date_prev := shift(date_curr, 1)]
  dt[, date_int := difftime(date_curr, date_prev, units = "mins")]
  dt$date_int[1] <- expected_interval
  v_longer <- sum(dt$date_int > expected_interval)
  v_shorter <- sum(dt$date_int < expected_interval)
  v_gaps <- which(dt$date_int != expected_interval)
  dt_gaps <- dt[v_gaps]
  return(list(
    v_longer = v_longer,
    v_shorter = v_shorter,
    v_gaps = v_gaps,
    dt_gaps = dt_gaps
  ))
}

#' @title impute
#' @description Impute missing values using various methods
#' @param y Response variable with missing values to be replaced
#'   (variable name as a "quoted string")
#' @param x Covariate to be used (name of a variable in the same data frame as
#'   a "quoted string")
#' @param l_met List of two data frames containing data and qc codes. Default: l_met
#' @param qc qc code to denote values imputed by this function, Default: 3
#' @param fit Whether to fit a linear model or directly replace missing y with
#'   x values, Default: TRUE
#' @return List of two data frames containing data and qc codes with imputed values.
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()) {
#'  #EXAMPLE1
#' l_met <- list(df = df, df_qc = df_qc)
#' l_met <- impute(y = "SW_IN", x = "PPFD_IN",  l_met)
#'  }
#' }
#' @rdname impute
#' @export
impute <- function(
  y,
  l_met = l_met,
  method = "era5",
  qc_tokeep = 0,
  selection = TRUE,
  date_field = "DATECT",
  k = 40,
  fit = TRUE,
  n_min = 10,
  x = NULL,
  df_era5 = NULL,
  lat = 55.792,
  lon = -3.243,
  plot_graph = TRUE
) {
  print(paste("Imputing", y))
  method <- match.arg(method, df_method$method)
  # get the qc code for the selected method
  qc <- df_method$qc[match(method, df_method$method)]

  dt <- l_met$dt
  dt_qc <- l_met$dt_qc
  dt_era5 <- l_met$dt_era5

  # how many non-missing data are there?
  n_data <- sum(!is.na(dt[, ..y][[1]]))
  # with very few/no data, just replace with era5 data rather than trying to fit a regression
  if (n_data <= n_min && method == "era5") {
    print(paste("Too few data to fit regression; using ERA5 data directly"))
    fit <- FALSE
  }
  # these methods don't work with very few/no data
  if (n_data <= n_min && (method == "time" || method == "regn")) {
    return(list(dt = dt, dt_qc = dt_qc, dt_era5 = dt_era5))
  }

  # dt_qc[, y][which(i_sel)]
  # table(i_sel)
  # table(dt[, y] < 0)
  # table(dt_qc[, y])
  # indices of values to change
  # default
  # qc_tokeep typically set to 0, so this selects any other value (missing or imputed)
  # selection optionally adds those selected in the metdb app ggiraph plots
  # isel = TRUE  = missing, imputed (AND selected)
  # isel = FALSE = raw
  i_sel <- dt_qc[, ..y][[1]] %!in% qc_tokeep & selection
  if (method == "noneg") {
    i_sel <- i_sel & dt[, y] < 0
  }
  if (method == "nightzero") {
    dt$date <- dt[, ..date_field] # needs to be called "date" for openair functions
    dt <- cutData(dt, type = "daylight", latitude = lat, longitude = lon)
    i_sel <- i_sel & dt$daylight == "nighttime"
    dt$daylight <- NULL
    # if date is not the original variable name, delete it - we don't want an extra column
    if (date_field != "date") dt$date <- NULL
  }

  # calculate replacement values depending on the method
  # if a constant zero
  if (method == "nightzero" | method == "noneg" | method == "zero") {
    dt[i_sel, eval(y) := 0]
  } else if (method == "time") {
    if (k > n_data / 4) {
      k <- as.integer(n_data / 4)
    }
    v_date <- dt[, ..date_field][[1]]
    datect_num <- as.numeric(v_date) ## !dt_qry$
    hour <- as.POSIXlt(v_date)$hour
    # yday <- as.POSIXlt(v_date)$yday
    # n_yday <- length(unique(yday))
    # k_yday <- as.integer(n_yday / 2)

    m <- gam(
      dt[, ..y][[1]] ~
        s(datect_num, k = k, bs = "cr") +
        # s(yday, k = k_yday, bs = "cr") +
        s(hour, k = -1, bs = "cc"),
      na.action = na.exclude #, data = dt
    )
    v_pred <- predict(m, newdata = data.frame(datect_num, hour))
    dt[i_sel, y] <- v_pred[i_sel]
  } else if (method == "regn" || method == "era5") {
    if (method == "era5") {
      v_x <- dt_era5[, ..y][[1]] # use ERA5 data
    } else {
      v_x <- dt[, ..x][[1]] # use x variable in the CEDA data
    }
    if (fit) {
      dtt <- data.frame(y = dt[, ..y][[1]], x = v_x)
      # exclude indices i_sel i.e. do not fit to those we are replacing
      dtt$y[i_sel] <- NA
      m <- lm(y ~ x, data = dtt, na.action = na.exclude)
      v_pred <- predict(m, newdata = dtt)
    } else {
      # or just replace y with x
      v_pred <- v_x
    }
    dt[i_sel, eval(y) := v_pred[i_sel]]
  }

  # add code for each replaced value in the qc dt
  dt_qc[i_sel, eval(y) := qc]

  if (plot_graph) {
    dtt <- data.table(
      date = dt[, ..date_field][[1]],
      y = dt[, ..y][[1]],
      qc = dt_qc[, ..y][[1]]
    )
    p <- ggplot(dtt, aes(date, y))
    # if (method == "era5") { # include era5 data in plot
    p <- p +
      geom_line(
        data = dt_era5,
        aes(x = dt_era5[, ..date_field][[1]], y = dt_era5[, ..y][[1]]),
        colour = "black"
      )
    # }
    p <- p + geom_point(aes(y = y, colour = factor(qc)), size = 1) + ylab(y)
    fname <- paste0("plot_", y, "_", method, ".png")
    ggsave(p, filename = here("output", fname))
  }
  return(list(dt = dt, dt_qc = dt_qc, dt_era5 = dt_era5))
}

plot_with_qc <- function(y, l_met = l_met, date_field = "DATECT") {
  dt <- l_met$dt
  dt_qc <- l_met$dt_qc
  dtt <- data.frame(date = dt[, date_field], y = dt[, y], qc = dt_qc[, y])
  p <- ggplot(dtt, aes(date, y))
  p <- p + geom_point(aes(y = y, colour = factor(qc)), size = 1) + ylab(y)
  return(p)
}

add_era5 <- function(
  l_lev,
  fname_era5 = "/gws/ssde/j25a/eddystore/era5met/data-raw/era5/N55.75_W03.25/monthly/dt.csv",
  fname_names = here("data", "ERA5_to_ICOS.xlsx"),
  date_field = "DATECT"
) {
  # get era5 data
  df_names_era5 <- read_excel(fname_names, sheet = "mainmet_to_era5")
  dt_era5 <- rename_era5(fname_era5, df_names_era5)
  setDT(dt_era5)

  ##* WIP: should do this in the ERA5 workflow
  # make era5 half-hourly and pad to full days
  dt_era5 <- make_hhourly_and_pad_era5(
    dt_era5,
    v_dates = l_lev$dt[, get(date_field)]
  )

  # restrict obs data to date range of era5
  start_date <- min(dt_era5[, ..date_field][[1]])
  end_date <- max(dt_era5[, ..date_field][[1]])
  l_lev$dt <- l_lev$dt[
    get(date_field) >= start_date & get(date_field) <= end_date
  ]
  l_lev$dt_qc <- l_lev$dt_qc[
    get(date_field) >= start_date & get(date_field) <= end_date
  ]

  # restrict era5 data to date range of obs - needed again
  start_date <- min(l_lev$dt[, ..date_field][[1]])
  end_date <- max(l_lev$dt[, ..date_field][[1]])
  dt_era5 <- dt_era5[
    get(date_field) >= start_date & get(date_field) <= end_date
  ]
  l_lev$dt <- pad_data(l_lev$dt)
  l_lev$dt_qc <- pad_data(l_lev$dt_qc)

  # check that all are the same no. rows
  if (!identical(dim(l_lev$dt)[1], dim(l_lev$dt_qc)[1], dim(dt_era5)[1])) {
    stop(paste(
      "Obs and ERA5 data tables do not have the same dimensions",
      dim(l_lev$dt),
      dim(l_lev$dt_qc),
      dim(dt_era5)
    ))
  }

  l_lev <- list(dt = l_lev$dt, dt_qc = l_lev$dt_qc, dt_era5 = dt_era5)
  return(l_lev)
}
