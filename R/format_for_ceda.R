#' Format the data for submission to CEDA
#'
#' @param l_lev A named list with three components \code{dt} and \code{dt_qc},
#'   which are the data tables of the observations, the quality
#'   control codes, and associated ERA5 data.
#' @param v_names A character vector of column names
#'   that should be included in the new data frame.
#'
#' @return A new data frame with the selected columns from \code{dt} and
#'   \code{dt_qc}.
#'
#' @export
format_for_ceda <- function(
  l_lev,
  v_names = c(
    "TA_4_1_1",
    "RH_4_1_1",
    "PA_4_1_1",
    "RG_4_1_0",
    "PPFD_IN_4_1_1",
    "RN_5_1_1",
    "P_12_1_1",
    "TS",
    "G",
    "SWC",
    "WS_6_1_1",
    "WD_6_1_1",
    "D_SNOW",
    "WTD"
  )
) {
  # Get the qc names by adding "_qc" to the v_names
  v_names_qc <- paste0(v_names, "_qc")

  # Select the columns from dt_qc with the names in v_names_qc
  l_lev$dt_qc <- setNames(l_lev$dt_qc[, ..v_names], v_names_qc)

  # Create a new data frame by binding the selected columns from dt and dt_qc
  dt <- cbind(l_lev$dt[, .(DATECT)], l_lev$dt[, ..v_names], l_lev$dt_qc)

  # Return the new data frame
  return(dt)
}
