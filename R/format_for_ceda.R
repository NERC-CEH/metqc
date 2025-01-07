# # for testing
library(pins)
board <- board_connect()
l_lev <- pin_read(board, "plevy/level2_data")
summary(l_lev)
names(l_lev$df)
names(l_lev$df_qc)


#' Format the data for submission to CEDA
#'
#' @param l_lev A named list with two components \code{df} and \code{df_qc},
#'   which are the data frames of the observations and the associated quality
#'   control codes.
#' @param v_names A character vector of column names in the data frame \code{df}
#'   that should be included in the new data frame.
#'
#' @return A new data frame with the selected columns from \code{df} and
#'   \code{df_qc}.
#'
#' @export
format_for_ceda <- function(l_lev,
  v_names = c( "TA_4_1_1", "RH_4_1_1", "PA_4_1_1", "RG_4_1_0", "PPFD_IN_4_1_1",
    "RN_5_1_1", "P_12_1_1", "TS", "G", "SWC", "WS_6_1_1", "WD_6_1_1", "D_SNOW",
    "WTD")
) {

  # Get the qc names by adding "_qc" to the v_names
  v_names_qc <- paste0(v_names, "_qc")

  # Select the columns from df_qc with the names in v_names_qc
  l_lev$df_qc <- setNames(l_lev$df_qc[, v_names], v_names_qc)

  # Create a new data frame by binding the selected columns from df and df_qc
  df <- cbind.data.frame(l_lev$df[, c("DATECT", v_names)], l_lev$df_qc)

  # Return the new data frame
  return(df)
}
