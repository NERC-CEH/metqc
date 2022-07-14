#' Custom plotting function for each variable
#'

#' @title plotting_function
#' @description Creates an interactive girafe plot, whereby the user can select 
#'   points for flagging as poor quality and imputing new values.
#' @param input_variable The name of the variable within the query data frame to plot.
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @export
#'
#' @examples
plotting_function <- function(input_variable) {
  df <- data.frame(DATECT = l_qry$df$DATECT, y = l_qry$df[, input_variable], 
    qc = l_qry$df_qc[, input_variable], checked = l_qry$df$checked)
  
  df <- left_join(df, df_method, by = "qc")

  p1_ggplot <- ggplot(df, 
                      aes(DATECT, y)) +
    geom_point_interactive(aes(data_id = checked, tooltip = method,
                               colour = factor(method)), size = 3) +
    xlab("Date") +
    ylab(paste("Your variable:", input_variable)) +
    ggtitle(paste(input_variable, "time series")) +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title = element_blank())
  p1_girafe <- girafe(code = print(p1_ggplot),
                      width_svg = 6, height_svg = 5)
  p1_girafe <- girafe_options(p1_girafe, opts_selection(
    type = "multiple",
    css = "fill:#FF3333;stroke:black;"),
    opts_hover(css = "fill:#FF3333;stroke:black;cursor:pointer;"),
    opts_zoom(max = 5))
}

#' Custom plotting function to construct a heatmap calendar
#'

#' @title plot_heatmap_calendar
#' @description Plots a calendar showing who has checked the data by date
#' @param df A data frame of met data
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @export
#'
#' @examples
plot_heatmap_calendar <- function(df) {
  # Transforming query dataframe with lubridate to fit the format needed for a heatmap calendar
  df <- df %>%
	  mutate(year = year(DATECT),
           day_of_the_week = lubridate::wday(DATECT, label = TRUE, week_start = 1),
           month = lubridate::month(DATECT, label = TRUE, abbr = FALSE),
           week = isoweek(DATECT),
           day = day(DATECT)) %>%
    select(year, month, day, week,
           day_of_the_week, validator)
  df$week <- as.double(df$week)
  df <- dplyr::mutate(df,
                                    week = case_when(month == "December" & week == 1 ~ 53,
                                                     month == "January" & week %in% 52:53 ~ 0,
                                                     TRUE ~ week))

  heatmap_plot <- ggplot(df,
                         aes(day_of_the_week, -week,
                             fill = validator)) +
    geom_tile(color = "white", size = 0.1) +
    facet_wrap(year~month, nrow = 4, ncol = 3, scales = "free") +
    theme_minimal(base_size = 8) +
    theme(legend.position = "bottom") +
    theme(plot.title = element_text(size = 14)) +
    theme(axis.text.y = element_text(size = 6)) +
    theme(strip.background = element_rect(colour = "white")) +
    theme(plot.title = element_text(hjust = 0)) +
    theme(axis.ticks = element_blank()) +
    theme(axis.text = element_text(size = 7)) +
    theme(legend.title = element_text(size = 8)) +
    theme(legend.text = element_text(size = 6)) +
    removeGrid()
  heatmap_plot
}
