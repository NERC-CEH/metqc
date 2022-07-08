library(rmarkdown)
render("/gws/nopw/j04/ceh_generic/plevy/amo_met/plot_UK-AMo_met.Rmd",
  output_file = "/gws/nopw/j04/dare_uk/public/plevy/UK-AMo/plot_UK-AMo_week.html",
  params = list(
    n_days = 7, 
    dir_out_mainmet = "/gws/nopw/j04/dare_uk/public/plevy/UK-AMo",
    validate_mainmet = TRUE
  )
)
render("/gws/nopw/j04/ceh_generic/plevy/amo_met/plot_UK-AMo_met.Rmd", 
  output_file = "/gws/nopw/j04/dare_uk/public/plevy/UK-AMo/plot_UK-AMo_month.html",
  params = list(
    n_days = 31, 
    dir_out_mainmet = "/gws/nopw/j04/dare_uk/public/plevy/UK-AMo",
    validate_mainmet = FALSE
  )
)
render("/gws/nopw/j04/ceh_generic/plevy/amo_met/plot_UK-AMo_met.Rmd", 
  output_file = "/gws/nopw/j04/dare_uk/public/plevy/UK-AMo/plot_UK-AMo_year.html",
  params = list(
    n_days = 365, 
    dir_out_mainmet = "/gws/nopw/j04/dare_uk/public/plevy/UK-AMo",
    validate_mainmet = FALSE
  )
)