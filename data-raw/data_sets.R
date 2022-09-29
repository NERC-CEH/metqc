
# This script is run to process raw data to create data objects 
# stored in \data, which are built into the package.
# The contents of \data-raw are ignored when building.

# Define lookup table for methods, qc (codes) and method labels (method_lab).

df_method <- data.frame(
  method = c('raw', "missing", "time", "regn", "nightzero", "noneg", "zero", "era5", "flagged"),
  method_longname = c('Original observation (raw data)', "Missing", "Time", "Regression with covariate", "Night-time zero", "No negative values", "All zeros", "Regression with ERA5 covariate", "Flagged data"),
  qc = c(0, 1, 2, 3, 4, 5, 6, 7, 8) # 0 = raw, 1 = missing
)

df_method$method_longname <- factor(df_method$method_longname, levels = c('Original observation (raw data)', "Missing", "Time", "Regression with covariate", "Night-time zero", "No negative values", "All zeros", "Regression with ERA5 covariate", "Flagged data"))

usethis::use_data(df_method, overwrite = T)

# End