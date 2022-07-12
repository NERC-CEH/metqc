
# This script is run to process raw data to create data objects 
# stored in \data, which are built into the package.
# The contents of \data-raw are ignored when building.

# Define lookup table for methods, qc (codes) and method labels (method_lab).

df_method <- data.frame(
  method = c('raw', "missing", "time", "regn", "nightzero", "noneg", "zero", "era5"),
  method_lab = c('Original observation (raw data)', "Missing", "Time", "REGN", "Night-zero", "NoneG", "Zero", "ERA5"),
  qc = c(0, 1, 2, 3, 4, 5, 6, 7) # 0 = raw, 1 = missing
)

df_method$method_lab <- factor(df_method$method_lab, levels = c('Original observation (raw data)', "Missing", "Time", "REGN", "Night-zero", "NoneG", "Zero", "ERA5"))

usethis::use_data(df_method, overwrite = T)

# End