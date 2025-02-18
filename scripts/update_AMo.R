## ----startup, eval=TRUE, echo=FALSE-------------------------------------------
library(pins)
library(dplyr)
library(ggplot2)
library(data.table)
board <- pins::board_connect()


## ----update, eval=TRUE, echo=FALSE--------------------------------------------
# Read in ERA5 data----
#url_era5 <- paste0("https://gws-access.jasmin.ac.uk/public/dare_uk/plevy/UK-AMo/df_era5.rds")
url_era5 <- paste0("https://gws-access.jasmin.ac.uk/public/ukem/UK-AMo/df_era5.rds")

df_era5 <- readRDS(url(url_era5, "rb"))

paste("ERA5 data from", min(df_era5$DATECT), "to",  max(df_era5$DATECT))

# Reading in the logger data----
this_year <- as.POSIXlt(Sys.Date())$year + 1900
# url_mainmet <- paste0("https://gws-access.jasmin.ac.uk/public/dare_uk/plevy/UK-AMo/UK-AMo_mainmet_", this_year, "_agf.rds")
url_mainmet <- paste0("https://gws-access.jasmin.ac.uk/public/ukem/UK-AMo/UK-AMo_mainmet_", this_year, "_agf.rds")

l_lev1 <- readRDS(url(url_mainmet, "rb"))

paste("Level 1 data from", min(l_lev1$df$DATECT), "to",  max(l_lev1$df$DATECT))

pins::pin_write(board,
  df_era5,
  name = "plevy/era5_data", type = "rds") # use qs instead? qs is actually slower somehow
  
pins::pin_write(board,
  l_lev1,
  name = "plevy/level1_data", type = "rds")


## ----readLevel2, eval=TRUE, echo=FALSE----------------------------------------
# and to read the data back
system.time(l_lev2 <- pin_read(
  board, 
  "plevy/level2_data"))
paste("Level 2 (checked by user) data exists from", min(l_lev2$df$DATECT), "to",  max(l_lev2$df$DATECT))
df    <- subset(l_lev2$df,    DATECT >= "2022-01-01")
df_qc <- subset(l_lev2$df_qc, DATECT >= "2022-01-01")

# write to pins
pins::pin_write(board, df,    name = "plevy/UK-AMo_met_level2_data",    type = "csv")
pins::pin_write(board, df_qc, name = "plevy/UK-AMo_met_level2_data_qc", type = "csv")
# write csv version for current year
# https://connect-apps.ceh.ac.uk/AMo_met_level2_data/
# https://connect-apps.ceh.ac.uk/UK-AMo_met_level2_data_qc/
# write to file on P: drive - doesnt work on Connect server - no write access
# dir_output <- "//nercbuctdb.adceh.ceh.ac.uk/projects1/NECXXXX_Auchencorth/metqc/output/"
# fwrite(df,    file = paste0(dir_output, "UK-AMo_met_level2.csv"))
# fwrite(df_qc, file = paste0(dir_output, "UK-AMo_met_level2_qc.csv"))

# plot calendar heatmap
df <- df_qc %>%
  mutate(year = lubridate::year(DATECT),
         day_of_the_week = lubridate::wday(DATECT, label = TRUE, week_start = 1),
         month = lubridate::month(DATECT, label = TRUE, abbr = FALSE),
         week = lubridate::isoweek(DATECT),
         day = lubridate::day(DATECT)) %>%
  dplyr::select(year, month, day, week,
         day_of_the_week, validator)
df$week <- as.double(df$week)
df <- dplyr::mutate(df,
  week = case_when(month == "December" & week == 1 ~ 53,
  month == "January" & week %in% 52:53 ~ 0,
  TRUE ~ week)
  )

heatmap_plot <- ggplot(df,
                       aes(day_of_the_week, -week,
                           fill = validator)) +
  geom_tile(color = "white", size = 0.1) +
  facet_wrap(year~month, scales = "free") +
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
  ggExtra::removeGrid()
heatmap_plot

# no need to write it out
# pins::pin_write(board,
  # l_lev2,
  # name = "level2_data", type = "rds")


## ----dataSummary, eval=TRUE, echo=TRUE----------------------------------------
dfs <- subset(df_era5, DATECT >= "2022-01-01")
paste("ERA5 data from", min(dfs$DATECT), "to",  max(dfs$DATECT))
with(dfs, plot(DATECT, TS))
summary(dfs)

