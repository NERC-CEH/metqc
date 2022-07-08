# if(!require(devtools)){install.packages("devtools")}
# devtools::install_github("bluegreen-labs/ecmwfr", build_vignettes = TRUE)

setwd("/gws/nopw/j04/ceh_generic/plevy/amo_met/data-raw")
here::i_am("data-raw/era5_update.R")

library(here)
library(keyring)
library(units)
library(ecmwfr)
library(readxl)
library(ncdf4)
library(stringr)
library(stars)
library(data.table)
library(humidity)
library(openair)
source(here("R/imputation.R"))

# constants
dir_out_public = "/gws/nopw/j04/dare_uk/public/plevy/UK-AMo"
#options(keyring_backend="file")
keyring_unlock(keyring = "ecmwfr", password = "test")

datect <- as.POSIXct(Sys.Date())

dt_era5 <- readRDS(file = here("data-raw", "UK-AMo_era5_2022.rds"))
dim(dt_era5)
#dt_era5 <- dt_era5[ time < "2022-04-17"]
datect_last_download <- as.POSIXct(max(dt_era5$time, na.rm = TRUE))

# make a time series between last download and today
v_datelt <- as.POSIXlt(seq.POSIXt(datect_last_download, datect, by = "day"))
v_year <- str_pad(unique(v_datelt$year + 1900) , width = 4, pad = "0")
v_month <- str_pad(unique(v_datelt$mon + 1) , width = 2, pad = "0")
v_day   <- str_pad(unique(v_datelt$mday), width = 2, pad = "0")
# v_month <- "05"
# v_day   <- "24"
#v_day <- v_day[9:31]
#v_day   <- str_pad(21:31, width = 2, pad = "0")
v_hour <- paste0(str_pad(0:23, width = 2, pad = "0"), ":00")

my_request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  #variable = c("2m_dewpoint_temperature", "2m_temperature", "total_precipitation"),
  variable = c(
    "10u",  # "10m_u-component_of_neutral_wind",
    "10v",  # "10m_v-component_of_neutral_wind",
    "2m_dewpoint_temperature",
    "2m_temperature",
    "soil_temperature_level_1",
    "volumetric_soil_water_layer_1",
    "surface_pressure",
    "surface_solar_radiation_downwards",
    "surface_thermal_radiation_downwards",
    "surface_net_solar_radiation",
    "surface_net_thermal_radiation",
    "total_cloud_cover",
    "total_precipitation",
    "total_sky_direct_solar_radiation_at_surface",
    "snow_depth",
    "boundary_layer_height",
    "surface_sensible_heat_flux",
    "surface_latent_heat_flux",
    "runoff",
    "evaporation"),
  # year = "2022",
  # month = "06",
  # day   = c("02", "03"),
  # time  = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  year = v_year,
  month = v_month,
  day   = v_day,
  time  = v_hour, # c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(55.75, -3.25, 55.74, -3.24),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "UK-AMo_era5_2022.nc"
)

wf_check_request(user = "133639", request = my_request)

system.time(
  wf_request(
    user = "133639",
    request = my_request,
    transfer = TRUE,
    verbose = TRUE, # TRUE is fine unless server down
    path = here("data-raw"),
    time_out = 60 #3600 * 12 # wait 12 hours
  )
)

# if timed out, can download with following                    
# wf_transfer(url = '0af12713-7085-44dd-b419-d2fd049be7b6',
# wf_transfer(url = '0af12713-7085-44dd-b419-d2fd049be7b6',
# wf_transfer(url = '7921e1b0-4767-4aed-bd4f-a886e92e0e57', # 24-31 May 2022
# wf_transfer(url = '379f7203-f4f1-4d52-a7ea-2cf5df506974', # 01-30 June 2022
# wf_transfer(url = '832c7023-edcc-4e86-a6a3-ddda4d168c3b', # 01-06 July 2022
wf_transfer(url = 'b3f0a15f-c4bb-4ddf-bb36-dcb7e7038c98', # 21-31 May 2022
 user = "133639",
 path = '/gws/nopw/j04/ceh_generic/plevy/amo_met/data-raw',
 filename = 'UK-AMo_era5_2022.nc',
 service = 'cds')


fname <- here("data-raw", "UK-AMo_era5_2022.nc")
nc <- nc_open(fname)
nc
nc_close(nc)

st <- read_stars(fname)
#st
dt <- as.data.table(st[, 1, 1, ])
dt[, x := NULL]; dt[, y := NULL] # ; dt[, expver := NULL] 
#dt
dt <- dt[complete.cases(dt)]
str(dt)

# calc RH
dt[, rh := RH(drop_units(t2m), drop_units(d2m), isK = TRUE)]
dt[, d2m := NULL]

# convert all temperatures to deg C
T0 <- 273.15
dt[, t2m  := set_units(drop_units(t2m) - T0, C)]
dt[, stl1 := set_units(drop_units(stl1) - T0, C)]

# convert SWC from m3/m3 to %
dt[, swvl1 := drop_units(swvl1) * 100]

# convert pressure from Pa to kPa
dt[, sp := set_units(sp, kPa)]

# convert radiation from hourly total to W m-2
secs_per_hour <- set_units(3600, s)
dt[, ssrd := ssrd / secs_per_hour]
dt[, strd := strd / secs_per_hour]
dt[, ssr  := ssr  / secs_per_hour]
dt[, str  := str  / secs_per_hour]
dt[, fdir := fdir / secs_per_hour]
dt[, sshf := sshf / secs_per_hour]
dt[, slhf := slhf / secs_per_hour]

# calc diffuse radiation
dt[, fdif := ssrd - fdir]

# calc net radiation
dt[, rn  := ssr + str]
dt[, ssru := ssrd - ssr]
dt[, stru := strd - str]
dt[, ssr := NULL]
dt[, str := NULL]

# reverse sign convention for surface heat fluxes
dt[, sshf := sshf * -1]
dt[, slhf := slhf * -1]
dt[, e    := e    * -1]

# units missing for evaporation
dt[, e  := set_units(e, m)]

# convert precip and snow depth to ICOS units
dt[, tp := set_units(tp, mm)]
dt[, sd := set_units(sd,  cm)]

# calc wind speed and direction from u v components
dt[, ws := sqrt(v10^2 + u10^2)]
dt[, wd := (270 - atan2(drop_units(v10), drop_units(u10)) * 180 / pi) %% 360]
dt[, u10 := NULL]
dt[, v10 := NULL]
summary(dt)
dim(dt)


##* WIP - will fail at rbind initially - have to get year so far in this format

# add old and new data, remove duplicates
dt <- rbind(dt_era5, dt)
dt <- dt[!duplicated(dt[, "time"], fromLast = TRUE), ]
dim(dt)

saveRDS(dt, file = here("data-raw", "UK-AMo_era5_2022.rds"))
fwrite( dt, file = here("data-raw", "UK-AMo_era5_2022.csv"))

# plotting
df <- drop_units(dt)
df$date <- df$time
pdf(file = "era5_update.pdf")
  windRose(df)
  timePlot(df, pollutant = c("ssrd", "ssru", "strd", "stru"))
  timePlot(df, pollutant = c("rn", "sshf", "slhf"))
  timePlot(df, pollutant = c("rn", "sshf", "blh"))
  timePlot(df, pollutant = c("ssrd", "fdir", "fdif", "tcc"), scales = "free")
  timePlot(df, pollutant = c("tp", "swvl1", "ro", "e"), scales = "free")
  timePlot(df, pollutant = c("t2m", "stl1"))
  timePlot(df, pollutant = c("swvl1", "sp", "rh"), scales = "free")
dev.off()


#------------------------------------------------------------------------------
# ERA 5 data
#------------------------------------------------------------------------------
# get names
df_names <- read_excel(here("data", "ERA5_to_ICOS.xlsx"), sheet = "mainmet_to_era5")
v_mainmet_name <- c("DATECT", df_names$mainmet_name)  # should be mainmet_name really?
# saveRDS(v_mainmet_name, file = here("data", "v_mainmet_name.rds"))

# # df_era5 <- import("era5_lat=55.75_lng=-3.25_period=19950101-20220403.csv",
  # # header.at = 12, data.at = 13, date = "datetime", date.format = "%d/%m/%Y %H:%M")
df_era5 <- readRDS(here("data-raw", "UK-AMo_era5.rds"))
##* WIP add in updates for current year with rbind
# df_era5_2022 <- readRDS(here("data", "UK-AMo_era5_2022.rds"))
df_era5_2022 <- dt
# add previous and current year data, remove duplicates
df_era5 <- rbind(df_era5, df_era5_2022)
df_era5 <- df_era5[!duplicated(df_era5[, "time"], fromLast = TRUE), ]

df_era5 <- as.data.frame(df_era5)
df_era5 <- drop_units(df_era5)
df_era5$date <- df_era5$time
df_era5$time <- NULL
names(df_era5)
str(df_era5)
first <- min(df_era5$date, na.rm = TRUE)
last  <- max(df_era5$date, na.rm = TRUE)
# make a vector of complete half-hours
DATECT <- seq.POSIXt(first, last, by = "30 min")
dt_date <- data.table(DATECT)
length(DATECT)
head(DATECT); tail(DATECT)

head(df_era5)
tail(df_era5)

# sort by date
df_era5 <- df_era5[order(df_era5$date), ]

names(df_era5)
dim(df_era5) # 365*24 * (2021-1995 +1) = 236520
str(df_era5)
# # df_era5$datetime_lst <- NULL

df_names$era5_name

df_mainmet_era5 <- df_era5[, df_names$era5_name]
str(df_mainmet_era5)
names(df_mainmet_era5) <- df_names$mainmet_name
df_mainmet_era5$date <- df_era5$date
df_era5 <- df_mainmet_era5
str(df_mainmet_era5)
summary(df_era5)
dim(df_era5)

# # # unit conversions
# # df_era5$RH <- df_era5$RH * 100  # frac to percent
# # df_era5$PA <- df_era5$PA / 1000 # Pa to kPa

# make half-hourly from hourly
df_era5 <- timeAverage(df_era5, avg.time = "30 min", fill = TRUE)

df_era5$DATECT <- df_era5$date
df_era5$date <- NULL
names(df_era5)
# ensure df has all variables, and ordered
df_era5 <- df_era5[, v_mainmet_name]
dim(df_era5)
str(df_era5)

# var_name = "TA"
# p <- ggplot(df_era5, aes(DATECT, get(var_name)))
# p <- p + geom_line(data = df_era5, colour = "red")
# p
#p <- p + geom_point(size = 1)

# fill in any missing timestamps
df_era5 <- pad_data(df_era5, v_dates = df_era5$DATECT)
dim(df_era5)
min(df_era5$DATECT); max(df_era5$DATECT)
era5_end_date <- max(df_era5$DATECT)

##* WIP mismatch between these files written here and 
## that read in to check datect_last_download on line 30
# write to file
saveRDS(df_era5, file = here("data", "df_era5.rds"))
# in public folder
fname <- paste0(dir_out_public,  "/df_era5.rds")
saveRDS(df_era5, file = fname)
    