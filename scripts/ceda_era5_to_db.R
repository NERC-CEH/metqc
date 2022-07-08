# read CEDA and ERA5 data
# check CEDA with rules and replace violations with NAs
# fill in these gaps via the impute function
# usually using the ERA5 data in a regression

here::i_am("scripts/ceda_era5_to_db.R")
library(here)
library(qs)
library(ggplot2)
library(dplyr)
library(readxl)
library(stringr)
library(ROracle)
library(openair)
library(mgcv)
library(data.table)
library(errorlocate)
library(units)
source(here("R/imputation.R"))

# constants
v_names_for_db <- read.table(here("data", "v_names_for_db.txt"), stringsAsFactors = FALSE)$V1
v_names_for_db_nodate <- v_names_for_db
v_names_for_db <- c("DATECT", v_names_for_db)
# Use UTC not DST----
Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")

#------------------------------------------------------------------------------
# ERA 5 data
#------------------------------------------------------------------------------
# get names
df_names <- read_excel(here("data", "ERA5_to_ICOS.xlsx"), sheet = "mainmet_to_era5")
v_mainmet_name <- c("DATECT", df_names$mainmet_name)  # should be mainmet_name really?
# saveRDS(v_mainmet_name, file = here("data", "v_mainmet_name.rds"))

# # df_era5 <- import("era5_lat=55.75_lng=-3.25_period=19950101-20220403.csv",
  # # header.at = 12, data.at = 13, date = "datetime", date.format = "%d/%m/%Y %H:%M")
df_era5 <- readRDS(here("data", "UK-AMo_era5.rds"))
##* WIP add in updates for current year with rbind
df_era5_2022 <- readRDS(here("data", "UK-AMo_era5_2022.rds"))
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
# write to file
saveRDS(df_era5, file = here("data", "df_era5.rds"))
    
#------------------------------------------------------------------------------
# CEDA data
#------------------------------------------------------------------------------
# Read in the CEDA data----
#df_names <- read_excel(here("data-raw", "CEDA_ICOS_names.xlsx"), sheet = "Sheet1")
df_names <- read_excel(here("data", "ERA5_to_ICOS.xlsx"), sheet = "ceda_to_mainmet")
df_names$ceda_name
df_names$icos_name
df_names$mainmet_name

# these files need some editing for errors in first line and extra columns/header lines
v_fname <- list.files(here("data-raw/amo_ceda"), full.names = TRUE)
l_dt <- lapply(v_fname, fread, skip = 202)

lapply(l_dt, names)
# remove extra cols from 2018 on
l_dt[[23]] <- l_dt[[23]][, 1:55]
l_dt[[24]] <- l_dt[[24]][, 1:55]
l_dt[[25]] <- l_dt[[25]][, 1:55]
l_dt[[26]] <- l_dt[[26]][, 1:55]

dt <- rbindlist(l_dt)
# map the ceda names to those we want in mainmet
# all the unwanted flags and gf data remain as "Vnn"
names(dt) <- df_names$mainmet_name
v_mainmet_names_in_ceda <- names(dt)[names(dt) %in% df_names$mainmet_name]

str(dt)
# keep only those we want in mainmet - discard all the flags and gf data
dt <- dt[, ..v_mainmet_names_in_ceda]
# convert data type of some columns needs correcting
dt$DATECT <- as.POSIXct(dt$DATECT, format="%d/%m/%Y %H:%M")

# add the variables missing from CEDA but wanted in MAINMET
names(dt) %in% v_names_for_db
# list MAINMET variables present
v_names_for_db[v_names_for_db %in% names(dt)]
# list MAINMET variables that are missing
v_missing <- v_names_for_db[v_names_for_db %!in% names(dt)]
# add these to the data table
dt[, c(v_missing) := NA]
# ensure df has all variables, and ordered
dt <- dt[, ..v_names_for_db]
names(dt)
str(dt)

# if all missing, these appear as logical; convert to numeric
dt <- dt[, lapply(.SD, as.numeric)]
# convert data type of some columns needs correcting
dt[, DATECT := as.POSIXct(dt$DATECT, origin = "1970-01-01")]

# some ad hoc unit conversions
dt$PA_4_1_1  <- dt$PA_4_1_1 / 10 # hPa to kPa
dt$D_SNOW  <- dt$D_SNOW / 10 # mm to cm
summary(dt)

# fill in missing timestamps
dim(dt)
dim(df_era5)
dt <- as.data.table(pad_data(dt, v_dates = df_era5$DATECT))
dt[is.na(dt)] <- NA  # seems to be needed in case of NaN
dim(dt)
names(dt)

# write out the CEDA data in mainmet format before any QC
saveRDS(dt, file = here("data", "UK-Amo_mainmet_lev0.rds"))
fwrite( dt, file = here("data", "UK-Amo_mainmet_lev0.csv"))

# validate data with simple range check
# this is much quicker than locate_errors from validate package which takes ~8 mins
dt[TS            <    -10| TS            >   30, TS            := NA]
dt[TS_4_1_1      <    -10| TS_4_1_1      >   30, TS_4_1_1      := NA]
dt[SWC           <    30 | SWC           >  100, SWC           := NA]
dt[SWC_4_1_1     <    30 | SWC_4_1_1     >  100, SWC_4_1_1     := NA]
dt[G             <   -20 | G             >   20, G             := NA]
dt[G_4_1_1       <   -20 | G_4_1_1       >   20, G_4_1_1       := NA]
dt[G_4_1_2       <   -20 | G_4_1_2       >   20, G_4_1_2       := NA]
dt[TA_4_1_1      <   -20 | TA_4_1_1      >   35, TA_4_1_1      := NA]
dt[PA_4_1_1      <    92 | PA_4_1_1      >  120, PA_4_1_1      := NA]
dt[RH_4_1_1      <    30                       , RH_4_1_1      := NA]
dt[RH_4_1_1      >   100 & RH_4_1_1      <  120, RH_4_1_1      := 100]
dt[RH_4_1_1      >=  120                       , RH_4_1_1      := NA]
dt[SW_IN         <     0 | SW_IN         > 1200, SW_IN         := NA]
dt[SW_OUT        <     0 | SW_OUT        >  200, SW_OUT        := NA]
dt[LW_IN         <     0 | LW_IN         > 1200, LW_IN         := NA]
dt[LW_OUT        <     0 | LW_OUT        > 1000, LW_OUT        := NA]
dt[PPFD_IN_4_1_1 <     0 | PPFD_IN_4_1_1 > 2200, PPFD_IN_4_1_1 := NA]
dt[P_12_1_1      <     0 | P_12_1_1      >   30, P_12_1_1      := NA]
dt[WS_6_1_1      <     0 | WS_6_1_1      >   30, WS_6_1_1      := NA]
dt[WD_6_1_1      <     0 | WD_6_1_1      >  360, WD_6_1_1      := NA]
dt[D_SNOW        <     0 | D_SNOW        >  500, D_SNOW        := NA]

var_name = "RH_4_1_1"
p <- ggplot(dt, aes(DATECT, get(var_name)))
p <- p + geom_line(data = dt, colour = "red")
p

##* WIP ultimately we want to keep this as a data table
df <- as.data.frame(dt)
# initialise QC code data frame
df_qc <- df
# convert logical to 0 = raw & 1 = missing
df_qc[, -1] <- as.numeric(is.na(df[, -1]))
str(df)
str(df_qc)

l_lev1_ceda <- list(df = df, df_qc = df_qc)
str(l_lev1_ceda)
dim(df_era5)
dim(l_lev1_ceda$df)

# add name of validator as a new variable - the above range checks correspond to
# validation that happens automatically, hence = "auto"
# later replaced by username in metqc app after manual check
l_lev1_ceda$df_qc$validator <- "auto"

# do we need to save to files at this point?
# just for backup
saveRDS(l_lev1_ceda,      file = here("data", "UK-Amo_mainmet_val.rds"))
fwrite(l_lev1_ceda$df,    file = here("data", "UK-Amo_mainmet_val.csv"))
fwrite(l_lev1_ceda$df_qc, file = here("data", "UK-Amo_mainmet_val_qc.csv"))
# just for backup
#l_lev1_ceda <- readRDS(file = here("data", "UK-Amo_mainmet_val.rds"))

# read new data for current year from data logger file - will need updating to "this_year"
l_lev1_thisyear <- readRDS(file = here("data", "UK-AMo_mainmet_2022_agf.rds"))

names(df_era5)
names(l_lev1_ceda$df_qc)
names(l_lev1_thisyear$df_qc)
min(df_era5$DATECT); max(df_era5$DATECT)
min(l_lev1_ceda$df_qc$DATECT); max(l_lev1_ceda$df_qc$DATECT)
min(l_lev1_thisyear$df_qc$DATECT); max(l_lev1_thisyear$df_qc$DATECT)

# subset to start of ICOS logger data, 1/12/2021
dim(l_lev1_ceda$df)
tail(l_lev1_ceda$df)

icos_start_data <- min(l_lev1_thisyear$df_qc$DATECT)
l_lev1_ceda$df    <- subset(l_lev1_ceda$df,    DATECT < icos_start_data)
l_lev1_ceda$df_qc <- subset(l_lev1_ceda$df_qc, DATECT < icos_start_data)

# match the variable names
l_lev1_ceda$df    <- l_lev1_ceda$df[,    names(l_lev1_thisyear$df)]
l_lev1_ceda$df_qc <- l_lev1_ceda$df_qc[, names(l_lev1_thisyear$df_qc)]

# combine existing ceda data with this year's data with rbind
l_lev1 <- list()
str(l_lev1)
l_lev1$df    <- rbind(l_lev1_ceda$df,    l_lev1_thisyear$df)
l_lev1$df_qc <- rbind(l_lev1_ceda$df_qc, l_lev1_thisyear$df_qc)
min(l_lev1$df_qc$DATECT); max(l_lev1$df_qc$DATECT)

l_lev1$df    <- subset(l_lev1$df,    DATECT <= era5_end_date)
l_lev1$df_qc <- subset(l_lev1$df_qc, DATECT <= era5_end_date) 

dim(l_lev1$df)
dim(df_era5)
names(l_lev1$df)
names(df_era5)
summary(l_lev1$df)

var_name = "WTD_4_1_1"
p <- ggplot(l_lev1$df, aes(DATECT, get(var_name)))
p <- p + geom_line()
p
plot_with_qc(y = var_name, l_lev1)
summary(l_lev1$df[, var_name])
sum(is.na(l_lev1$df[, var_name]))
#l_lev1$df[which(is.na(l_lev1$df[, var_name])), ]

# list of data frames containing gap-filled data and qc codes
# ERA5 works well
plot_graph = FALSE # FALSE # TRUE
# get the point measurements complete first
# ERA5 works ok, but straight replacement better
l_lev1 <- impute(y = "P_12_1_1", l_met = l_lev1, method = "era5", fit = FALSE, df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "WD_6_1_1", l_met = l_lev1, method = "era5", fit = FALSE, df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "SW_IN",    l_met = l_lev1, method = "era5", fit = FALSE, df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "SW_IN",    l_met = l_lev1, method = "regn", qc_tokeep = c(0, 7), x = "TS_4_1_1", plot_graph = plot_graph)
l_lev1 <- impute(y = "SW_OUT",   l_met = l_lev1, method = "era5", fit = TRUE, df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "LW_IN",    l_met = l_lev1, method = "era5", fit = FALSE, df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "LW_OUT",   l_met = l_lev1, method = "era5", fit = FALSE, df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "RG_4_1_0", l_met = l_lev1, method = "era5", fit = FALSE, df_era5 = df_era5, plot_graph = plot_graph)

# use ERA5 regression
l_lev1 <- impute(y = "TA_4_1_1", l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "RH_4_1_1", l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "TS_4_1_1", l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "PA_4_1_1", l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "PPFD_DIF", l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "WS_6_1_1", l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "NDVI_649IN_5_1_1", l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "NDVI_649OUT_5_1_1", l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "NDVI_797IN_5_1_1", l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "NDVI_797OUT_5_1_1", l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)
# For RN, CEDA data dodgy (just wrong col? never <0) - check; 
# better to replace pre-icos data with era5; for now, replace all with qc_tokeep = "" (so even replace raw data)
# or recalc as balance of in-out
l_lev1 <- impute(y = "RN_5_1_1", l_met = l_lev1, qc_tokeep = "", method = "era5", df_era5 = df_era5, fit = FALSE, plot_graph = plot_graph)

# use other covariate regression
l_lev1 <- impute(y = "G_4_1_1", l_met = l_lev1, method = "time", plot_graph = plot_graph)
l_lev1 <- impute(y = "G_4_1_2", l_met = l_lev1, method = "regn", x = "G_4_1_1", plot_graph = plot_graph)
l_lev1 <- impute(y = "PPFD_IN_4_1_1", l_met = l_lev1, method = "regn", x = "SW_IN", plot_graph = plot_graph)
l_lev1 <- impute(y = "PPFD_OUT",      l_met = l_lev1, method = "regn", x = "PPFD_IN_4_1_1", plot_graph = plot_graph)
l_lev1 <- impute(y = "PPFD_DIF",      l_met = l_lev1, method = "regn", x = "PPFD_IN_4_1_1", plot_graph = plot_graph)
# fill in remaining gaps
l_lev1 <- impute(y = "PPFD_IN_4_1_1", l_met = l_lev1, method = "regn", qc_tokeep = c(0, 3), x = "TS_4_1_1", plot_graph = plot_graph)
l_lev1 <- impute(y = "PPFD_IN_4_1_1", l_met = l_lev1, method = "time", qc_tokeep = c(0, 3), plot_graph = plot_graph)

# soil temperature profile: use top TS in covariate regression
l_lev1 <- impute(y = "TS_4_2_1", l_met = l_lev1, method = "regn", x = "TS_4_1_1", plot_graph = plot_graph)
l_lev1 <- impute(y = "TS_4_3_1", l_met = l_lev1, method = "regn", x = "TS_4_1_1", plot_graph = plot_graph)
l_lev1 <- impute(y = "TS_4_4_1", l_met = l_lev1, method = "regn", x = "TS_4_1_1", plot_graph = plot_graph)

# soil water profile: use top SWC in covariate regression
l_lev1 <- impute(y = "SWC_4_1_1", l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)
l_lev1 <- impute(y = "SWC_4_2_1", l_met = l_lev1, method = "regn", x = "SWC_4_1_1", plot_graph = plot_graph)
l_lev1 <- impute(y = "SWC_4_3_1", l_met = l_lev1, method = "regn", x = "SWC_4_1_1", plot_graph = plot_graph)
l_lev1 <- impute(y = "SWC_4_4_1", l_met = l_lev1, method = "regn", x = "SWC_4_1_1", plot_graph = plot_graph)

l_lev1 <- impute(y = "SW_IN", l_met = l_lev1, method = "regn", x = "TS_4_1_1", plot_graph = plot_graph)

# doesnt really work with ERA5
l_lev1 <- impute(y = "WTD_4_1_1",  l_met = l_lev1, method = "era5", df_era5 = df_era5, plot_graph = plot_graph)


# for spatial means:
# do regression with point variables present in CEDA
# then do regression with ERA5 data
l_lev1 <- impute(y = "TS",  l_met = l_lev1, method = "regn", x = "TS_4_1_1", plot_graph = plot_graph)
l_lev1 <- impute(y = "G",   l_met = l_lev1, method = "regn", fit = FALSE, x = "G_4_1_1", plot_graph = plot_graph)
l_lev1 <- impute(y = "SWC", l_met = l_lev1, method = "regn", fit = TRUE, x = "SWC_4_1_1", plot_graph = plot_graph)
l_lev1 <- impute(y = "WTD", l_met = l_lev1, method = "regn", fit = FALSE, x = "WTD_4_1_1", plot_graph = plot_graph)

# snow is difficult: no CEDA data, recent data dodgy, ERA5 looks too high
# best available is replace with ERA5
l_lev1 <- impute(y = "D_SNOW", l_met = l_lev1, qc_tokeep = "", method = "era5", df_era5 = df_era5, fit = FALSE, plot_graph = plot_graph)

saveRDS(l_lev1,      file = here("data", "UK-Amo_mainmet_lev1.rds"))
# fwrite(l_lev1$df,    file = here("data", "UK-Amo_mainmet_val.csv"))
# fwrite(l_lev1$df_qc, file = here("data", "UK-Amo_mainmet_val_qc.csv"))
dim(l_lev1$df_qc)

# p <- p + geom_point(size = 1)

# # make a daily data frame for quicker plotting
# dt$date <- dt$DATECT
# dt_day      <- timeAverage(dt, avg.time = "1 day")
# summary(dt_day)
# df_mainmet_era5$date <- df_mainmet_era5$DATECT
# dt_day_era5 <- timeAverage(df_mainmet_era5, avg.time = "1 day")

# var_name = "RH"
# p <- ggplot(dt_day, aes(DATECT, get(var_name)))
# p <- p + geom_point(size = 1)
# p
# p <- p + geom_point(data = dt_day_era5, colour = "red")
# p <- p + geom_line(data = dt_day_era5, colour = "red")

##* WIP - 2/5/2022 - got to here - written files, not db

# Setting up the db server----
Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")
Sys.setenv(DBNAME = "budbase.nerc-bush.ac.uk/BUA")
Sys.setenv(DBUID  = "BU_FIELD_SITES")
Sys.setenv(DBPWD  = "0ig2mtYUL9")

# Making database connection----
drv <- dbDriver("Oracle")
con <- dbConnect(drv,
                 dbname = Sys.getenv("DBNAME"),
                 username = Sys.getenv("DBUID"),
                 password = Sys.getenv("DBPWD")
)
dbListTables(con)
dbListFields(con, "MAINMET_RAW")

dbRemoveTable(con, "MAINMET_RAW")


l_lev1$df[is.na(l_lev1$df)] <- NA  # seems to be needed in case of NaN
str(l_lev1$df)
str(l_lev1$df)

# write CEDA data to db tables
dbWriteTable(con, "MAINMET_LEVEL1",    l_lev1$df,    overwrite = TRUE)
dbWriteTable(con, "MAINMET_LEVEL1_QC", l_lev1$df_qc, overwrite = TRUE)

dt <- readRDS(file = here("data", "UK-Amo_mainmet_lev0.rds"))
str(dt)
dbWriteTable(con, "MAINMET_LEVEL0",    dt, overwrite = TRUE)

dbCommit(con)
dbDisconnect(con)
