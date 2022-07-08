# R script to:
#   read ICOS logger BM files,
#   subset to day of interest,
#   write daily files, and
#   upload daily files to ICOS server, with MD5 hash 

# Peter Levy
# Centre for Ecology & Hydrology, Edinburgh, U.K.
# plevy@ceh.ac.uk

rm(list = ls())
library(fs)
library(stringr)
library(tools)
library(yaml)
library(data.table)
# Use UTC time throughout
Sys.setenv(TZ = "UTC")

# user & password credentials stored in file readable by user only
cred <- yaml.load_file("/gws/nopw/j04/ceh_generic/plevy/amo_met/credentials.yml")

# define constants
station_code <- "UK-AMo_BM_" # actually includes "BM" for bio-met data
ext <- ".dat"

# usually constant
dir_in  <- "/gws/nopw/j04/ceh_generic/plevy/amo_met/server_mirror"
dir_out <- "/gws/nopw/j04/dare_uk/public/plevy/UK-AMo/daily"

# define function to read TOA5 data
importCSdata <- function(filename,RetOpt="data"){
	if(RetOpt=="info"){
		# bring in entire header of CSI TOA5 data file for metadata
		stn.info <- scan(file=filename,nlines=4,what=character(),sep="\r")
		return(stn.info)
	} else {
		# second line of header contains variable names
		header <- scan(file=filename,skip=1,nlines=1,what=character(),sep=",")
		# bring in data
		stn.data <- read.table(file=filename,skip=4,header=FALSE, na.strings=c("NAN"),sep=",")
		names(stn.data) <- header
		# add column of R-formatted date/timestamps
		stn.data$TIMESTAMP <- as.POSIXlt(strptime(stn.data$TIMESTAMP,"%Y-%m-%d %H:%M:%S"))
		return(stn.data)}
}

# define function to subset and write daily file
write_daily_file <- function(date_to_process, dir_in, fname_in, dir_out, 
  station_code = "UK-AMo_BM_", logger_id, file_id, ext = ".dat"){

  fname <- path(dir_in, fname_in)
  df <- importCSdata(fname)

  # remove duplicate rows - sometimes occur in the Campbell files
  df <- df[!duplicated(df$TIMESTAMP), ]

  # ICOS require the midnight value to be included as the final value of the day
  # i.e. not as the first value of the next day, as subsetting on yday would do.
  # So we need to lag the timestamps by -1 row and subset on this
  datect <- as.POSIXct(df$TIMESTAMP)
  # lag with n=1 and pad with NA (returns vector)
  datect <- shift(datect, n=1, fill=NA, type="lag")
  df$TIMESTAMP_lagged <- as.POSIXlt(datect)
  # subset using the lagged values
  df <- subset(df, TIMESTAMP_lagged$year == date_to_process$year &
                    TIMESTAMP_lagged$yday == date_to_process$yday)
  
  df$TIMESTAMP_lagged <- NULL # and remove it
  # change timestamp to a character variable with no punctuation
  df$TIMESTAMP <- str_remove_all(as.character(df$TIMESTAMP), "[-: ]")

  # get the date parts of the output file name
  year <- date_to_process$year + 1900
  mon <- str_pad(date_to_process$mon + 1, 2, pad = "0")
  mday <- str_pad(date_to_process$mday, 2, pad = "0")

  fname_out <- paste0(station_code, year, mon, mday, logger_id, file_id, ext)
  # add the full path for writing output
  fname <- path(dir_out, paste0(station_code, year, mon, mday, logger_id, file_id, ext))
  write.csv(df, file = fname, eol = "\r\n", na = "NAN", row.names = FALSE)
  
  return(fname_out)
}

# define function to get the MD5 hash and upload daily file to ICOS server
upload_daily_file <- function(dir, fname,
  user = cred$user, password = cred$password){

  fpath <- path(dir, fname)
  MD5 <- md5sum(fpath)

  # create the command from the parts
  cmd <- paste0("curl --upload-file ", fpath, " https://", user, 
    password, "@data.icos-cp.eu/upload/etc/", MD5, "/", fname)

  # submit the command to the OS
  err <- system(cmd)
    
  return(err)
}

# run the functions 
# get today's date
n <- 1 # or n days ago - usually 1 i.e. uploading yesterday's data
date_to_process <- as.POSIXlt(Sys.Date() - n)
date_to_process


#### 1. Logger 02 File 01 - 30-sec T and RH data
logger_id <- "_L02"
file_id   <- "_F01"
fname_in  <- paste0("UK-AMo_BM", logger_id, file_id, ".dat")
# write the daily file
fname_out <- write_daily_file(date_to_process, dir_in, fname_in, dir_out, 
  station_code, logger_id, file_id, ext)
# upload to the ICOS server
err <- upload_daily_file(dir_out, fname_out)


#### 2. Logger 03 File 01 - 20-sec radiation data
logger_id <- "_L03"
file_id   <- "_F01"
fname_in  <- paste0("UK-AMo_BM", logger_id, file_id, ".dat")
# write the daily file
fname_out <- write_daily_file(date_to_process, dir_in, fname_in, dir_out, 
  station_code, logger_id, file_id, ext)
# upload to the ICOS server
err <- upload_daily_file(dir_out, fname_out)


#### 3. Logger 03 File 02 - 1-min rainfall and snow data
logger_id <- "_L03"
file_id   <- "_F02"
fname_in  <- paste0("UK-AMo_BM", logger_id, file_id, ".dat")
# write the daily file
fname_out <- write_daily_file(date_to_process, dir_in, fname_in, dir_out, 
  station_code, logger_id, file_id, ext)
# upload to the ICOS server
err <- upload_daily_file(dir_out, fname_out)


#### 4. Logger 04 File 01 - 1-min soil-met data
logger_id <- "_L04"
file_id   <- "_F01"
fname_in  <- paste0("UK-AMo_BM", logger_id, file_id, ".dat")
# write the daily file
fname_out <- write_daily_file(date_to_process, dir_in, fname_in, dir_out, 
  station_code, logger_id, file_id, ext)
# upload to the ICOS server
err <- upload_daily_file(dir_out, fname_out)


#### 5. Logger 04 File 02 - 20-sec radiation data
logger_id <- "_L04"
file_id   <- "_F02"
fname_in  <- paste0("UK-AMo_BM", logger_id, file_id, ".dat")
# write the daily file
fname_out <- write_daily_file(date_to_process, dir_in, fname_in, dir_out, 
  station_code, logger_id, file_id, ext)
# upload to the ICOS server
err <- upload_daily_file(dir_out, fname_out)


#### 6. Logger 04 File 03 - 30-sec TA/RH data
logger_id <- "_L04"
file_id   <- "_F03"
fname_in  <- paste0("UK-AMo_BM", logger_id, file_id, ".dat")
# write the daily file
fname_out <- write_daily_file(date_to_process, dir_in, fname_in, dir_out, 
  station_code, logger_id, file_id, ext)
# upload to the ICOS server
err <- upload_daily_file(dir_out, fname_out)


#### 7. Logger 04 File 04 - 1-min precip data
logger_id <- "_L04"
file_id   <- "_F04"
fname_in  <- paste0("UK-AMo_BM", logger_id, file_id, ".dat")
# write the daily file
fname_out <- write_daily_file(date_to_process, dir_in, fname_in, dir_out, 
  station_code, logger_id, file_id, ext)
# upload to the ICOS server
err <- upload_daily_file(dir_out, fname_out)


#### 8. Logger 05 File 01 - 1-min soil-met data
logger_id <- "_L05"
file_id   <- "_F01"
fname_in  <- paste0("UK-AMo_BM", logger_id, file_id, ".dat")
# write the daily file
fname_out <- write_daily_file(date_to_process, dir_in, fname_in, dir_out, 
  station_code, logger_id, file_id, ext)
# upload to the ICOS server
err <- upload_daily_file(dir_out, fname_out)

# exit gracefully
quit("no")
