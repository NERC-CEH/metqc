# Set timezone to GMT to stop R/Oracle changing dates based on daylight saving time
Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")

# Set database connection
dbuid <- "BU_FIELD_SITES" 
dbpwd <- "0ig2mtYUL9" 
drv <- dbDriver("Oracle")
con <- dbConnect(drv, dbname = "budbase.nerc-bush.ac.uk/BUA",
                 username = dbuid,
                 password = dbpwd)
table_name <- "MET_30MIN"                      
dbNames <- dbListFields(con, table_name)

df_proc <- data.frame(
  startDate = "1995/01/01 00:00",
  endDate   = "2019/12/31 00:00",
  ghgName = "co2"
)


#Format the dates for R
df_proc$startDate <- as.POSIXct(df_proc$startDate, format = "%Y/%m/%d %H:%M", tz = "UTC")
df_proc$endDate   <- as.POSIXct(df_proc$endDate, format = "%Y/%m/%d %H:%M", tz = "UTC")