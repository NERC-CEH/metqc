library(lubridate)
library(ggExtra)
dbuid <- "BU_FIELD_SITES" 
dbpwd <- "0ig2mtYUL9" 
drv <- dbDriver("Oracle")
con <- dbConnect(drv, dbname = "budbase.nerc-bush.ac.uk/BUA",
                 username = dbuid,
                 password = dbpwd)
table_name <- "MET_30MIN"                      
dbNames <<- dbListFields(con, table_name)
dbNamesForBox <- dbNames[!dbNames %in% c("DATECT","TIMESTAMP","DATECT_NUM","checked","pred")]

qry_variables <-"TS"
date1 <- '2017/01/01 00:00'
date2 <- '2017/12/18 05:20'

qry <- paste0("SELECT DATECT, TIMESTAMP, ",qry_variables," FROM ", table_name, 
              " WHERE DATECT > TO_DATE('", date1, "', 'yyyy/mm/dd hh24:mi') 
                     AND DATECT < TO_DATE('", date2, "', 'yyyy/mm/dd hh24:mi')")         
df_qry <<- dbGetQuery(con, qry)

plot_heatmap_calendar <- function(input_variable, df_qry){
date_coverage_df <- df_qry %>% 
  mutate(year = year(DATECT),
         day_of_the_week = lubridate::wday(DATECT, label = TRUE, week_start = 1),
         month = lubridate::month(DATECT, label = TRUE, abbr = FALSE),
         week = isoweek(DATECT),
         day = day(DATECT)) %>% 
  select(year,month,day,week,
         day_of_the_week)
date_coverage_df$week <- as.double(date_coverage_df$week)
date_coverage_df <- dplyr::mutate(date_coverage_df, 
                 week = case_when(month == "December" & week == 1 ~ 53,
                                  month == "January" & week %in% 52:53 ~ 0,
                                  TRUE ~ week))
date_coverage_df$variable <- qry_variables
date_coverage_df$has_been_checked <- FALSE
date_coverage_df$has_been_checked[date_coverage_df$month == "February"] <- TRUE

heatmap_plot <- ggplot(date_coverage_df,aes(day_of_the_week, -week, fill = has_been_checked))+
  geom_tile(color= "white",size=0.1) + 
  #scale_fill_viridis(name="Hrly Temps C",option ="C")+
  facet_wrap(year~month, nrow = 4, ncol =3, scales ="free")+
  #facet_grid(year~month)+
  #scale_y_reverse()+
  #scale_x_discrete()+
  theme_minimal(base_size = 8)+ 
  theme(legend.position = "bottom")+
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=6)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=7))+
  theme(legend.title=element_text(size=8))+
  theme(legend.text=element_text(size=6))+
  removeGrid()
heatmap_plot
}

plot_heatmap_calendar("TS", df_qry)