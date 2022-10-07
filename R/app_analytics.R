
############################################################
#                                                          #
#                   MetQC App Analytics                    #
#                                                          #
############################################################

#remotes::install_github('rstudio/connectapi')
# API key (created on the connect server website) and server  are saved in the projects environ file by using:
#usethis::edit_r_environ('project')

library(connectapi)
library(dplyr)

client <- connect()

usage_shiny <- get_usage_shiny(client)

usage_shiny <- usage_shiny %>% 
  mutate(time_active = lubridate::seconds_to_period(ended - started))

##

all_content <- get_content(client, limit = Inf) %>% 
  filter(name %in% c('metqc', 'flagged_data', 'level2_data', 'level1_data', 'era5_data', 'data flags'))

# End