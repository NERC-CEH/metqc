## ----------------------------------------
##
## Script name: update description file and other devs
##
## Purpose of the script: update description file + install and build the package
##
## -----------------------------------------
##
## Author: Cristina Martin Hernandez
## Date created:  Wed Jul  9 12:51:01 2025
## Institution: UKCEH Edinburgh 
## Contact: crimar@ceh.ac.uk
##
## -----------------------------------------
##
## Notes: using usethis and devtools packages
##
## -----------------------------------------

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
usethis::use_author(
  given = "Will",
  family = "Fincham",
  role = c("aut")
)
usethis::use_author(
  given = "Cristina",
  family = "Martin Hernandez",
  role = c("aut")
)
usethis::use_author(
  given = "Claudia",
  family = "Caporusso",
  role = c("aut")
)

usethis::use_description(fields = list(
  Title = "Metqc app",
  Description = "This app aims to be an standarisation to the UKCEH metadata quality control and processing, such as gap filling. It is a R Shiny app that allows users to retrieve met data from a database, and process them in an standard way."
))

2######################################
# adding a Citation File Format (CFF)#
######################################

cffr::cff_write(
  x = "./DESCRIPTION",
  outfile = "CITATION.cff",
  keys = list(),
  cff_version = "1.2.0",
  gh_keywords = TRUE,
  r_citation = FALSE,
  dependencies = TRUE,
  validate = TRUE,
  verbose = TRUE,
  authors_roles =  c("aut"),
  encoding = "UTF-8"
)
