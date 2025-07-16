## ----------------------------------------
##
## Script name: update description file and other devs
##
## Purpose of the script: update description file + install and build the package
##
## -----------------------------------------
##
## Author: Cristina Martin Hernandez and Peter Levy
## Date created:  Wed Jul  9 12:51:01 2025
## Institution: UKCEH Edinburgh
## Contact: crimar@ceh.ac.uk
##
## -----------------------------------------
##
## Notes: using usethis and devtools packages
##
## -----------------------------------------

## Set the formatting to air and add a github action to enforce it----
usethis::use_air()
usethis::use_github_action(
  url = "https://github.com/posit-dev/setup-air/blob/main/examples/format-suggest.yaml"
)
# In VS code, use Ctrl+Shift+P: Air: Format Workspace Folder
# For Rstudio, see https://posit-dev.github.io/air/editor-rstudio.html

## Fill the DESCRIPTION ----
## Add meta data about your application
##
## /!\ Note: if you want to change the name of your app during development,
## either re-run this function, call golem::set_golem_name(), or don't forget
## to change the name in the app_sys() function in app_config.R /!\
##
usethis::use_description(
  fields = list(
    Title = "Quality Control Of Field Site Meteorological Data",
    Description = "This app aims to be an standarisation to the UKCEH metadata quality control and processing, such as gap filling. It is a R Shiny app that allows users to retrieve met data from a database, and process them in an standard way.",
    licence = "CC BY 4.0"
  )
)

usethis::use_author(
  given = "Peter",
  family = "Levy",
  email = "plevy@ceh.ac.uk",
  role = c("aut"),
  comment = c(ORCID = "0000-0002-8505-1901")
)
usethis::use_author(
  given = "Thomas",
  family = "Zwagerman",
  email = "thowag@ceh.ac.uk",
  role = c("aut", "cre")
)
usethis::use_author(
  given = "David",
  family = "Leaver",
  email = "leav@ceh.ac.uk",
  role = c("aut")
)
usethis::use_author(
  given = "Juan Pablo",
  family = "Lobo Guerrero",
  email = "jualob@ceh.ac.uk",
  role = c("ctb")
)

usethis::use_author(
  given = "Will",
  family = "Fincham",
  role = c("aut")
)
usethis::use_author(
  given = "Karen",
  family = "Yeung",
  email = "karung@ceh.ac.uk",
  role = c("ctb")
)

usethis::use_author(
  given = "Cristina",
  family = "Martin Hernandez",
  role = c("aut"),
  comment = c(ORCID = "0009-0003-9594-0299")
)
usethis::use_author(
  given = "Claudia",
  family = "Caporusso",
  role = c("aut")
)


######################################
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
  authors_roles = c("aut", "ctb", "cre"),
  encoding = "UTF-8"
)
