## R script to build the metqc package
##
## Peter Levy, CEH Edinburgh
## Bush Estate, Penicuik, EH26 0QB, U.K.
## plevy@ceh.ac.uk
## Tel: 0131 445 8556

here::i_am("scripts/make_metqc.R")
#rm(list=ls(all=TRUE))
library(knitr)
library(devtools)  # alternative is devtools::install_github("klutometis/roxygen")
library(roxygen2)
getwd()
setwd("..")
setwd("./metqc")
#devtools::use_data(nSectors, sectorName, sectorLongName, alpha_year_byGHG_df, mod.yday, alpha_wday_df, mod.hour, overwrite = TRUE)
#devtools::use_data(ch4BySector, co2BySector, n2oBySector, internal = TRUE)

check_man()
document()
clean_vignettes()
build_vignettes()

# build the manual
#Sys.getenv("PKG_CONFIG_PATH")
#Sys.getenv(c("R_TEXI2DVICMD", "R_PAPERSIZE", "RD2PDF_INPUTENC"))
#Sys.setenv(RD2PDF_INPUTENC = "inputenx ")
pack <- "metqc"
path <- find.package(pack)
system(paste(shQuote(file.path(R.home("bin"), "R")),"CMD", "Rd2pdf", shQuote(path)))
#C:/PROGRA~1/R/R-32~1.4RE/bin/x64/R R CMD Rd2pdf --no-clean N:/0Peter/prop/UKinverseFlux/GHG_TAP/DelD/anthEmis/metqc

#check()
build()
build(manual = FALSE, vignettes = TRUE)
build(binary = TRUE)

setwd("..")
install("metqc")
install.packages("./metqc_0.0.1.tar.gz", repos = NULL, type="source")
library(metqc)
vignette("use_metqc")
?metqc
?impute
?pad_data
runShinyApp()
