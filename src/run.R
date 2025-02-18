##############################################################
# Herramienta digital Análisis de Riesgo SR - run.R
# Organización Panamericana de la Salud
# Autor: Luis Quezada
# Última fecha de modificación: 2023-08-17
# R 4.3.0
##############################################################
 
Sys.setlocale(locale = "es_ES.UTF-8")

# SETUP                             -------------------------------
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
rm(list=ls())

library(pacman)
p_load(devtools,webshot,lubridate,forcats,stringr,dplyr,purrr,readr,tibble,
       tidyverse,tidyr,shinydashboard,shinyBS,shiny,shinycssloaders,sf,scales,
       rmarkdown,readxl,RColorBrewer,plotly,ggplot2,mapview,leaflet,janitor,
       htmltools,fontawesome,data.table,knitr,geojsonio,rmapshaper,sp,
       tinytex,DT,rio)

# 1. FLAG                 ----------------------------------------
file.copy(from = "Data/country_flag.png",to ="R/Dashboard/www/country_flag.png",overwrite = T)

# 2. EVAL                 ----------------------------------------
source("R/risk_eval.R")
file.copy(from = "SR_BD.RData",to ="R/Dashboard/SR_BD.RData",overwrite = T)
file.remove(from = "SR_BD.RData")

# 3. REPORT               ----------------------------------------
# HTML Report
rmarkdown::render("R/SR_report_HTML.Rmd", "html_document")
file.copy(from = "R/SR_report_HTML.html",to ="R/Dashboard/www/SR_report.html",overwrite = T)
file.remove(from = "R/SR_report_HTML.html")
# WORD Report
rmarkdown::render("R/SR_report_WORD.Rmd", "word_document")
file.copy(from = "R/SR_report_WORD.docx",to ="R/Dashboard/www/SR_report.docx",overwrite = T)
file.remove(from = "R/SR_report_WORD.docx")

# 4. DASHBOARD            ----------------------------------------
shiny::runApp("R/Dashboard/app.R")

