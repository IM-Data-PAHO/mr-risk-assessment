##############################################################
# Herramienta digital Análisis de Riesgo SR - config.R
# Organización Panamericana de la Salud
# Autor: Luis Quezada
# Última fecha de modificación: 2023-08-25
# R 4.3.0
##############################################################
if (!require("rstudioapi")) {install.packages("rstudioapi")}
# Working dir ----
# Working directory set from R project
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
Sys.setlocale(locale = "es_ES.UTF-8")

# Pacman ----
# Install and load package manager
if (!require("pacman")) {install.packages("pacman")}
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library(pacman)

# Install ----
# Install required packages if not already installed
p_load(devtools,webshot,lubridate,forcats,stringr,dplyr,purrr,readr,tibble,
       tidyverse,tidyr,shinydashboard,shinyBS,shiny,shinycssloaders,sf,scales,
       rmarkdown,readxl,RColorBrewer,plotly,ggplot2,mapview,leaflet,janitor,
       htmltools,fontawesome,DT,data.table,knitr,geojsonio,rmapshaper,sp,
       tinytex,rsconnect,writexl, rio,
       install = T)

# Installation of phantomjs from Github
webshot::install_phantomjs(force=T)
# Installation of TinyTeX via tinytex package
if (!(tinytex::is_tinytex())) {tinytex::install_tinytex(force=T)}

loaded_packages = p_loaded()
to_be_installed <- setdiff(
  c("devtools","webshot","lubridate","forcats","stringr","dplyr","purrr","readr","tibble",
    "tidyverse","tidyr","shinydashboard","shinyBS","shiny","sf","scales","rmarkdown",
    "readxl","RColorBrewer","plotly","ggplot2","mapview","leaflet","janitor","htmltools",
    "fontawesome","DT","data.table","knitr","geojsonio","rmapshaper","sp","tinytex",
    "shinycssloaders","rsconnect","writexl"), loaded_packages)

# LANG MSG ----
LANG <- as.character(read_excel("Data/country_data.xlsx",sheet = 1)[8,2])
if (!(LANG %in% c("SPA","ENG","FRA","POR"))) {LANG = "SPA"}
LANG_TLS <- read_excel("R/translations.xlsx",sheet="MSG") %>% select(LABEL,all_of(LANG))
colnames(LANG_TLS) <- c("LABEL","LANG")
if (length(to_be_installed)>0) {
  LANG_TLS$LABEL[LANG_TLS$LABEL == "config_title_incorrect"] = "config_title_res"
  LANG_TLS$LABEL[LANG_TLS$LABEL == "config_msg_incorrect"] = "config_msg_res"
  LANG_TLS <- rbind(LANG_TLS,c("config_to_be_installed",paste0(" ",toString(to_be_installed))))
  LANG_TLS <- rbind(LANG_TLS,c("config_result","FALSE"))
} else {
  LANG_TLS$LABEL[LANG_TLS$LABEL == "config_title_correct"] = "config_title_res"
  LANG_TLS$LABEL[LANG_TLS$LABEL == "config_msg_correct"] = "config_msg_res"
  LANG_TLS <- rbind(LANG_TLS,c("config_to_be_installed","."))
  LANG_TLS <- rbind(LANG_TLS,c("config_result","TRUE"))
}
write_xlsx(LANG_TLS,"R/Modals/configrm_lang.xlsx")

# Modal ----
shiny::runApp("R/Modals/config_modal.R")

