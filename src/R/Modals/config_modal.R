##############################################################
# Herramienta digital Análisis de Riesgo SR - config_modal.R
# Organización Panamericana de la Salud
# Autor: Luis Quezada
# Última fecha de modificación: 2023-08-11
# R 4.3.0
##############################################################

Sys.setlocale(locale = "es_ES.UTF-8")

# LIBS ----
library(shiny)
library(shinydashboard)
library(readxl)
library(fontawesome)

# LANG ----
LANG_TLS <- read_xlsx("configrm_lang.xlsx")
lang_label <- function(label) {
  return(LANG_TLS$LANG[LANG_TLS$LABEL == label])
}

# ICON ----
config_result <- as.logical(LANG_TLS$LANG[LANG_TLS$LABEL == "config_result"])
get_res_icon <- function(res) {
  status_class <- ""
  status_color <- ""
  if (res) {
    status_class <- "fa fa-check-circle"
    status_color <- "color: #2ecc71"
  } else {
    status_class <- "fa fa-times-circle"
    status_color <- "color: #dd4b39"
  }
  return(div(tags$i(class = status_class,style = status_color),lang_label("config_title_res")))
}

ui <- bootstrapPage(
  column(width = 12,
         box(
           title = get_res_icon(config_result), width = NULL, solidHeader = TRUE, status = "info", collapsible = F,
           HTML(paste0(lang_label("config_msg_res"),lang_label("config_to_be_installed"))),
           br(),br(),
           actionButton("close_butt",lang_label("button_close"),icon=icon("close")),
           br()
         )
  )
)

# SERVER ----
server <- function(input, output, session) {
  observeEvent(input$close_butt, {
    stopApp()
  })
}

# RUN ----
if (exists(".rs.invokeShinyWindowViewer")) {
  shinyApp(ui = ui, server = server, options = options(shiny.launch.browser = .rs.invokeShinyWindowViewer))
} else {
  shinyApp(ui = ui, server = server)
}

