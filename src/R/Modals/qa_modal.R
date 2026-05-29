##############################################################
# Herramienta digital Análisis de Riesgo SR - qa_modal.R
# Organización Panamericana de la Salud
# Autor: Luis Quezada
# Última fecha de modificación: 2023-08-10
# R 4.3.0
##############################################################

Sys.setlocale(locale = "es_ES.UTF-8")

# LIBS ----
library(shiny)
library(shinydashboard)
library(readxl)
library(fontawesome)

# LANG ----
LANG_TLS <- read_xlsx("qarm_lang.xlsx")
lang_label <- function(label) {
  return(LANG_TLS$LANG[LANG_TLS$LABEL == label])
}

# ICON ----
qa_result <- as.logical(LANG_TLS$LANG[LANG_TLS$LABEL == "qa_result"])
get_res_icon <- function(qa_res) {
  status_class <- ""
  status_color <- ""
  if (qa_res) {
    status_class <- "fa fa-check-circle"
    status_color <- "color: #2ecc71"
  } else {
    status_class <- "fa fa-times-circle"
    status_color <- "color: #dd4b39"
  }
  return(div(tags$i(class = status_class,style = status_color),lang_label("qa_report_result_title")))
}

ui <- bootstrapPage(
  column(width = 12,
         box(
           title = get_res_icon(qa_result), width = NULL, solidHeader = TRUE, status = "info", collapsible = F,
           HTML(lang_label("qa_report_result_msg")),
           br(),br(),
           tags$style(type="text/css", "
                       #download_report {background-color: white;color: black;border-radius: 5px !important;"),
           div(
             downloadButton("download_report", lang_label("qa_report_download"),icon=icon('file-lines',style='font-size: 15px'), class = "butt1"),
             actionButton("close_butt",lang_label("button_close"),icon=icon("close"))
           ),
         )
  )
)

# SERVER ----
server <- function(input, output, session) {
  output$download_report <- downloadHandler(
    filename = paste0(lang_label("qa_report_filename"),".docx"),
    content = function(file) {
      file.copy("www/QA_report.docx", file)
    }
  )
  
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

