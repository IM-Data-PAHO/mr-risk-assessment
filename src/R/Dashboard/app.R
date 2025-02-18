##############################################################
# Herramienta digital Análisis de Riesgo SR - app.R
# Organización Panamericana de la Salud
# Autor: Luis Quezada
# Última fecha de modificación: 2023-08-18
# R 4.3.0
##############################################################

Sys.setlocale(locale = "es_ES.UTF-8")

# LIBS ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinycssloaders)
library(fontawesome)
library(plotly)
library(leaflet)
library(readxl)
library(data.table)
library(tidyr)
library(DT)
library(janitor)
library(RColorBrewer)
library(sf)
library(htmltools)
library(tidyverse)
library(scales)
library(mapview)
library(webshot)
if (!webshot::is_phantomjs_installed()) {
webshot::install_phantomjs(version = "2.1.1", force = TRUE)
}
# LOAD DATA ----
load(file = "SR_BD.RData")

# FUNCS ----
get_a1_geo_id <- function(admin1) {
  return(admin1_geo_id_df$`ADMIN1 GEO_ID`[admin1_geo_id_df$ADMIN1 == admin1])
}

lang_label <- function(label) {
  return(LANG_TLS$LANG[LANG_TLS$LABEL == label])
}

# TITLES ----

title_map_box <- function(indicator,admin1) {
  indicator = tolower(indicator)
  if (indicator == tolower(lang_label("menuitem_general_label"))) {
    indicator = lang_label("total")
  } else {
    indicator = paste0("(",indicator,")")
  }
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("general_title_map_box")," ",indicator," - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("general_title_map_box")," ",indicator," - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}

title_bar_box <- function(indicator,admin1) {
  indicator = tolower(indicator)
  if (indicator == tolower(lang_label("menuitem_general_label"))) {
    indicator = lang_label("total")
  } else {
    indicator = paste0("(",indicator,")")
  }
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("general_title_bar_box")," ",indicator," - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("general_title_bar_box")," ",indicator," - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}

title_data_box <- function(indicator,admin1) {
  indicator = tolower(indicator)
  if (indicator == tolower(lang_label("menuitem_general_label"))) {
    indicator = lang_label("total")
  } else {
    indicator = paste0("(",indicator,")")
  }
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("general_title_data_box")," ",indicator," - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("general_title_data_box")," ",indicator," - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}

title_pie_box <- function(indicator,admin1) {
  indicator = tolower(indicator)
  if (admin1 == toupper(lang_label("rep_label_all"))) {
    title_text <- paste0(lang_label("title_pie_box")," (",indicator,") - ",toupper(COUNTRY_NAME))
  } else {
    title_text <- paste0(lang_label("title_pie_box")," (",indicator,") - ",admin1,", ",toupper(COUNTRY_NAME))
  }
  return(title_text)
}


# SOURCE ----
source("general.R")
source("inm_pob.R")
source("surv_qual.R")
source("prog_del.R")
source("vul_group.R")
source("thre_asse.R")
source("rap_res.R")

# UI ----
ui <- fluidPage(
    ## HEADER ----
    fluidRow(
        box(width = 12, background = "maroon",
            HTML(paste0('<center><div style = "text-align: left; padding-left: 30px; padding-right: 30px; padding-top: 10px;">
                        <img src="',lang_label("logo_org"),'" height="35"> <img id="country_flag" style = "right: 30px !important; position: absolute; padding-top: 1px; padding-bottom: 1px; padding-right: 1px; padding-left: 1px; margin-bottom: 10px; background-color: white;" src="country_flag.png" height="50">
                        </div> <h2>',lang_label("dashboard_title"),' - <b>',toupper(COUNTRY_NAME),'</b></h2> </center>'))
        )
    ),
    
    dashboardPage(
        
        skin = "purple",
        title = lang_label("dashboard_tab_title"),
        
        header = dashboardHeader(
          titleWidth = "30%",
          title = paste0(lang_label("header_year_eval"),": ",YEAR_EVAL)
        ),
        
        dashboardSidebar(
          width = 300,
          
          sidebarMenu( 
            id = "sidebarid",
            div(
              downloadButton("download_report_word",lang_label("download_report_word"), icon=icon("file-lines"), class = "button_word"),
              downloadButton("download_report_html",lang_label("download_report_html"), icon=icon("file-lines"), class = "button_html")
            ),
            
            # GENERAL IND ----
            menuItem(lang_label("menuitem_general"),
                     tabName = "GENERAL",
                     icon = icon("square-check")
            ),
            conditionalPanel(
              'input.sidebarid == "GENERAL"',
              
              selectInput("indicadores_select_indicador", label=paste0(lang_label("general_select_ind"),":"),
                          choices = c(
                            lang_label("menuitem_general_label"),
                            lang_label("menuitem_inm_pob"),
                            lang_label("menuitem_prog_del"),
                            lang_label("menuitem_surv_qual"),
                            lang_label("menuitem_thre_asse"),
                            lang_label("menuitem_rap_res")
                          ),
                          selected = lang_label("menuitem_general_label")),
              bsTooltip("indicadores_select_indicador", lang_label("tooltip_select_ind"), placement = "right", trigger = "hover",options = NULL),
              
              selectInput("indicadores_select_admin1",label=paste0(lang_label("general_select_admin1"),":"),choices = admin1_list,selected = admin1_list[1]),
              bsTooltip("indicadores_select_admin1", lang_label("tooltip_select_admin1"), placement = "right", trigger = "hover",options = NULL),
              
              selectInput("indicadores_select_risk",label=paste0(lang_label("general_select_risk"),":"),
                          choices = c(toupper(lang_label("rep_label_all")),
                                      lang_label("cut_offs_VHR"),
                                      lang_label("cut_offs_HR"),
                                      lang_label("cut_offs_MR"),
                                      lang_label("cut_offs_LR")
                                      )),
              bsTooltip("indicadores_select_risk", lang_label("tooltip_select_risk"), placement = "right", trigger = "hover",options = NULL)
            ),
            
            # INM_POB ----
            menuItem(lang_label("menuitem_inm_pob"),
                     tabName = "inmunidad",
                     icon = icon("syringe")
            ),
            conditionalPanel(
              'input.sidebarid == "inmunidad"',
              selectInput("inmunidad_select_admin1", label=paste0(lang_label("general_select_admin1"),":"),choices = admin1_list,selected = admin1_list[1]),
              bsTooltip("inmunidad_select_admin1", lang_label("tooltip_select_admin1"), placement = "right", trigger = "hover",options = NULL)
            ),
            
            # PROG_DEL ----
            menuItem(lang_label("menuitem_prog_del"),
                     tabName = "rendimiento",
                     icon = icon("line-chart")
            ),
            conditionalPanel(
              'input.sidebarid == "rendimiento"',
              selectInput("rendimiento_select_admin1", label=paste0(lang_label("general_select_admin1"),":"),choices = admin1_list,selected = admin1_list[1]),
              bsTooltip("rendimiento_select_admin1", lang_label("tooltip_select_admin1"), placement = "right", trigger = "hover",options = NULL)
            ),
            
            # SURV_QUAL ----
            menuItem(lang_label("menuitem_surv_qual"),
                     tabName = "calidad",
                     icon = icon("eye",class="fa-solid fa-eye")
            ),
            conditionalPanel(
              'input.sidebarid == "calidad"',
              selectInput("calidad_select_admin1", label=paste0(lang_label("general_select_admin1"),":"),choices = admin1_list,selected = admin1_list[1]),
              bsTooltip("calidad_select_admin1", lang_label("tooltip_select_admin1"), placement = "right", trigger = "hover",options = NULL)
            ),
            
            # THRE_ASSE ----
            menuItem(lang_label("menuitem_thre_asse"),
                     tabName = "amenaza",
                     icon = icon("pen-to-square")
            ),
            conditionalPanel(
              'input.sidebarid == "amenaza"',
              selectInput("amenaza_select_admin1", label=paste0(lang_label("general_select_admin1"),":"),choices = admin1_list,selected = admin1_list[1]),
              bsTooltip("amenaza_select_admin1", lang_label("tooltip_select_admin1"), placement = "right", trigger = "hover",options = NULL),
            ),
            
            # RAP_RES ----
            menuItem(lang_label("menuitem_rap_res"),
                     tabName = "resrapida",
                     icon = icon("user-md")
            ),
            conditionalPanel(
              'input.sidebarid == "resrapida"',
              selectInput("resrapida_select_admin1", label=paste0(lang_label("general_select_admin1"),":"),choices = admin1_list,selected = admin1_list[1]),
              bsTooltip("resrapida_select_admin1", lang_label("tooltip_select_admin1"), placement = "right", trigger = "hover",options = NULL),
            )
        )
    ),
        
    dashboardBody(
      fluidPage(
        # JS  ----
        tags$head(tags$script(src = "message-handler.js")),
        # CSS ----
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
        
        tabItems(
          
          # Tab GENERAL IND ----
          tabItem(tabName = "GENERAL",
                  h2(textOutput("indicadores_title")),
                  br(),
                  
                  fluidRow(
                    valueBoxOutput("ind_box_1",width = 3),
                    valueBoxOutput("ind_box_2",width = 3),
                    valueBoxOutput("ind_box_3",width = 3),
                    valueBoxOutput("ind_box_4",width = 3)
                  ),
                  
                  conditionalPanel(
                    paste0('input.indicadores_select_admin1 == "',toupper(lang_label("rep_label_all")),'"'),
                    fluidRow(
                      box(width = 12,
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          title = textOutput("indicadores_title_map_box"),
                          
                          tabBox(width = 12,
                                 height = NULL,
                                 tabPanel(
                                   title = lang_label("button_map"),icon = icon("map",class="fa-solid fa-map"),
                                   shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                                   br(),div(style="text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map",lang_label("button_download_map"),icon=icon('camera')))
                                 ),
                                 
                                 tabPanel(
                                   title = lang_label("button_datatable"),icon = icon("table"),
                                   shinycssloaders::withSpinner(dataTableOutput("indicadores_table",height = 620),color = "#1c9ad6", type = "8", size = 0.5)
                                 )
                          )
                      )
                    )
                  ),
                  
                  conditionalPanel( # Display both map and bar if an admin1 is selected
                    paste0('input.indicadores_select_admin1 != "',toupper(lang_label("rep_label_all")),'"'),
                    fluidRow(
                      box(width = 6,
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          title = textOutput("indicadores_title_map_box_2"),
                          
                          tabBox(width = 12,
                                 height = NULL,
                                 tabPanel(
                                   title = lang_label("button_map"),icon = icon("map",class="fa-solid fa-map"),
                                   shinycssloaders::withSpinner(leafletOutput("indicadores_plot_map_2",height = 600),color = "#1c9ad6", type = "8", size = 0.5),
                                   br(),div(style="text-align: center;",downloadButton(outputId = "dl_indicadores_plot_map_2",lang_label("button_download_map"),icon=icon('camera')))
                                 ),
                                 
                                 tabPanel(
                                   title = lang_label("button_datatable"),icon = icon("table"),
                                   shinycssloaders::withSpinner(dataTableOutput("indicadores_table_2",height = 620),color = "#1c9ad6", type = "8", size = 0.5)
                                 )
                          )
                      ),
                      box(width = 6,
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          title = textOutput("indicadores_title_bar_box"),
                          tabBox(width = 12,
                                 height = NULL,
                                 tabPanel(
                                   title = lang_label("general_title_plot_bar"),icon = icon("bar-chart"),
                                   shinycssloaders::withSpinner(plotlyOutput("indicadores_plot_bar",height = 595),color = "#1c9ad6", type = "8", size = 0.5)
                                 ),
                                 tabPanel(
                                   title = lang_label("general_title_plot_multibar"),icon = icon("square-check"),
                                   shinycssloaders::withSpinner(plotlyOutput("indicadores_plot_multibar",height = 595),color = "#1c9ad6", type = "8", size = 0.5)
                                 )
                          )
                      )
                    )
                  ),
                  fluidRow(
                    column(width = 6, offset = 3,
                           box(width = 12,
                               solidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                               shinycssloaders::withSpinner(dataTableOutput("indicadores_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3)
                           )
                    )
                  )
          ),
          
          # Tab INM_POB ----
          tabItem(tabName = "inmunidad",
                  h2(lang_label("menuitem_inm_pob")),
                  br(),
                  
                  fluidRow(
                    box(width = 7,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("inmunidad_title_map_box"),
                        
                        tabBox(width = 12,
                               height = NULL,
                               
                               tabPanel(
                                 title = lang_label("total_pr"),icon = icon("square-check"),
                                 shinycssloaders::withSpinner(leafletOutput("inmunidad_map_total",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_inmunidad_map_total",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("inm_mmr1_cob"),icon = icon("syringe"),
                                 column(width = 12,
                                        selectInput("radio_inmunidad_cob_1", label ="", 
                                                    choices = c(
                                                      paste(lang_label("vac_coverage"),YEAR_1),
                                                      paste(lang_label("vac_coverage"),YEAR_2),
                                                      paste(lang_label("vac_coverage"),YEAR_3),
                                                      paste(lang_label("vac_coverage"),YEAR_4),
                                                      paste(lang_label("vac_coverage"),YEAR_5)
                                                    ),
                                        ),style="z-index:2000;"),
                                 shinycssloaders::withSpinner(leafletOutput("inmunidad_map_cob_1",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_inmunidad_map_cob_1",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("inm_mmr2_cob"),icon = icon("syringe"),
                                 column(width = 12,
                                        selectInput("radio_inmunidad_cob_2", label ="", 
                                                    choices = c(
                                                      paste(lang_label("vac_coverage"),YEAR_1),
                                                      paste(lang_label("vac_coverage"),YEAR_2),
                                                      paste(lang_label("vac_coverage"),YEAR_3),
                                                      paste(lang_label("vac_coverage"),YEAR_4),
                                                      paste(lang_label("vac_coverage"),YEAR_5)
                                                    ),
                                        ),
                                        style="z-index:2000;"),
                                 shinycssloaders::withSpinner(leafletOutput("inmunidad_map_cob_2",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_inmunidad_map_cob_2",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("inm_cob_last_camp"),icon = icon("syringe"),
                                 shinycssloaders::withSpinner(leafletOutput("inmunidad_map_camp",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_inmunidad_map_camp",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("inm_novac"),icon = icon("question-circle"),
                                 p(style="text-align: center;",lang_label("inm_novac_text")),
                                 shinycssloaders::withSpinner(leafletOutput("inmunidad_map_casos",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_inmunidad_map_casos",lang_label("button_download_map"),icon=icon('camera')))
                               )
                        )
                    ),
                    box(width = 5,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("inmunidad_title_pie_box"),
                        
                        tabBox(width = 12,
                               height = NULL,
                               
                               tabPanel(
                                 title = lang_label("button_plot"),icon = icon("pie-chart"),
                                 br(),
                                 shinycssloaders::withSpinner(plotlyOutput("inmunidad_plot_pie"),color = "#1c9ad6", type = "8", size = 0.5)
                               ),
                               
                               tabPanel(
                                 title = lang_label("button_datatable"),icon = icon("table"),
                                 br(),
                                 shinycssloaders::withSpinner(dataTableOutput("inmunidad_table_dist"),color = "#1c9ad6", type = "8", size = 0.5)
                               )
                        )
                    )
                  ),
                  
                  fluidRow(
                    box(width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("inmunidad_title_data_box"),
                        column(width = 12,shinycssloaders::withSpinner(dataTableOutput("inmunidad_table"),color = "#1c9ad6", type = "8", size = 0.5))
                    )
                  ),
                  
                  fluidRow(
                    column(width = 3),
                    column(width = 6,
                           box(width = 12,
                               solidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                               shinycssloaders::withSpinner(dataTableOutput("inmu_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3)
                           )
                    ),
                    column(width = 3)
                  )
          ),
          
          # Tab SURV_QUAL ----
          tabItem(tabName = "calidad",
                  h2(lang_label("menuitem_surv_qual")),
                  br(),
                  
                  fluidRow(
                    box(width = 7,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("calidad_title_map_box"),
                        
                        tabBox(width = 12,
                               height = NULL,
                               
                               tabPanel(
                                 title = lang_label("total_pr"),icon = icon("square-check"),
                                 shinycssloaders::withSpinner(leafletOutput("calidad_map_total",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_calidad_map_total",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("surv_rate_novac"),icon = icon("calculator"),
                                 shinycssloaders::withSpinner(leafletOutput("calidad_map_1",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_calidad_map_1",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("surv_adeq_inv"),icon = icon("search-plus"),
                                 shinycssloaders::withSpinner(leafletOutput("calidad_map_2",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_calidad_map_2",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("surv_adeq_sample"),icon = icon("folder-open"),
                                 shinycssloaders::withSpinner(leafletOutput("calidad_map_3",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_calidad_map_3",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("surv_timely_lab"),icon = icon("flask"),
                                 shinycssloaders::withSpinner(leafletOutput("calidad_map_4",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_calidad_map_4",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               ## Silent municipalities ---- 
                               tabPanel(
                                 title = lang_label("silent_mun_lab"),icon = icon("bell-slash"),
                                 shinycssloaders::withSpinner(leafletOutput("calidad_map_5",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_calidad_map_5",lang_label("button_download_map"),icon=icon('camera')))
                               )
                        )
                    ),
                    box(width = 5,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("calidad_title_pie_box"),
                        
                        tabBox(width = 12,
                               height = NULL,
                               
                               tabPanel(
                                 title = lang_label("button_plot"),icon = icon("pie-chart"),
                                 br(),
                                 shinycssloaders::withSpinner(plotlyOutput("calidad_plot_pie"),color = "#1c9ad6", type = "8", size = 0.5)
                               ),
                               
                               tabPanel(
                                 title = lang_label("button_datatable"),icon = icon("table"),
                                 br(),
                                 shinycssloaders::withSpinner(dataTableOutput("calidad_table_dist"),color = "#1c9ad6", type = "8", size = 0.5)
                               )
                        )
                        ## Silent municipalities ValueBox ----
                        
                    ),
                    valueBoxOutput("ind_box_silent_mun",width = 5)
                  ),
                  
                  fluidRow(
                    box(width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("calidad_title_data_box"),
                        column(width = 12,shinycssloaders::withSpinner(dataTableOutput("calidad_table"),color = "#1c9ad6", type = "8", size = 0.5))
                    )
                  ),
                  
                  fluidRow(
                    column(width = 3),
                    column(width = 6,
                           box(width = 12,
                               solidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                               shinycssloaders::withSpinner(dataTableOutput("cal_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3)
                           )
                    ),
                    column(width = 3)
                  )
          ),
          
          # Tab PROG_DEL ----
          tabItem(tabName = "rendimiento",
                  h2(lang_label("menuitem_prog_del")),
                  br(),
                  
                  fluidRow(
                    box(width = 7,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("rendimiento_title_map_box"),
                        
                        tabBox(width = 12,
                               height = NULL,
                               
                               tabPanel(
                                 title = lang_label("total_pr"),icon = icon("square-check"),
                                 shinycssloaders::withSpinner(leafletOutput("rendimiento_map_total",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_rendimiento_map_total",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("prog_cob_trend"),icon = icon("line-chart"),
                                 column(width = 12,
                                        selectInput("radio_rendimiento_map_1", label ="", 
                                                    choices = c(lang_label("mmr1"),lang_label("mmr2")),
                                        ),style="z-index:2000;"),
                                 shinycssloaders::withSpinner(leafletOutput("rendimiento_map_1",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_rendimiento_map_1",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("prog_dropout_rate"),icon = icon("minus-square"),
                                 column(width = 12,
                                        selectInput("radio_rendimiento_map_2", label ="", 
                                                    choices = c(lang_label("prog_mmr1_mmr2"),lang_label("prog_penta1_mmr1")),
                                        ),style="z-index:2000;"),
                                 shinycssloaders::withSpinner(leafletOutput("rendimiento_map_2",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_rendimiento_map_2",lang_label("button_download_map"),icon=icon('camera')))
                               )
                        )
                    ),
                    box(width = 5,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("rendimiento_title_pie_box"),
                        
                        tabBox(width = 12,
                               height = NULL,
                               
                               tabPanel(
                                 title = lang_label("button_plot"),icon = icon("pie-chart"),
                                 shinycssloaders::withSpinner(plotlyOutput("rendimiento_plot_pie"),color = "#1c9ad6", type = "8", size = 0.5)
                               ),
                               
                               tabPanel(
                                 title = lang_label("button_datatable"),icon = icon("table"),
                                 shinycssloaders::withSpinner(dataTableOutput("rendimiento_table_dist"),color = "#1c9ad6", type = "8", size = 0.5)
                               )
                        )
                    )
                  ),
                  
                  fluidRow(
                    box(width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("rendimiento_title_data_box"),
                        column(width = 12,shinycssloaders::withSpinner(dataTableOutput("rendimiento_table"),color = "#1c9ad6", type = "8", size = 0.5))
                    )
                  ),
                  
                  fluidRow(
                    column(width = 3),
                    column(width = 6,
                           box(width = 12,
                               solidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                               shinycssloaders::withSpinner(dataTableOutput("rend_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3)
                           )
                    ),
                    column(width = 3)
                  )
          ),
          
          # Tab THRE_ASSE ----
          tabItem(tabName = "amenaza",
                  h2(lang_label("menuitem_thre_asse")),
                  br(),
                  
                  fluidRow(
                    box(width = 7,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("amenaza_title_map_box"),
                        
                        tabBox(width = 12,
                               height = NULL,
                               
                               tabPanel(
                                 title = lang_label("total_pr"),icon = icon("square-check"),
                                 shinycssloaders::withSpinner(leafletOutput("amenaza_map_total",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_amenaza_map_total",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("thre_pop_dens"),icon = icon("users"),
                                 shinycssloaders::withSpinner(leafletOutput("amenaza_map_1",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_amenaza_map_1",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("thre_vul"),icon = icon("exclamation-circle"),
                                 column(width = 12,
                                        selectInput("radio_amenaza_map_2", label ="", 
                                                    choices = c(
                                                      lang_label("thre_risk_level"),
                                                      lang_label("thre_pres_inter_pob"),
                                                      lang_label("thre_pres_turism"),
                                                      lang_label("thre_pres_prob"),
                                                      lang_label("thre_pres_calam"),
                                                      lang_label("thre_dif_topo"),
                                                      lang_label("thre_pres_com"),
                                                      lang_label("thre_pres_trafic"),
                                                      lang_label("thre_pres_events")
                                                    ),
                                        ),
                                        style="z-index:2000;"),
                                 div(style="text-align: center;",textOutput("vulnerables_pres_subtitle")),
                                 br(),
                                 shinycssloaders::withSpinner(leafletOutput("amenaza_map_2",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_amenaza_map_2",lang_label("button_download_map"),icon=icon('camera')))
                               )
                        )
                    ),
                    box(width = 5,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("amenaza_title_pie_box"),
                        
                        tabBox(width = 12,
                               height = NULL,
                               
                               tabPanel(
                                 title = lang_label("button_plot"),icon = icon("pie-chart"),
                                 br(),
                                 shinycssloaders::withSpinner(plotlyOutput("amenaza_plot_pie"),color = "#1c9ad6", type = "8", size = 0.5)
                               ),
                               
                               tabPanel(
                                 title = lang_label("button_datatable"),icon = icon("table"),
                                 br(),
                                 shinycssloaders::withSpinner(dataTableOutput("amenaza_table_dist"),color = "#1c9ad6", type = "8", size = 0.5)
                               )
                        )
                    )
                  ),
                  fluidRow(
                    box(width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("amenaza_title_data_box"),
                        column(width = 12,shinycssloaders::withSpinner(dataTableOutput("amenaza_table"),color = "#1c9ad6", type = "8", size = 0.5))
                    )
                  ),
                  
                  fluidRow(
                    column(width = 3),
                    column(width = 6,
                           box(width = 12,
                               solidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                               shinycssloaders::withSpinner(dataTableOutput("eval_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3)
                           )
                    ),
                    column(width = 3)
                  )
          ),
          
          # Tab RAP_RES ----
          tabItem(tabName = "resrapida",
                  h2(lang_label("menuitem_rap_res")),
                  br(),
                  
                  fluidRow(
                    box(width = 7,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("resrapida_title_map_box"),
                        
                        tabBox(width = 12,
                               height = NULL,
                               
                               tabPanel(
                                 title = lang_label("total_pr"),icon = icon("square-check"),
                                 br(),
                                 shinycssloaders::withSpinner(leafletOutput("resrapida_map_total",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_resrapida_map_total",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("rap_pres_team"),icon = icon("user-md"),
                                 br(),
                                 shinycssloaders::withSpinner(leafletOutput("resrapida_map_1",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_resrapida_map_1",lang_label("button_download_map"),icon=icon('camera')))
                               ),
                               
                               tabPanel(
                                 title = lang_label("rap_pres_hospital"),icon = icon("user-clock"),
                                 br(),
                                 p(style="text-align: center;",lang_label("rap_pres_hospital_note")),
                                 br(),
                                 shinycssloaders::withSpinner(leafletOutput("resrapida_map_2",height = 500),color = "#1c9ad6", type = "8", size = 0.5),
                                 br(),div(style="text-align: center;",downloadButton(outputId = "dl_resrapida_map_2",lang_label("button_download_map"),icon=icon('camera')))
                               )
                        )
                    ),
                    box(width = 5,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        height = 680,
                        title = textOutput("resrapida_title_pie_box"),
                        
                        tabBox(width = 12,
                               height = NULL,
                               
                               tabPanel(
                                 title = lang_label("button_plot"),icon = icon("pie-chart"),
                                 br(),br(),
                                 shinycssloaders::withSpinner(plotlyOutput("resrapida_plot_pie"),color = "#1c9ad6", type = "8", size = 0.5)
                               ),
                               
                               tabPanel(
                                 title = lang_label("button_datatable"),icon = icon("table"),
                                 br(),
                                 shinycssloaders::withSpinner(dataTableOutput("resrapida_table_dist"),color = "#1c9ad6", type = "8", size = 0.5)
                               )
                        )
                    )
                  ),
                  fluidRow(
                    box(width = 12,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        title = textOutput("resrapida_title_data_box"),
                        column(width = 12,shinycssloaders::withSpinner(dataTableOutput("resrapida_table"),color = "#1c9ad6", type = "8", size = 0.5))
                    )
                  ),
                  fluidRow(
                    column(width = 3),
                    column(width = 6,
                           box(width = 12,
                               solidHeader = TRUE,collapsible = TRUE,title = lang_label("general_title_limits_table"),
                               shinycssloaders::withSpinner(dataTableOutput("resrap_rangos_table"),color = "#1c9ad6", type = "8", size = 0.3)
                           )
                    ),
                    column(width = 3)
                  )
          )
        )
      )
    )
    )
)

# SERVER ----
server <- function(input, output, session) {
  
  # DOWNLOAD Report ----
  output$download_report_word <- downloadHandler(
    filename = paste0(lang_label("report_filename")," ",toupper(COUNTRY_NAME),".docx"),
    content = function(file) {
      file.copy("www/SR_report.docx", file)
    }
  )
  
  output$download_report_html <- downloadHandler(
    filename = paste0(lang_label("report_filename")," ",toupper(COUNTRY_NAME),".html"),
    content = function(file) {
      file.copy("www/SR_report.html", file)
    }
  )
  
  # SERVER GENERAL IND ----
  ind_rename <- function(selected_ind) {
    return(
      case_when(
        lang_label("menuitem_general_label") == selected_ind ~ "GENERAL",
        lang_label("menuitem_inm_pob") == selected_ind ~ "INM_POB",
        lang_label("menuitem_surv_qual") == selected_ind ~ "SURV_QUAL",
        lang_label("menuitem_prog_del") == selected_ind ~ "PROG_DEL",
        lang_label("menuitem_thre_asse") == selected_ind ~ "THRE_ASSE",
        lang_label("menuitem_rap_res") == selected_ind ~ "RAP_RES"
      )
    )
  }
  
  risk_rename <- function(selected_risk) {
    return(
      case_when(
        toupper(lang_label("rep_label_all")) == selected_risk ~ "ALL",
        lang_label("cut_offs_VHR") == selected_risk ~ "VHR",
        lang_label("cut_offs_HR") == selected_risk ~ "HR",
        lang_label("cut_offs_MR") == selected_risk ~ "MR",
        lang_label("cut_offs_LR") == selected_risk ~ "LR"
      )
    )
  }
  
  box_data <- reactiveValues()
  box_data$a1 <- 0
  box_data$a2 <- 0
  box_data$a3 <- 0
  box_data$a4 <- 0
  box_data$at <- 0
  
  observeEvent(input$indicadores_select_indicador, {
    new_box_data <- datos_boxes(LANG_TLS,indicadores_prep_box_data())
    box_data$a1 <- new_box_data[1]
    box_data$a2 <- new_box_data[2]
    box_data$a3 <- new_box_data[3]
    box_data$a4 <- new_box_data[4]
    box_data$at <- new_box_data[5]
  })
  
  observeEvent(input$indicadores_select_admin1, {
    new_box_data <- datos_boxes(LANG_TLS,indicadores_prep_box_data())
    box_data$a1 <- new_box_data[1]
    box_data$a2 <- new_box_data[2]
    box_data$a3 <- new_box_data[3]
    box_data$a4 <- new_box_data[4]
    box_data$at <- new_box_data[5]
  })
  
  box_lugar <- function(admin1) {
    if (admin1 == toupper(lang_label("rep_label_all"))) {
      return(toupper(COUNTRY_NAME))
    } else {
      return(toupper(admin1))
    }
  }
  
  
  output$ind_box_1 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a1,box_data$at,"LR"),"font-size: 90%;"),
      VB_style(paste(lang_label("box_LR_admin2"),box_lugar(input$indicadores_select_admin1)),"font-size: 95%;"),
      icon = icon('ok-sign', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$ind_box_2 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a2,box_data$at,"MR"),"font-size: 85%;"),
      VB_style(paste(lang_label("box_MR_admin2"),box_lugar(input$indicadores_select_admin1)),"font-size: 95%;"),
      icon = icon('minus-sign', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$ind_box_3 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a3,box_data$at,"HR"),"font-size: 85%;"),
      VB_style(paste(lang_label("box_HR_admin2"),box_lugar(input$indicadores_select_admin1)),"font-size: 95%;"),
      icon = icon('exclamation-sign', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$ind_box_4 <- renderValueBox({
    valueBox(
      VB_style(get_box_text(box_data$a4,box_data$at,"VHR"),"font-size: 85%;"),
      VB_style(paste(lang_label("box_VHR_admin2"),box_lugar(input$indicadores_select_admin1)),"font-size: 95%;"),
      icon = icon('alert', lib = 'glyphicon'),
      color = "purple"
    )
  })
  
  output$indicadores_title <- renderText({
    input$indicadores_select_indicador
  })
  
  output$indicadores_title_bar_box <- renderText({
    text_title <- title_bar_box(input$indicadores_select_indicador,input$indicadores_select_admin1)
    text_title <- paste0(text_title," (",YEAR_1," - ",YEAR_5,")")
    text_title
  })
  
  output$indicadores_title_map_box <- renderText({
    text_title <- title_map_box(input$indicadores_select_indicador,input$indicadores_select_admin1)
    text_title <- paste0(text_title," (",YEAR_1," - ",YEAR_5,")")
    text_title
  })
  
  output$indicadores_title_map_box_2 <- renderText({
    text_title <- title_map_box(input$indicadores_select_indicador,input$indicadores_select_admin1)
    text_title <- paste0(text_title," (",YEAR_1," - ",YEAR_5,")")
    text_title
  })
  
  indicadores_prep_bar_data <- reactive({
    ind_prep_bar_data(LANG_TLS,CUT_OFFS,indicadores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
  })
  
  indicadores_prep_map_data <- reactive({
    ind_prep_map_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,country_shapes,indicadores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
  })
  
  indicadores_prep_box_data <- reactive({
    ind_prep_box_data(LANG_TLS,CUT_OFFS,indicadores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1))
  })
  
  output$indicadores_plot_bar <- renderPlotly({
    ind_plot_bar_data(LANG_TLS,CUT_OFFS,indicadores_prep_bar_data(),ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1))
  })
  
  output$indicadores_plot_multibar <- renderPlotly({
    ind_plot_multibar_data(LANG_TLS,CUT_OFFS,indicadores_data,get_a1_geo_id(input$indicadores_select_admin1),ind_rename(input$indicadores_select_indicador),risk_rename(input$indicadores_select_risk))
  })
  
  output$indicadores_table <- renderDataTable(server = FALSE,{
    ind_get_bar_table(LANG_TLS,CUT_OFFS,indicadores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
  })
  
  output$indicadores_table_2 <- renderDataTable(server = FALSE,{
    ind_get_bar_table(LANG_TLS,CUT_OFFS,indicadores_data,ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
  })
  
  ind_map <- reactiveValues(dat = 0)
  output$dl_indicadores_plot_map <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",toupper(COUNTRY_NAME)," ",input$indicadores_select_indicador," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(ind_map$dat, file = file)
    }
  )
  
  output$indicadores_plot_map <- renderLeaflet({
    ind_map$dat <- ind_plot_map_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,indicadores_prep_map_data(),ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
    ind_map$dat
  })
  
  ind_map_2 <- reactiveValues(dat = 0)
  output$dl_indicadores_plot_map_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$indicadores_select_admin1," ",toupper(COUNTRY_NAME)," ",input$indicadores_select_indicador," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(ind_map_2$dat, file = file)
    }
  )
  
  output$indicadores_plot_map_2 <- renderLeaflet({
    ind_map_2$dat <- ind_plot_map_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,indicadores_prep_map_data(),ind_rename(input$indicadores_select_indicador),get_a1_geo_id(input$indicadores_select_admin1),risk_rename(input$indicadores_select_risk))
    ind_map_2$dat
  })
  
  output$indicadores_rangos_table <- renderDataTable(server = FALSE,{
    ind_rangos_table(LANG_TLS,CUT_OFFS,ind_rename(input$indicadores_select_indicador))
  })
  
  # Rangos tables
  output$inmu_rangos_table <- renderDataTable(server = FALSE,{
    ind_rangos_table(LANG_TLS,CUT_OFFS,"INM_POB")
  })
  output$cal_rangos_table <- renderDataTable(server = FALSE,{
    ind_rangos_table(LANG_TLS,CUT_OFFS,"SURV_QUAL")
  })
  output$rend_rangos_table <- renderDataTable(server = FALSE,{
    ind_rangos_table(LANG_TLS,CUT_OFFS,"PROG_DEL")
  })
  output$eval_rangos_table <- renderDataTable(server = FALSE,{
    ind_rangos_table(LANG_TLS,CUT_OFFS,"THRE_ASSE")
  })
  output$resrap_rangos_table <- renderDataTable(server = FALSE,{
    ind_rangos_table(LANG_TLS,CUT_OFFS,"RAP_RES")
  })
  
  # SERVER INM_POB ----
  output$inmunidad_title_data_box <- renderText({
    title_data_box(lang_label("INM_POB"),input$inmunidad_select_admin1)
  })
  
  output$inmunidad_title_pie_box <- renderText({
    title_pie_box(lang_label("INM_POB"),input$inmunidad_select_admin1)
  })
  
  output$inmunidad_title_map_box <- renderText({
    title_map_box(lang_label("INM_POB"),input$inmunidad_select_admin1)
  })
  
  output$inmunidad_table <- renderDataTable(server = FALSE,{
    inmu_get_data_table(LANG_TLS,YEAR_LIST,CUT_OFFS,inmunidad_data,get_a1_geo_id(input$inmunidad_select_admin1))
  })
  
  output$inmunidad_table_dist <- renderDataTable(server = FALSE,{
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"INM_POB",inmunidad_data,get_a1_geo_id(input$inmunidad_select_admin1),return_table=T)
  })
  
  output$inmunidad_plot_pie <- renderPlotly({
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"INM_POB",inmunidad_data,get_a1_geo_id(input$inmunidad_select_admin1),return_table=F)
  })
  
  inmu_map_total <- reactiveValues(dat = 0)
  output$dl_inmunidad_map_total <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$inmunidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("INM_POB")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(inmu_map_total$dat, file = file)
    }
  )
  
  output$inmunidad_map_total <- renderLeaflet({
    inmu_map_total$dat <- inmu_plot_map_data(LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,inmunidad_data,"TOTAL_PR",input$inmunidad_select_admin1,get_a1_geo_id(input$inmunidad_select_admin1),admin1_geo_id_df)
    inmu_map_total$dat
  })
  
  inmu_map_cob_1 <- reactiveValues(dat = 0)
  output$dl_inmunidad_map_cob_1 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$inmunidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("inm_mmr1_cob")," (",input$radio_inmunidad_cob_1,").png")
    },
    content = function(file) {
      mapshot(inmu_map_cob_1$dat, file = file)
    }
  )
  
  output$inmunidad_map_cob_1 <- renderLeaflet({
    var_srp1 <- case_when(
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_1) ~ "SRP1_year1",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_2) ~ "SRP1_year2",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_3) ~ "SRP1_year3",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_4) ~ "SRP1_year4",
      input$radio_inmunidad_cob_1 == paste(lang_label("vac_coverage"),YEAR_5) ~ "SRP1_year5",
      input$radio_inmunidad_cob_1 == lang_label("risk_points") ~ "SRP1_PR"
    )
    inmu_map_cob_1$dat <- inmu_plot_map_data(LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,inmunidad_data,var_srp1,input$inmunidad_select_admin1,get_a1_geo_id(input$inmunidad_select_admin1),admin1_geo_id_df)
    inmu_map_cob_1$dat
  })
  
  inmu_map_cob_2 <- reactiveValues(dat = 0)
  output$dl_inmunidad_map_cob_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$inmunidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("inm_mmr2_cob")," (",input$radio_inmunidad_cob_2,").png")
    },
    content = function(file) {
      mapshot(inmu_map_cob_2$dat, file = file)
    }
  )
  
  output$inmunidad_map_cob_2 <- renderLeaflet({
    var_srp2 <- case_when(
      input$radio_inmunidad_cob_2 == paste(lang_label("vac_coverage"),YEAR_1) ~ "SRP2_year1",
      input$radio_inmunidad_cob_2 == paste(lang_label("vac_coverage"),YEAR_2) ~ "SRP2_year2",
      input$radio_inmunidad_cob_2 == paste(lang_label("vac_coverage"),YEAR_3) ~ "SRP2_year3",
      input$radio_inmunidad_cob_2 == paste(lang_label("vac_coverage"),YEAR_4) ~ "SRP2_year4",
      input$radio_inmunidad_cob_2 == paste(lang_label("vac_coverage"),YEAR_5) ~ "SRP2_year5",
      input$radio_inmunidad_cob_2 == lang_label("risk_points") ~ "SRP2_PR"
    )
    inmu_map_cob_2$dat <- inmu_plot_map_data(LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,inmunidad_data,var_srp2,input$inmunidad_select_admin1,get_a1_geo_id(input$inmunidad_select_admin1),admin1_geo_id_df)
    inmu_map_cob_2$dat
  })
  
  inmu_map_camp <- reactiveValues(dat = 0)
  output$dl_inmunidad_map_camp <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$inmunidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("inm_title_map_last_camp")," (",YEAR_CAMP_SR,").png")
    },
    content = function(file) {
      mapshot(inmu_map_camp$dat, file = file)
    }
  )
  
  output$inmunidad_map_camp <- renderLeaflet({
    inmu_map_camp$dat <- inmu_plot_map_data(LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,inmunidad_data,"cob_last_camp",input$inmunidad_select_admin1,get_a1_geo_id(input$inmunidad_select_admin1),admin1_geo_id_df)
    inmu_map_camp$dat
  })
  
  inmu_map_casos <- reactiveValues(dat = 0)
  output$dl_inmunidad_map_casos <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$inmunidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("inm_title_map_novac")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(inmu_map_casos$dat, file = file)
    }
  )
  
  output$inmunidad_map_casos <- renderLeaflet({
    inmu_map_casos$dat <- inmu_plot_map_data(LANG_TLS,YEAR_CAMP_SR,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,inmunidad_data,"p_sospechosos_novac",input$inmunidad_select_admin1,get_a1_geo_id(input$inmunidad_select_admin1),admin1_geo_id_df)
    inmu_map_casos$dat
  })
  
  
  # SERVER SURV_QUAL ----
  output$calidad_title_data_box <- renderText({
    title_data_box(lang_label("SURV_QUAL"),input$calidad_select_admin1)
  })
  
  output$calidad_title_map_box <- renderText({
    title_map_box(lang_label("SURV_QUAL"),input$calidad_select_admin1)
  })
  
  output$calidad_title_pie_box <- renderText({
    title_pie_box(lang_label("SURV_QUAL"),input$calidad_select_admin1)
  })
  
  output$calidad_table <- renderDataTable(server = FALSE,{
    cal_get_data_table(LANG_TLS,CUT_OFFS,calidad_data,get_a1_geo_id(input$calidad_select_admin1))
  })
  
  output$calidad_table_dist <- renderDataTable(server = FALSE,{
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"SURV_QUAL",calidad_data,get_a1_geo_id(input$calidad_select_admin1),return_table=T)
  })
  
  output$calidad_plot_pie <- renderPlotly({
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"SURV_QUAL",calidad_data,get_a1_geo_id(input$calidad_select_admin1),return_table=F)
  })
  
  cal_map_total <- reactiveValues(dat = 0)
  output$dl_calidad_map_total <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$calidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("SURV_QUAL")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_total$dat, file = file)
    }
  )
  
  output$calidad_map_total <- renderLeaflet({
    cal_map_total$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,calidad_data,"TOTAL_PR",input$calidad_select_admin1,get_a1_geo_id(input$calidad_select_admin1),admin1_geo_id_df)
    cal_map_total$dat
  })
  
  cal_map_1 <- reactiveValues(dat = 0)
  output$dl_calidad_map_1 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$calidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("surv_rate_novac")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_1$dat, file = file)
    }
  )
  
  output$calidad_map_1 <- renderLeaflet({
    cal_map_1$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,calidad_data,"tasa_casos",input$calidad_select_admin1,get_a1_geo_id(input$calidad_select_admin1),admin1_geo_id_df)
    cal_map_1$dat
  })
  
  cal_map_2 <- reactiveValues(dat = 0)
  output$dl_calidad_map_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$calidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("surv_adeq_inv")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_2$dat, file = file)
    }
  )
  
  output$calidad_map_2 <- renderLeaflet({
    cal_map_2$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,calidad_data,"p_casos_inv",input$calidad_select_admin1,get_a1_geo_id(input$calidad_select_admin1),admin1_geo_id_df)
    cal_map_2$dat
  })
  
  cal_map_3 <- reactiveValues(dat = 0)
  output$dl_calidad_map_3 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$calidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("surv_adeq_sample")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(cal_map_3$dat, file = file)
    }
  )
  
  output$calidad_map_3 <- renderLeaflet({
    cal_map_3$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,calidad_data,"p_casos_muestra",input$calidad_select_admin1,get_a1_geo_id(input$calidad_select_admin1),admin1_geo_id_df)
    cal_map_3$dat
  })

  calidad_map_4 <- reactiveValues(dat = 0)
  output$dl_calidad_map_4 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$calidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("surv_timely_lab")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(calidad_map_4$dat, file = file)
    }
  )
  
  output$calidad_map_4 <- renderLeaflet({
    calidad_map_4$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,calidad_data,"p_muestras_lab",input$calidad_select_admin1,get_a1_geo_id(input$calidad_select_admin1),admin1_geo_id_df)
    calidad_map_4$dat
  })
  
  ## Silent Municipalities Server ####
  ### Silent muni map downloader ####
  calidad_map_5 <- reactiveValues(dat = 0)
  output$dl_calidad_map_5 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$calidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("silent_mun_lab")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(calidad_map_5$dat, file = file)
    }
  )
  ### Silent muni map output ####
  # Output for cal_plot_map_data with silent municipalities
  output$calidad_map_5 <- renderLeaflet({
    calidad_map_5$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,calidad_data,"silent_mun",input$calidad_select_admin1,get_a1_geo_id(input$calidad_select_admin1),admin1_geo_id_df)
    calidad_map_5$dat
  })
  
  ### Silent muni valuebox ----
  # Value box that renders the silent municipalities surveillance quality
  # indicators
  output$ind_box_silent_mun <- renderValueBox({
    surv_box_data <- cal_surv_data_vbox(LANG_TLS,toupper(COUNTRY_NAME),calidad_data,input$calidad_select_admin1, get_a1_geo_id(input$calidad_select_admin1))
    valueBox(
      VB_style(surv_box_data[[2]],"font-size: 85%;"),
      VB_style(surv_box_data[[1]],"font-size: 100%;"),
      icon = icon("bell-slash"),
      color = "purple"
    )
  })
  
  
  # SERVER PROG_DEL ----
  output$rendimiento_title_data_box <- renderText({
    title_data_box(lang_label("PROG_DEL"),input$rendimiento_select_admin1)
  })
  
  output$rendimiento_title_map_box <- renderText({
    title_map_box(lang_label("PROG_DEL"),input$rendimiento_select_admin1)
  })
  
  output$rendimiento_title_pie_box <- renderText({
    title_pie_box(lang_label("PROG_DEL"),input$rendimiento_select_admin1)
  })
  
  output$rendimiento_table <- renderDataTable(server = FALSE,{
    rend_get_data_table(LANG_TLS,CUT_OFFS,rendimiento_data,get_a1_geo_id(input$rendimiento_select_admin1))
  })
  
  output$rendimiento_table_dist <- renderDataTable(server = FALSE,{
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"PROG_DEL",rendimiento_data,get_a1_geo_id(input$rendimiento_select_admin1),return_table=T)
  })
  
  output$rendimiento_plot_pie <- renderPlotly({
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"PROG_DEL",rendimiento_data,get_a1_geo_id(input$rendimiento_select_admin1),return_table=F)
  })
  
  rendimiento_map_total <- reactiveValues(dat = 0)
  output$dl_rendimiento_map_total <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$rendimiento_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("PROG_DEL")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(rendimiento_map_total$dat, file = file)
    }
  )
  
  output$rendimiento_map_total <- renderLeaflet({
    rendimiento_map_total$dat <- rend_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,rendimiento_data,"TOTAL_PR",input$rendimiento_select_admin1,get_a1_geo_id(input$rendimiento_select_admin1),admin1_geo_id_df)
    rendimiento_map_total$dat
  })
  
  rendimiento_map_1 <- reactiveValues(dat = 0)
  output$dl_rendimiento_map_1 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$rendimiento_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("prog_cob_trend")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(rendimiento_map_1$dat, file = file)
    }
  )
  
  output$rendimiento_map_1 <- renderLeaflet({
    var_trend <- case_when(
      input$radio_rendimiento_map_1 == lang_label("mmr1") ~ "tendencia_SRP1",
      input$radio_rendimiento_map_1 == lang_label("mmr2") ~ "tendencia_SRP2"
    )
    rendimiento_map_1$dat <- rend_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,rendimiento_data,var_trend,input$rendimiento_select_admin1,get_a1_geo_id(input$rendimiento_select_admin1),admin1_geo_id_df)
    rendimiento_map_1$dat
  })
  
  rendimiento_map_2 <- reactiveValues(dat = 0)
  output$dl_rendimiento_map_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$rendimiento_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("prog_dropout_rate")," (",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(rendimiento_map_2$dat, file = file)
    }
  )
  
  output$rendimiento_map_2 <- renderLeaflet({
    var_dropout <- case_when(
      input$radio_rendimiento_map_2 == lang_label("prog_mmr1_mmr2") ~ "tasa_des_srp1_srp2",
      input$radio_rendimiento_map_2 == lang_label("prog_penta1_mmr1") ~ "tasa_des_penta1_srp1"
    )
    rendimiento_map_2$dat <- rend_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,rendimiento_data,var_dropout,input$rendimiento_select_admin1,get_a1_geo_id(input$rendimiento_select_admin1),admin1_geo_id_df)
    rendimiento_map_2$dat
  })
  
  
  # SERVER THRE_ASSE ----
  output$amenaza_title_data_box <- renderText({
    title_data_box(lang_label("THRE_ASSE"),input$amenaza_select_admin1)
  })
  
  output$amenaza_title_map_box <- renderText({
    title_map_box(lang_label("THRE_ASSE"),input$amenaza_select_admin1)
  })
  
  output$amenaza_title_pie_box <- renderText({
    title_pie_box(lang_label("THRE_ASSE"),input$amenaza_select_admin1)
  })
  
  output$amenaza_table <- renderDataTable(server = FALSE,{
    amenaza_get_data_table(LANG_TLS,CUT_OFFS,eval_amenaza_data,get_a1_geo_id(input$amenaza_select_admin1))
  })
  
  output$amenaza_table_dist <- renderDataTable(server = FALSE,{
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"THRE_ASSE",eval_amenaza_data,get_a1_geo_id(input$amenaza_select_admin1),return_table=T)
  })
  
  output$amenaza_plot_pie <- renderPlotly({
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"THRE_ASSE",eval_amenaza_data,get_a1_geo_id(input$amenaza_select_admin1),return_table=F)
  })
  
  amenaza_map_total <- reactiveValues(dat = 0)
  output$dl_amenaza_map_total <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$amenaza_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("THRE_ASSE")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(amenaza_map_total$dat, file = file)
    }
  )
  
  output$amenaza_map_total <- renderLeaflet({
    amenaza_map_total$dat <- amenaza_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,eval_amenaza_data,"TOTAL_PR",input$amenaza_select_admin1,get_a1_geo_id(input$amenaza_select_admin1),admin1_geo_id_df)
    amenaza_map_total$dat
  })
  
  amenaza_map_1 <- reactiveValues(dat = 0)
  output$dl_amenaza_map_1 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$amenaza_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("thre_pop_dens")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(amenaza_map_1$dat, file = file)
    }
  )
  
  output$amenaza_map_1 <- renderLeaflet({
    amenaza_map_1$dat <- amenaza_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,eval_amenaza_data,"dens_pob_PR",input$amenaza_select_admin1,get_a1_geo_id(input$amenaza_select_admin1),admin1_geo_id_df)
    amenaza_map_1$dat
  })
  
  output$vulnerables_pres_subtitle <- renderText({
    var_vul <- case_when(
      input$radio_amenaza_map_2 == lang_label("thre_risk_level") ~ "TOTAL_PR",
      input$radio_amenaza_map_2 == lang_label("thre_pres_inter_pob") ~ "pres_intercambio_pob",
      input$radio_amenaza_map_2 == lang_label("thre_pres_turism") ~ "pres_turismo",
      input$radio_amenaza_map_2 == lang_label("thre_pres_prob") ~ "pres_problemas",
      input$radio_amenaza_map_2 == lang_label("thre_pres_calam") ~ "pres_calamidades",
      input$radio_amenaza_map_2 == lang_label("thre_dif_topo") ~ "dif_topo_transporte",
      input$radio_amenaza_map_2 == lang_label("thre_pres_com") ~ "pres_comunidades",
      input$radio_amenaza_map_2 == lang_label("thre_pres_trafic") ~ "pres_trafico",
      input$radio_amenaza_map_2 == lang_label("thre_pres_events") ~ "pres_eventos"
    )
    vul_pres_subtitle(LANG_TLS,var_vul)
  })
  
  amenaza_map_2 <- reactiveValues(dat = 0)
  output$dl_amenaza_map_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$amenaza_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("thre_vul")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(amenaza_map_2$dat, file = file)
    }
  )
  
  output$amenaza_map_2 <- renderLeaflet({
    var_vul <- case_when(
      input$radio_amenaza_map_2 == lang_label("thre_risk_level") ~ "TOTAL_PR",
      input$radio_amenaza_map_2 == lang_label("thre_pres_inter_pob") ~ "pres_intercambio_pob",
      input$radio_amenaza_map_2 == lang_label("thre_pres_turism") ~ "pres_turismo",
      input$radio_amenaza_map_2 == lang_label("thre_pres_prob") ~ "pres_problemas",
      input$radio_amenaza_map_2 == lang_label("thre_pres_calam") ~ "pres_calamidades",
      input$radio_amenaza_map_2 == lang_label("thre_dif_topo") ~ "dif_topo_transporte",
      input$radio_amenaza_map_2 == lang_label("thre_pres_com") ~ "pres_comunidades",
      input$radio_amenaza_map_2 == lang_label("thre_pres_trafic") ~ "pres_trafico",
      input$radio_amenaza_map_2 == lang_label("thre_pres_events") ~ "pres_eventos"
    )
    amenaza_map_2$dat <- vul_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,vulnerables_data,var_vul,input$amenaza_select_admin1,get_a1_geo_id(input$amenaza_select_admin1),admin1_geo_id_df)
    amenaza_map_2$dat
  })
  
  
  # SERVER RAP_RES ----
  output$resrapida_title_data_box <- renderText({
    title_data_box(lang_label("RAP_RES"),input$resrapida_select_admin1)
  })
  
  output$resrapida_title_map_box <- renderText({
    title_map_box(lang_label("RAP_RES"),input$resrapida_select_admin1)
  })
  
  output$resrapida_title_pie_box <- renderText({
    title_pie_box(lang_label("RAP_RES"),input$resrapida_select_admin1)
  })
  
  output$resrapida_table <- renderDataTable(server = FALSE,{
    resrapida_get_data_table(LANG_TLS,CUT_OFFS,respuesta_rapida_data,get_a1_geo_id(input$resrapida_select_admin1))
  })
  
  output$resrapida_table_dist <- renderDataTable(server = FALSE,{
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"RAP_RES",respuesta_rapida_data,get_a1_geo_id(input$resrapida_select_admin1),return_table=T)
  })
  
  output$resrapida_plot_pie <- renderPlotly({
    plot_pie_data(LANG_TLS,ZERO_POB_LIST,CUT_OFFS,"RAP_RES",respuesta_rapida_data,get_a1_geo_id(input$resrapida_select_admin1),return_table=F)
  })
  
  resrapida_map_total <- reactiveValues(dat = 0)
  output$dl_resrapida_map_total <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$resrapida_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("RAP_RES")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(resrapida_map_total$dat, file = file)
    }
  )
  
  output$resrapida_map_total <- renderLeaflet({
    resrapida_map_total$dat <- resrapida_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,respuesta_rapida_data,"TOTAL_PR",input$resrapida_select_admin1,get_a1_geo_id(input$resrapida_select_admin1),admin1_geo_id_df)
    resrapida_map_total$dat
  })
  
  resrapida_map_1 <- reactiveValues(dat = 0)
  output$dl_resrapida_map_1 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$resrapida_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("rap_pres_team")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(resrapida_map_1$dat, file = file)
    }
  )
  
  output$resrapida_map_1 <- renderLeaflet({
    resrapida_map_1$dat <- resrapida_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,respuesta_rapida_data,"equipo",input$resrapida_select_admin1,get_a1_geo_id(input$resrapida_select_admin1),admin1_geo_id_df)
    resrapida_map_1$dat
  })
  
  resrapida_map_2 <- reactiveValues(dat = 0)
  output$dl_resrapida_map_2 <- downloadHandler(
    filename = function() {
      paste0(lang_label("map")," ",input$resrapida_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("rap_pres_hospital")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(resrapida_map_2$dat, file = file)
    }
  )
  
  output$resrapida_map_2 <- renderLeaflet({
    resrapida_map_2$dat <- resrapida_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,respuesta_rapida_data,"hospitales_p",input$resrapida_select_admin1,get_a1_geo_id(input$resrapida_select_admin1),admin1_geo_id_df)
    resrapida_map_2$dat
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
