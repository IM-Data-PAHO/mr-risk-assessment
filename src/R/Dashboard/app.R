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

# JOINT RISK DATA ----
normalize_admin_value <- function(value) {
  normalized <- iconv(value, to = "ASCII//TRANSLIT")
  normalized <- stringr::str_trim(toupper(normalized))
  return(normalized)
}

pull_first_available_column <- function(df, candidates, default = NA_character_) {
  available <- candidates[candidates %in% names(df)]
  if (length(available) == 0) {
    if (is.numeric(default) || is.integer(default)) {
      return(rep(as.numeric(default), nrow(df)))
    }
    if (is.logical(default)) {
      return(rep(default, nrow(df)))
    }
    return(rep(as.character(default), nrow(df)))
  }
  df[[available[1]]]
}

joint_admin1_column_candidates <- c(
  "subnational_level",
  "nivel_subnacional",
  "niveau_infranational",
  "province",
  "provincia",
  "departamento",
  "departement",
  "estado",
  "state",
  "region",
  "regiao",
  "admin1",
  "admin_1"
)

joint_admin2_column_candidates <- c(
  "municipality",
  "municipio",
  "municipalidad",
  "municipalite",
  "districts",
  "district",
  "distrito",
  "departamento",
  "commune",
  "comuna",
  "county",
  "city",
  "ciudad",
  "ville",
  "localidad",
  "localite",
  "prefecture",
  "admin2",
  "admin_2"
)

joint_risk_points_candidates <- c(
  "total_risk_points",
  "risk_points",
  "risk_score",
  "total_score",
  "overall_score",
  "puntos_totales_de_riesgo",
  "puntos_de_riesgo",
  "puntaje_total_de_riesgo",
  "pontos_totais_de_risco",
  "pontos_de_risco",
  "pontuacao_total_de_risco",
  "points_de_risque_totaux",
  "total_des_points_de_risque"
)

joint_risk_level_candidates <- c(
  "risk_level",
  "risk_category",
  "risk_classification",
  "categoria_de_riesgo",
  "classification_du_risque",
  "categorie_de_risque",
  "nivel_de_riesgo",
  "nivel_de_risco",
  "niveau_de_risque"
)

joint_population_immunity_candidates <- c(
  "population_immunity",
  "population_immunity_score",
  "immunity",
  "immunity_score",
  "inmunidad_de_la_poblacion",
  "imunidade_da_populacao",
  "immunite_de_la_population"
)

joint_surveillance_quality_candidates <- c(
  "surveillance_quality",
  "quality_of_surveillance",
  "surveillance",
  "surveillance_score",
  "calidad_de_la_vigilancia",
  "qualidade_da_vigilancia",
  "qualite_de_la_surveillance"
)

joint_program_delivery_candidates <- c(
  "program_delivery_performance",
  "program_delivery",
  "program_delivery_score",
  "desempeno_del_programa",
  "desempeno_programa",
  "desempenho_do_programa",
  "performance_du_programme",
  "performance_du_program"
)

joint_threat_assessment_candidates <- c(
  "threat_assessment",
  "threats",
  "threat_score",
  "threats_score",
  "risk_of_outbreak",
  "evaluacion_de_la_amenaza",
  "evaluation_de_la_menace",
  "avaliacao_da_ameaca"
)

joint_rapid_response_candidates <- c(
  "rapid_response",
  "respuesta_rapida",
  "resposta_rapida",
  "reponse_rapide"
)

standardize_joint_label <- function(x) {
  lx <- iconv(x, to = "ASCII//TRANSLIT")
  lx <- stringr::str_replace_all(lx, "[-_]", " ")
  lx <- stringr::str_to_lower(lx)
  lx <- stringr::str_replace_all(lx, "\\s+", " ")
  lx <- stringr::str_trim(lx)
  dplyr::case_when(
    is.na(lx) ~ NA_character_,
    lx %in% c("low", "bajo", "baja", "baixo", "baixa", "faible", "bas") ~ "Low",
    lx %in% c("medium", "mediano", "medio", "media", "moyen", "moyenne", "moderate", "moderado", "moderada", "modere", "moderee") ~ "Medium",
    lx %in% c("high", "alto", "alta", "haut", "haute", "eleve", "elevee", "elevado", "elevada") ~ "High",
    lx %in% c("very high", "veryhigh", "muy alto", "muy alta", "muyalto", "muyalta", "muito alto", "muito alta", "muitoalto", "muitoalta", "muito elevado", "muito elevada", "tres haut", "tres haute", "tres eleve", "tres elevee", "treseleve", "treselevee") ~ "Very high",
    TRUE ~ NA_character_
  )
}

derive_polio_risk_level <- function(score) {
  dplyr::case_when(
    is.na(score) ~ NA_character_,
    score <= 25 ~ "Low",
    score <= 50 ~ "Medium",
    score <= 75 ~ "High",
    TRUE ~ "Very high"
  )
}

joint_risk_levels <- c("Low","Medium","High","Very high")
joint_palette <- c(
  "Low" = "#92d050",
  "Medium" = "#fec000",
  "High" = "#e8132b",
  "Very high" = "#920000"
)

joint_risk_matrix <- tribble(
  ~measles_level, ~polio_level, ~joint_level,
  "Low", "Low", "Low",
  "Low", "Medium", "Medium",
  "Low", "High", "Medium",
  "Low", "Very high", "High",
  "Medium", "Low", "Medium",
  "Medium", "Medium", "Medium",
  "Medium", "High", "High",
  "Medium", "Very high", "High",
  "High", "Low", "Medium",
  "High", "Medium", "High",
  "High", "High", "High",
  "High", "Very high", "Very high",
  "Very high", "Low", "High",
  "Very high", "Medium", "High",
  "Very high", "High", "Very high",
  "Very high", "Very high", "Very high"
)

read_joint_component_sheet <- function(file_path, sheet_name, disease_prefix, component_key) {
  raw <- tryCatch(read_excel(file_path, sheet = sheet_name), error = function(e) NULL)
  if (is.null(raw)) {
    return(tibble())
  }
  raw <- clean_names(raw)
  admin1_label <- pull_first_available_column(raw, joint_admin1_column_candidates, default = NA_character_)
  admin2_label <- pull_first_available_column(raw, joint_admin2_column_candidates, default = NA_character_)
  admin1_key <- normalize_admin_value(admin1_label)
  admin2_key <- normalize_admin_value(admin2_label)
  score_values <- suppressWarnings(as.numeric(pull_first_available_column(raw, joint_risk_points_candidates, default = NA_real_)))
  risk_raw <- pull_first_available_column(raw, joint_risk_level_candidates, default = NA_character_)
  risk_levels <- standardize_joint_label(risk_raw)
  score_col <- paste0(disease_prefix, "_", component_key, "_score")
  level_col <- paste0(disease_prefix, "_", component_key, "_risk_level")
  tibble(
    admin1_key = admin1_key,
    admin2_key = admin2_key,
    admin1_label = admin1_label,
    admin2_label = admin2_label
  ) %>%
    mutate(
      !!score_col := score_values,
      !!level_col := risk_levels
    ) %>%
    filter(
      !is.na(admin1_key),
      admin1_key != "",
      !is.na(admin2_key),
      admin2_key != ""
    )
}

build_joint_component_dataset <- function(measles_df, polio_df, component_key) {
  if (nrow(measles_df) == 0 || nrow(polio_df) == 0) {
    return(tibble())
  }
  joined <- inner_join(measles_df, polio_df, by = c("admin1_key","admin2_key"), suffix = c("_measles","_polio")) %>%
    mutate(
      admin1_label = coalesce(admin1_label_measles, admin1_label_polio),
      admin2_label = coalesce(admin2_label_measles, admin2_label_polio)
    ) %>%
    select(-admin1_label_measles, -admin1_label_polio, -admin2_label_measles, -admin2_label_polio)
  measles_level_col <- paste0("measles_", component_key, "_risk_level")
  polio_level_col <- paste0("polio_", component_key, "_risk_level")
  join_keys <- c(
    setNames("measles_level", measles_level_col),
    setNames("polio_level", polio_level_col)
  )
  joint_level_col <- paste0("joint_", component_key, "_risk_level")
  joint_rank_col <- paste0("joint_", component_key, "_rank")
  level_cols <- c(measles_level_col, polio_level_col)
  joined <- joined %>%
    left_join(joint_risk_matrix, by = join_keys) %>%
    mutate(
      across(all_of(level_cols), ~factor(., levels = joint_risk_levels))
    ) %>%
    mutate(
      !!joint_level_col := factor(coalesce(.data[["joint_level"]], .data[[measles_level_col]], .data[[polio_level_col]]), levels = joint_risk_levels),
      !!joint_rank_col := as.integer(.data[[joint_level_col]])
    ) %>%
    select(-joint_level)
  joined
}

country_shapes <- country_shapes %>%
  mutate(
    ADMIN1_KEY = normalize_admin_value(ADMIN1),
    ADMIN2_KEY = normalize_admin_value(ADMIN2)
  )

mmr_file_path <- "../../Data/mmr_results.xlsx"
polio_file_path <- "../../Data/polio_results.xlsx"
joint_data_available <- file.exists(mmr_file_path) && file.exists(polio_file_path)

if (joint_data_available) {
  mmr_joint_general_raw <- read_excel(mmr_file_path, sheet = "general_risk") %>%
    clean_names()
  
  mmr_admin1_values <- pull_first_available_column(
    mmr_joint_general_raw,
    joint_admin1_column_candidates,
    default = NA_character_
  )
  mmr_admin2_values <- pull_first_available_column(
    mmr_joint_general_raw,
    joint_admin2_column_candidates,
    default = NA_character_
  )
  mmr_immunity_values <- suppressWarnings(as.numeric(pull_first_available_column(
    mmr_joint_general_raw,
    joint_population_immunity_candidates,
    default = NA_real_
  )))
  mmr_surveillance_values <- suppressWarnings(as.numeric(pull_first_available_column(
    mmr_joint_general_raw,
    joint_surveillance_quality_candidates,
    default = NA_real_
  )))
  mmr_program_delivery_values <- suppressWarnings(as.numeric(pull_first_available_column(
    mmr_joint_general_raw,
    joint_program_delivery_candidates,
    default = NA_real_
  )))
  mmr_threat_values <- suppressWarnings(as.numeric(pull_first_available_column(
    mmr_joint_general_raw,
    joint_threat_assessment_candidates,
    default = NA_real_
  )))
  mmr_rapid_response_values <- suppressWarnings(as.numeric(pull_first_available_column(
    mmr_joint_general_raw,
    joint_rapid_response_candidates,
    default = NA_real_
  )))
  mmr_risk_points_values <- suppressWarnings(as.numeric(pull_first_available_column(
    mmr_joint_general_raw,
    joint_risk_points_candidates,
    default = NA_real_
  )))
  mmr_risk_raw <- pull_first_available_column(
    mmr_joint_general_raw,
    joint_risk_level_candidates,
    default = NA_character_
  )
  mmr_risk_levels <- standardize_joint_label(mmr_risk_raw)
  
  mmr_joint_general <- mmr_joint_general_raw %>%
    mutate(
      admin1_label = mmr_admin1_values,
      admin2_label = mmr_admin2_values,
      admin1_key = normalize_admin_value(admin1_label),
      admin2_key = normalize_admin_value(admin2_label),
      measles_population_immunity = mmr_immunity_values,
      measles_surveillance_quality = mmr_surveillance_values,
      measles_program_delivery_performance = mmr_program_delivery_values,
      measles_threat_assessment = mmr_threat_values,
      measles_rapid_response = mmr_rapid_response_values,
      measles_risk_points = mmr_risk_points_values,
      measles_risk_level = mmr_risk_levels
    ) %>%
    transmute(
      admin1_key,
      admin2_key,
      admin1_label,
      admin2_label,
      measles_population_immunity,
      measles_surveillance_quality,
      measles_program_delivery_performance,
      measles_threat_assessment,
      measles_rapid_response,
      measles_risk_points,
      measles_risk_level
    )
  
  polio_joint_general_raw <- tryCatch(
    read_excel(polio_file_path, sheet = "general_risk"),
    error = function(e) read_excel(polio_file_path, sheet = 1)
  ) %>%
    clean_names()
  
  polio_admin1_values <- pull_first_available_column(
    polio_joint_general_raw,
    joint_admin1_column_candidates,
    default = NA_character_
  )
  polio_admin2_values <- pull_first_available_column(
    polio_joint_general_raw,
    joint_admin2_column_candidates,
    default = NA_character_
  )
  polio_immunity_values <- pull_first_available_column(
    polio_joint_general_raw,
    joint_population_immunity_candidates,
    default = NA_real_
  )
  polio_surveillance_values <- pull_first_available_column(
    polio_joint_general_raw,
    joint_surveillance_quality_candidates,
    default = NA_real_
  )
  polio_determinants_values <- pull_first_available_column(
    polio_joint_general_raw,
    c(
      "program_delivery_performance",
      "health_determinants",
      "determinants_score",
      "determinants",
      "determinants_and_threats"
    ),
    default = NA_real_
  )
  polio_outbreaks_values <- pull_first_available_column(
    polio_joint_general_raw,
    c(joint_threat_assessment_candidates, "outbreaks_score", "outbreak_score", "outbreaks", "outbreak"),
    default = NA_real_
  )
  polio_total_values <- pull_first_available_column(
    polio_joint_general_raw,
    joint_risk_points_candidates,
    default = NA_real_
  )
  polio_risk_raw <- pull_first_available_column(
    polio_joint_general_raw,
    joint_risk_level_candidates,
    default = NA_character_
  )
  polio_risk_levels <- standardize_joint_label(polio_risk_raw)
  polio_risk_levels <- ifelse(
    is.na(polio_risk_levels),
    derive_polio_risk_level(polio_total_values),
    polio_risk_levels
  )
  
  polio_joint_general <- polio_joint_general_raw %>%
    mutate(
      admin1_label = polio_admin1_values,
      admin2_label = polio_admin2_values,
      admin1_key = normalize_admin_value(admin1_label),
      admin2_key = normalize_admin_value(admin2_label),
      polio_immunity_score = polio_immunity_values,
      polio_surveillance_score = polio_surveillance_values,
      polio_determinants_score = polio_determinants_values,
      polio_outbreaks_score = polio_outbreaks_values,
      polio_total_score = polio_total_values,
      polio_risk_level = polio_risk_levels
    ) %>%
    transmute(
      admin1_key,
      admin2_key,
      admin1_label,
      admin2_label,
      polio_immunity_score,
      polio_surveillance_score,
      polio_determinants_score,
      polio_outbreaks_score,
      polio_total_score,
      polio_risk_level
    )
  
  mmr_immunity_component <- read_joint_component_sheet(mmr_file_path, "population_immunity", "measles", "immunity")
  polio_immunity_component <- read_joint_component_sheet(polio_file_path, "population_immunity", "polio", "immunity")
  joint_immunity_data <- build_joint_component_dataset(mmr_immunity_component, polio_immunity_component, "immunity")
  
  mmr_surveillance_component <- read_joint_component_sheet(mmr_file_path, "quality_of_surveillance", "measles", "surveillance")
  polio_surveillance_component <- read_joint_component_sheet(polio_file_path, "quality_of_surveillance", "polio", "surveillance")
  joint_surveillance_data <- build_joint_component_dataset(mmr_surveillance_component, polio_surveillance_component, "surveillance")
  
  joint_base_data <- inner_join(mmr_joint_general, polio_joint_general, by = c("admin1_key","admin2_key"), suffix = c("_measles","_polio")) %>%
    mutate(
      admin1_label = coalesce(admin1_label_measles, admin1_label_polio),
      admin2_label = coalesce(admin2_label_measles, admin2_label_polio)
    ) %>%
    select(
      admin1_key,
      admin2_key,
      admin1_label,
      admin2_label,
      measles_population_immunity,
      measles_surveillance_quality,
      measles_program_delivery_performance,
      measles_threat_assessment,
      measles_rapid_response,
      measles_risk_points,
      measles_risk_level,
      polio_immunity_score,
      polio_surveillance_score,
      polio_determinants_score,
      polio_outbreaks_score,
      polio_total_score,
      polio_risk_level
    ) %>%
    left_join(joint_risk_matrix, by = c("measles_risk_level" = "measles_level","polio_risk_level" = "polio_level")) %>%
    mutate(
      measles_risk_level = factor(measles_risk_level, levels = joint_risk_levels),
      polio_risk_level = factor(polio_risk_level, levels = joint_risk_levels),
      joint_risk_level = factor(coalesce(joint_level, measles_risk_level, polio_risk_level), levels = joint_risk_levels),
      joint_rank = as.integer(joint_risk_level)
    ) %>%
    select(-joint_level)
  
  joint_admin_lookup <- joint_base_data %>%
    distinct(admin1_key, admin1_label) %>%
    arrange(admin1_label) %>%
    mutate(admin1_label = ifelse(is.na(admin1_label) | admin1_label == "", "Unknown", admin1_label))
  
  joint_admin_choices <- c("All subnational levels" = "ALL")
  if (nrow(joint_admin_lookup) > 0) {
    joint_admin_choices <- c(joint_admin_choices, setNames(joint_admin_lookup$admin1_key, joint_admin_lookup$admin1_label))
  }
  joint_admin_default <- if (length(joint_admin_choices) > 0) joint_admin_choices[[1]] else NULL

} else {
  joint_base_data <- tibble()
  joint_admin_lookup <- tibble()
  joint_admin_choices <- c()
  joint_admin_default <- NULL
  joint_immunity_data <- tibble()
  joint_surveillance_data <- tibble()
}

joint_component_data_lookup <- list()
joint_component_options <- character()
if (joint_data_available && nrow(joint_immunity_data) > 0) {
  joint_component_data_lookup$immunity <- joint_immunity_data
  joint_component_options <- c(joint_component_options, "immunity")
}
if (joint_data_available && nrow(joint_surveillance_data) > 0) {
  joint_component_data_lookup$surveillance <- joint_surveillance_data
  joint_component_options <- c(joint_component_options, "surveillance")
}
# FUNCS ----
get_a1_geo_id <- function(admin1) {
  return(admin1_geo_id_df$`ADMIN1 GEO_ID`[admin1_geo_id_df$ADMIN1 == admin1])
}

lang_label <- function(label) {
  return(LANG_TLS$LANG[LANG_TLS$LABEL == label])
}

joint_admin_all_label <- as.character(lang_label("rep_label_all"))
if (length(joint_admin_all_label) == 0) {
  joint_admin_all_label <- "ALL"
} else {
  joint_admin_all_label <- toupper(joint_admin_all_label[1])
  if (is.na(joint_admin_all_label) || joint_admin_all_label == "") {
    joint_admin_all_label <- "ALL"
  }
}
if (length(joint_admin_choices) > 0) {
  all_idx <- joint_admin_choices == "ALL"
  if (any(all_idx)) {
    names(joint_admin_choices)[all_idx] <- joint_admin_all_label
  }
}

joint_component_label_lookup <- c(
  immunity = lang_label("menuitem_inm_pob"),
  surveillance = lang_label("menuitem_surv_qual")
)
joint_component_choices <- c()
if ("immunity" %in% joint_component_options) {
  joint_component_choices <- c(joint_component_choices, setNames("immunity", joint_component_label_lookup[["immunity"]]))
}
if ("surveillance" %in% joint_component_options) {
  joint_component_choices <- c(joint_component_choices, setNames("surveillance", joint_component_label_lookup[["surveillance"]]))
}
joint_component_default <- if (length(joint_component_choices) > 0) joint_component_choices[[1]] else NULL
joint_component_section_available <- length(joint_component_choices) > 0

tabItems_safe <- function(...) {
  items <- list(...)
  items <- Filter(function(x) !is.null(x), items)
  do.call(shinydashboard::tabItems, items)
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
            ),
            if (joint_data_available) {
              tagList(
                menuItem(lang_label("menuitem_joint_risk"),
                         tabName = "joint_risk",
                         icon = icon("layer-group")
                ),
                conditionalPanel(
                  'input.sidebarid == "joint_risk"',
                  selectInput("joint_admin_filter", label = paste0(lang_label("rep_label_admin1_name"),":"), choices = joint_admin_choices, selected = joint_admin_default)
                )
              )
            }
        )
    ),
        
    dashboardBody(
      fluidPage(
        # JS  ----
        tags$head(tags$script(src = "message-handler.js")),
        # CSS ----
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
        
        tabItems_safe(
          
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
          
          if (joint_data_available) {
            tabItem(tabName = "joint_risk",
                    h2(lang_label("joint_overview_title")),
                    br(),
                    fluidRow(
                      valueBoxOutput("joint_box_low", width = 3),
                      valueBoxOutput("joint_box_medium", width = 3),
                      valueBoxOutput("joint_box_high", width = 3),
                      valueBoxOutput("joint_box_very_high", width = 3)
                    ),
                    
                    fluidRow(
                      box(width = 7,
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          title = textOutput("joint_map_title"),
                          shinycssloaders::withSpinner(leafletOutput("joint_map", height = 600), color = "#1c9ad6", type = "8", size = 0.5)
                      ),
                      box(width = 5,
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          title = textOutput("joint_bar_title"),
                          shinycssloaders::withSpinner(plotlyOutput("joint_barplot", height = 600), color = "#1c9ad6", type = "8", size = 0.5)
                      )
                    ),
                    
                    fluidRow(
                      box(width = 12,
                          solidHeader = TRUE,
                          collapsible = TRUE,
                          title = lang_label("joint_table_title"),
                          shinycssloaders::withSpinner(dataTableOutput("joint_data_table"), color = "#1c9ad6", type = "8", size = 0.5)
                      )
                    ),
                    if (joint_component_section_available) {
                      tagList(
                        br(),
                        hr(),
                        h3(lang_label("joint_component_section_title")),
                        fluidRow(
                          column(width = 5,
                                 selectInput(
                                   "joint_detail_metric",
                                   paste0(lang_label("joint_component_select"),":"),
                                   choices = joint_component_choices,
                                   selected = joint_component_default
                                 )
                          )
                        ),
                        fluidRow(
                          box(width = 7,
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              title = textOutput("joint_detail_map_title"),
                              shinycssloaders::withSpinner(leafletOutput("joint_detail_map", height = 600), color = "#1c9ad6", type = "8", size = 0.5)
                          ),
                          box(width = 5,
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              title = textOutput("joint_detail_bar_title"),
                              shinycssloaders::withSpinner(plotlyOutput("joint_detail_barplot", height = 600), color = "#1c9ad6", type = "8", size = 0.5)
                          )
                        ),
                        fluidRow(
                          box(width = 12,
                              solidHeader = TRUE,
                              collapsible = TRUE,
                              title = textOutput("joint_detail_table_title"),
                              shinycssloaders::withSpinner(dataTableOutput("joint_detail_table"), color = "#1c9ad6", type = "8", size = 0.5)
                          )
                        )
                      )
                    }
            )
          },
          
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
                                 title = lang_label("case_class_lab_short"),icon = icon("bell"),
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
                                 p(style="text-align: center;",lang_label("rap_pres_team_note")),
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
  
  # SERVER JOINT RISK ----
  if (joint_data_available) {
    joint_admin_label <- function(value) {
      if (is.null(value) || value == "ALL") {
        return(joint_admin_all_label)
      }
      label <- joint_admin_lookup$admin1_label[joint_admin_lookup$admin1_key == value]
      ifelse(length(label) == 0, value, label)
    }
    
    joint_filtered_data <- reactive({
      req(nrow(joint_base_data) > 0)
      selected_admin <- input$joint_admin_filter
      dat <- joint_base_data
      if (!is.null(selected_admin) && selected_admin != "ALL") {
        dat <- dat %>% filter(admin1_key == selected_admin)
      }
      dat %>% distinct(admin1_key, admin2_key, .keep_all = TRUE)
    })
    
    joint_counts <- reactive({
      dat <- joint_filtered_data()
      counts <- dat %>%
        count(level = joint_risk_level, name = "n") %>%
        mutate(level = as.character(level))
      tibble(level = joint_risk_levels) %>%
        left_join(counts, by = "level") %>%
        mutate(
          n = replace_na(n, 0L),
          level = factor(level, levels = joint_risk_levels)
        )
    })
    
    joint_value_summary <- reactive({
      counts_tbl <- joint_counts()
      counts_named <- setNames(counts_tbl$n, as.character(counts_tbl$level))
      counts_named <- counts_named[joint_risk_levels]
      counts_named[is.na(counts_named)] <- 0
      total_val <- sum(counts_tbl$n)
      list(
        counts = counts_named,
        total = ifelse(total_val == 0, 1, total_val)
      )
    })

    get_joint_count <- function(summary_counts, level) {
      val <- summary_counts[[level]]
      if (is.null(val) || is.na(val)) {
        return(0)
      }
      val
    }

    joint_box_location <- reactive({
      selected <- input$joint_admin_filter
      if (is.null(selected) || selected == "ALL") {
        return(toupper(COUNTRY_NAME))
      }
      label <- joint_admin_lookup$admin1_label[joint_admin_lookup$admin1_key == selected]
      if (length(label) == 0 || is.na(label)) {
        return(toupper(selected))
      }
      toupper(label)
    })
    
    joint_map_data <- reactive({
      shapes <- country_shapes
      selected_admin <- input$joint_admin_filter
      if (!is.null(selected_admin) && selected_admin != "ALL") {
        shapes <- shapes %>% filter(ADMIN1_KEY == selected_admin)
      }
      shapes <- shapes %>%
        left_join(joint_filtered_data(), by = c("ADMIN1_KEY" = "admin1_key","ADMIN2_KEY" = "admin2_key"))

      shapes
    })

    
    output$joint_map_title <- renderText({
      sprintf(lang_label("joint_map_title"), joint_admin_label(input$joint_admin_filter))
    })
    
    output$joint_bar_title <- renderText({
      sprintf(lang_label("joint_distribution_title"), joint_admin_label(input$joint_admin_filter))
    })
    
    output$joint_box_low <- renderValueBox({
      summary <- joint_value_summary()
      low_value <- get_joint_count(summary$counts, "Low")
      subtitle <- sprintf(lang_label("joint_valuebox_low"), joint_box_location())
      valueBox(
        VB_style(get_box_text(low_value, summary$total, "LR"),"font-size: 90%;"),
        VB_style(subtitle,"font-size: 95%;"),
        icon = icon('ok-sign', lib = 'glyphicon'),
        color = "purple"
      )
    })
    outputOptions(output, "joint_box_low", suspendWhenHidden = FALSE)
    
    output$joint_box_medium <- renderValueBox({
      summary <- joint_value_summary()
      medium_value <- get_joint_count(summary$counts, "Medium")
      subtitle <- sprintf(lang_label("joint_valuebox_medium"), joint_box_location())
      valueBox(
        VB_style(get_box_text(medium_value, summary$total, "MR"),"font-size: 90%;"),
        VB_style(subtitle,"font-size: 95%;"),
        icon = icon('minus-sign', lib = 'glyphicon'),
        color = "purple"
      )
    })
    outputOptions(output, "joint_box_medium", suspendWhenHidden = FALSE)
    
    output$joint_box_high <- renderValueBox({
      summary <- joint_value_summary()
      high_value <- get_joint_count(summary$counts, "High")
      subtitle <- sprintf(lang_label("joint_valuebox_high"), joint_box_location())
      valueBox(
        VB_style(get_box_text(high_value, summary$total, "HR"),"font-size: 90%;"),
        VB_style(subtitle,"font-size: 95%;"),
        icon = icon('exclamation-sign', lib = 'glyphicon'),
        color = "purple"
      )
    })
    outputOptions(output, "joint_box_high", suspendWhenHidden = FALSE)
    
    output$joint_box_very_high <- renderValueBox({
      summary <- joint_value_summary()
      very_high_value <- get_joint_count(summary$counts, "Very high")
      subtitle <- sprintf(lang_label("joint_valuebox_very_high"), joint_box_location())
      valueBox(
        VB_style(get_box_text(very_high_value, summary$total, "VHR"),"font-size: 90%;"),
        VB_style(subtitle,"font-size: 95%;"),
        icon = icon('alert', lib = 'glyphicon'),
        color = "purple"
      )
    })
    outputOptions(output, "joint_box_very_high", suspendWhenHidden = FALSE)
    
    output$joint_map <- renderLeaflet({
      map_data <- joint_map_data()
      req(nrow(map_data) > 0)
      pal <- colorFactor(palette = joint_palette, domain = joint_risk_levels, levels = joint_risk_levels, na.color = "#666666")
      no_data_label <- lang_label("no_data")
      joint_labels <- ifelse(is.na(map_data$joint_risk_level), no_data_label, as.character(map_data$joint_risk_level))
      measles_labels <- ifelse(is.na(map_data$measles_risk_level), no_data_label, as.character(map_data$measles_risk_level))
      polio_labels <- ifelse(is.na(map_data$polio_risk_level), no_data_label, as.character(map_data$polio_risk_level))
      shape_label <- sprintf(
        "<strong>%s</strong>, %s<br/>%s: %s<br/>%s: %s<br/>%s: %s",
        map_data$ADMIN2,
        map_data$ADMIN1,
        lang_label("joint_label_joint"),
        joint_labels,
        lang_label("joint_label_measles"),
        measles_labels,
        lang_label("joint_label_polio"),
        polio_labels
      ) %>% lapply(HTML)

      map_data <- map_data %>% 
        ungroup() %>% 
        select(ADMIN1_GEO_ID, GEO_ID, ADMIN1, ADMIN2, joint_risk_level, geometry) %>% 
        st_as_sf()
      

      leaflet(options = leafletOptions(doubleClickZoom = TRUE, attributionControl = FALSE, zoomSnap = 0.1, zoomDelta = 0.1)) %>%
        addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
        addPolygons(data = map_data,
          fillColor   = ~pal(joint_risk_level),
          fillOpacity = 0.7,
          weight      = 1,
          color       = "#333333",
          opacity     = 1,
          highlight = highlightOptions(
            weight = 2,
            color = "#333333",
            fillOpacity = 1,
            bringToFront = TRUE
          ),
          label = shape_label,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(
          title = lang_label("joint_risk_level_title"),
          colors = joint_palette[rev(joint_risk_levels)],
          labels = rev(joint_risk_levels),
          opacity = 0.7,
          position = 'topright'
        )
    })
    
    output$joint_barplot <- renderPlotly({
      counts_tbl <- joint_counts()
      plot_ly(
        counts_tbl,
        x = ~level,
        y = ~n,
        type = "bar",
        color = ~level,
        colors = joint_palette[joint_risk_levels],
        text = ~n,
        textposition = "outside"
      ) %>%
        layout(
          xaxis = list(title = "", tickfont = list(size = 12)),
          yaxis = list(title = lang_label("joint_num_municipalities"), tickfont = list(size = 12)),
          showlegend = FALSE,
          margin = list(l = 40, r = 20, t = 30, b = 60)
        ) %>%
        config(displaylogo = FALSE)
    })
    
    output$joint_data_table <- renderDataTable(server = FALSE,{
      admin1_col <- lang_label("joint_table_admin1")
      admin2_col <- lang_label("joint_table_admin2")
      measles_points_col <- lang_label("joint_table_measles_points")
      measles_level_col <- lang_label("joint_table_measles_level")
      polio_score_col <- lang_label("joint_table_polio_score")
      polio_level_col <- lang_label("joint_table_polio_level")
      joint_level_col_name <- lang_label("joint_risk_level_title")
      dat <- joint_filtered_data() %>%
        transmute(
          !!admin1_col := admin1_label,
          !!admin2_col := admin2_label,
          !!measles_points_col := measles_risk_points,
          !!measles_level_col := as.character(measles_risk_level),
          !!polio_score_col := polio_total_score,
          !!polio_level_col := as.character(polio_risk_level),
          !!joint_level_col_name := as.character(joint_risk_level)
        )
      
      datatable(
        dat,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          dom = 'Bfrtip',
          buttons = c('copy','csv','excel'),
          pageLength = 10,
          scrollX = TRUE
        )
      ) %>%
        formatStyle(
          joint_level_col_name,
          backgroundColor = styleEqual(
            joint_risk_levels,
            joint_palette[joint_risk_levels]
          ),
          color = "black"
        )
    })
    
    if (joint_component_section_available) {
      joint_component_display_label <- function(key) {
        label <- joint_component_label_lookup[[key]]
        if (is.null(label) || is.na(label) || label == "") {
          return(stringr::str_to_title(key))
        }
        label
      }
      
      joint_component_filtered_data <- reactive({
        req(length(joint_component_data_lookup) > 0)
        available_keys <- names(joint_component_data_lookup)
        req(length(available_keys) > 0)
        selected_key <- input$joint_detail_metric
        if (is.null(selected_key) || !(selected_key %in% available_keys)) {
          selected_key <- available_keys[1]
        }
        dat <- joint_component_data_lookup[[selected_key]]
        req(!is.null(dat))
        selected_admin <- input$joint_admin_filter
        if (!is.null(selected_admin) && selected_admin != "ALL") {
          dat <- dat %>% filter(admin1_key == selected_admin)
        }
        list(
          key = selected_key,
          data = dat %>% distinct(admin1_key, admin2_key, .keep_all = TRUE)
        )
      })
      
      joint_component_counts <- reactive({
        info <- joint_component_filtered_data()
        joint_level_col <- paste0("joint_", info$key, "_risk_level")
        counts <- info$data %>%
          mutate(level = as.character(.data[[joint_level_col]])) %>%
          count(level, name = "n")
        tibble(level = joint_risk_levels) %>%
          left_join(counts, by = "level") %>%
          mutate(
            n = replace_na(n, 0L),
            level = factor(level, levels = joint_risk_levels)
          )
      })
      
      joint_component_map_data <- reactive({
        info <- joint_component_filtered_data()
        shapes <- country_shapes
        selected_admin <- input$joint_admin_filter
        if (!is.null(selected_admin) && selected_admin != "ALL") {
          shapes <- shapes %>% filter(ADMIN1_KEY == selected_admin)
        }
        shapes <- shapes %>%
          left_join(info$data, by = c("ADMIN1_KEY" = "admin1_key","ADMIN2_KEY" = "admin2_key"))
        list(
          key = info$key,
          data = shapes
        )
      })
      
      output$joint_detail_map_title <- renderText({
        info <- joint_component_filtered_data()
        component_label <- joint_component_display_label(info$key)
        sprintf(
          lang_label("joint_component_map_title"),
          component_label,
          joint_admin_label(input$joint_admin_filter)
        )
      })
      
      output$joint_detail_bar_title <- renderText({
        info <- joint_component_filtered_data()
        component_label <- joint_component_display_label(info$key)
        sprintf(
          lang_label("joint_component_distribution_title"),
          component_label,
          joint_admin_label(input$joint_admin_filter)
        )
      })
      
      output$joint_detail_table_title <- renderText({
        info <- joint_component_filtered_data()
        component_label <- joint_component_display_label(info$key)
        sprintf(
          lang_label("joint_component_table_title"),
          component_label,
          joint_admin_label(input$joint_admin_filter)
        )
      })
      
      output$joint_detail_map <- renderLeaflet({
        map_info <- joint_component_map_data()
        map_data <- map_info$data
        key <- map_info$key
        joint_level_col <- paste0("joint_", key, "_risk_level")
        measles_level_col <- paste0("measles_", key, "_risk_level")
        polio_level_col <- paste0("polio_", key, "_risk_level")
        req(joint_level_col %in% names(map_data))
        req(nrow(map_data) > 0)
        pal <- colorFactor(palette = joint_palette, domain = joint_risk_levels, levels = joint_risk_levels, na.color = "#666666")
        no_data_label <- lang_label("no_data")
        joint_labels <- ifelse(is.na(map_data[[joint_level_col]]), no_data_label, as.character(map_data[[joint_level_col]]))
        measles_labels <- ifelse(is.na(map_data[[measles_level_col]]), no_data_label, as.character(map_data[[measles_level_col]]))
        polio_labels <- ifelse(is.na(map_data[[polio_level_col]]), no_data_label, as.character(map_data[[polio_level_col]]))
        component_label <- joint_component_display_label(key)
        shape_label <- sprintf(
          "<strong>%s</strong>, %s<br/>%s %s: %s<br/>%s: %s<br/>%s: %s",
          map_data$ADMIN2,
          map_data$ADMIN1,
          lang_label("joint_label_joint"),
          component_label,
          joint_labels,
          lang_label("joint_label_measles"),
          measles_labels,
          lang_label("joint_label_polio"),
          polio_labels
        ) %>% lapply(HTML)
        map_sf <- map_data %>%
          ungroup() %>%
          select(ADMIN1_GEO_ID, GEO_ID, ADMIN1, ADMIN2, all_of(joint_level_col), geometry) %>%
          rename(joint_display = all_of(joint_level_col)) %>%
          st_as_sf()
        leaflet(options = leafletOptions(doubleClickZoom = TRUE, attributionControl = FALSE, zoomSnap = 0.1, zoomDelta = 0.1)) %>%
          addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
          addPolygons(data = map_sf,
            fillColor   = ~pal(joint_display),
            fillOpacity = 0.7,
            weight      = 1,
            color       = "#333333",
            opacity     = 1,
            highlight = highlightOptions(
              weight = 2,
              color = "#333333",
              fillOpacity = 1,
              bringToFront = TRUE
            ),
            label = shape_label,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "15px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            title = sprintf(lang_label("joint_component_joint_level"), component_label),
            colors = joint_palette[rev(joint_risk_levels)],
            labels = rev(joint_risk_levels),
            opacity = 0.7,
            position = 'topright'
          )
      })
      
      output$joint_detail_barplot <- renderPlotly({
        counts_tbl <- joint_component_counts()
        plot_ly(
          counts_tbl,
          x = ~level,
          y = ~n,
          type = "bar",
          color = ~level,
          colors = joint_palette[joint_risk_levels],
          text = ~n,
          textposition = "outside"
        ) %>%
        layout(
          xaxis = list(title = "", tickfont = list(size = 12)),
          yaxis = list(title = lang_label("joint_num_municipalities"), tickfont = list(size = 12)),
            showlegend = FALSE,
            margin = list(l = 40, r = 20, t = 30, b = 60)
          ) %>%
          config(displaylogo = FALSE)
      })
      
      output$joint_detail_table <- renderDataTable(server = FALSE,{
        info <- joint_component_filtered_data()
        key <- info$key
        component_label <- joint_component_display_label(key)
        measles_score_col <- paste0("measles_", key, "_score")
        measles_level_col <- paste0("measles_", key, "_risk_level")
        polio_score_col <- paste0("polio_", key, "_score")
        polio_level_col <- paste0("polio_", key, "_risk_level")
        joint_level_col <- paste0("joint_", key, "_risk_level")
        admin1_col <- lang_label("joint_table_admin1")
        admin2_col <- lang_label("joint_table_admin2")
        measles_score_label <- sprintf(lang_label("joint_component_measles_score"), component_label)
        measles_level_label <- sprintf(lang_label("joint_component_measles_level"), component_label)
        polio_score_label <- sprintf(lang_label("joint_component_polio_score"), component_label)
        polio_level_label <- sprintf(lang_label("joint_component_polio_level"), component_label)
        joint_level_label <- sprintf(lang_label("joint_component_joint_level"), component_label)
        dat <- info$data %>%
          transmute(
            !!admin1_col := admin1_label,
            !!admin2_col := admin2_label,
            !!measles_score_label := .data[[measles_score_col]],
            !!measles_level_label := as.character(.data[[measles_level_col]]),
            !!polio_score_label := .data[[polio_score_col]],
            !!polio_level_label := as.character(.data[[polio_level_col]]),
            !!joint_level_label := as.character(.data[[joint_level_col]])
          )
        datatable(
          dat,
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(
            dom = 'Bfrtip',
            buttons = c('copy','csv','excel'),
            pageLength = 10,
            scrollX = TRUE
          )
        ) %>%
          formatStyle(
            joint_level_label,
            backgroundColor = styleEqual(
              joint_risk_levels,
              joint_palette[joint_risk_levels]
            ),
            color = "black"
          )
      })
    }
  }
  
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
      paste0(lang_label("map")," ",input$calidad_select_admin1," ",toupper(COUNTRY_NAME)," ",lang_label("case_class_lab")," (",YEAR_1,"-",YEAR_5,").png")
    },
    content = function(file) {
      mapshot(calidad_map_5$dat, file = file)
    }
  )
  ### Silent muni map output ####
  # Output for cal_plot_map_data with silent municipalities
  output$calidad_map_5 <- renderLeaflet({
    calidad_map_5$dat <- cal_plot_map_data(LANG_TLS,toupper(COUNTRY_NAME),YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,country_shapes,calidad_data,"case_class",input$calidad_select_admin1,get_a1_geo_id(input$calidad_select_admin1),admin1_geo_id_df)
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
      icon = icon("bell"),
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
