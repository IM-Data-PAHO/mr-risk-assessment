##############################################################
# Herramienta digital Análisis de Riesgo SR - vul_group.R
# Organización Panamericana de la Salud
# Autor: Luis Quezada
# Última fecha de modificación: 2023-08-17
# R 4.3.0
##############################################################


vul_title_map <- function(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var) {
  YEAR_1=YEAR_LIST[1];YEAR_2=YEAR_LIST[2];YEAR_3=YEAR_LIST[3];YEAR_4=YEAR_LIST[4];YEAR_5=YEAR_LIST[5];
  var_text <- case_when(
    var == "TOTAL_PR" ~ lang_label_tls(LANG_TLS,"vul_title_map_total_pr"),
    var == "pres_intercambio_pob" ~ lang_label_tls(LANG_TLS,"vul_title_map_inter_pob"),
    var == "pres_turismo" ~ lang_label_tls(LANG_TLS,"vul_title_map_turism"),
    var == "pres_problemas" ~ lang_label_tls(LANG_TLS,"vul_title_map_prob"),
    var == "pres_calamidades" ~ lang_label_tls(LANG_TLS,"vul_title_map_calam"),
    var == "dif_topo_transporte" ~ lang_label_tls(LANG_TLS,"vul_title_map_topo"),
    var == "pres_comunidades" ~ lang_label_tls(LANG_TLS,"vul_title_map_com"),
    var == "pres_trafico" ~ lang_label_tls(LANG_TLS,"vul_title_map_trafic"),
    var == "pres_eventos" ~ lang_label_tls(LANG_TLS,"vul_title_map_events")
  )
  var_text <- paste0(var_text," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_5,")")
  return(var_text)
}

vul_pres_subtitle <- function(LANG_TLS,pres) {
  subtitle_text = case_when(
    pres == "TOTAL_PR" ~ "",
    pres == "pres_intercambio_pob" ~ lang_label_tls(LANG_TLS,"vul_subtitle_inter_pob"),
    pres == "pres_turismo" ~ lang_label_tls(LANG_TLS,"vul_subtitle_turism"),
    pres == "pres_problemas" ~ lang_label_tls(LANG_TLS,"vul_subtitle_prob"),
    pres == "pres_calamidades" ~ lang_label_tls(LANG_TLS,"vul_subtitle_calam"),
    pres == "dif_topo_transporte" ~ lang_label_tls(LANG_TLS,"vul_subtitle_topo"),
    pres == "pres_comunidades" ~ lang_label_tls(LANG_TLS,"vul_subtitle_com"),
    pres == "pres_trafico" ~ lang_label_tls(LANG_TLS,"vul_subtitle_trafic"),
    pres == "pres_eventos" ~ lang_label_tls(LANG_TLS,"vul_subtitle_events")
  )
  return(subtitle_text)
}


vul_plot_map_data <- function(LANG_TLS,COUNTRY_NAME,YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,map_data,data,pres_type,admin1,admin1_id,admin1_geo_id_df) {
  
  data <- data %>% select(-ADMIN1,-ADMIN2)
  map_data <- full_join(map_data,data,by="GEO_ID")
  map_data <- map_data %>% rename(var = all_of(pres_type))
  
  map_data$`ADMIN1 GEO_ID`[is.na(map_data$`ADMIN1 GEO_ID`) & map_data$ADMIN1 == admin1] <- admin1_geo_id_df$`ADMIN1 GEO_ID`[admin1_geo_id_df$ADMIN1 == admin1]
  
  if (pres_type != "TOTAL_PR") {
    
    if (admin1_id == 0) {
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry)
    }
    
    pal_gradient <- colorNumeric(
      c("#92d050","#e8132b","#666666","#9bc2e6"),
      domain = c(0,3)
    )
    
    legend_colors = c("#e8132b","#92d050")
    legend_values = c(lang_label_tls(LANG_TLS,"vul_legend_present"),lang_label_tls(LANG_TLS,"vul_legend_not_present"))
    
    if (2 %in% map_data$var) {
      legend_colors = c("#666666",legend_colors)
      legend_values = c(lang_label_tls(LANG_TLS,"no_data"),legend_values)
    }
    
    if (length(ZERO_POB_LIST) > 0) {
      legend_colors = c(legend_colors,"#9bc2e6")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
    }
    
    map_data$var[is.na(map_data$var)] = 3 # no_hab
    
    map_data <- map_data %>% mutate(
      var_word = case_when(
        var == 0 ~ lang_label_tls(LANG_TLS,"no"),
        var == 1 ~ lang_label_tls(LANG_TLS,"yes"),
        var == 2 ~ lang_label_tls(LANG_TLS,"no_data"),
        var == 3 ~ lang_label_tls(LANG_TLS,"no_hab")
      )
    )
    
    shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s",
                           map_data$ADMIN2,
                           map_data$ADMIN1,
                           lang_label_tls(LANG_TLS,"presence"),
                           map_data$var_word
    ) %>% lapply(HTML)
    
    # MAPA
    map <- leaflet(map_data,options = leafletOptions(doubleClickZoom = T, attributionControl = F, zoomSnap=0.1, zoomDelta=0.1)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(
        fillColor   = ~pal_gradient(var),
        fillOpacity = 0.7,
        dashArray   = "",
        weight      = 1,
        color       = "#333333",
        opacity     = 1,
        highlight = highlightOptions(
          weight = 2,
          color = "#333333",
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE),
        label = shape_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>% 
      addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",vul_title_map(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,pres_type),"</strong>"))) %>%
      addLegend(title = lang_label_tls(LANG_TLS,"presence"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
    
  } else {
    
    if (admin1_id == 0) {
      map_data <- map_data %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry)
    } else {
      map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(GEO_ID,ADMIN1,ADMIN2,var,geometry)
    }
    
    map_data$var[is.na(map_data$var)] = 9 # No Hab. / Sin pob.
    map_data$var_word = as.character(map_data$var)
    map_data$var_word[map_data$var == "9"] = "0"
    map_data <- map_data %>% mutate(
      var_num = case_when(
        is.na(var) ~ 0,
        var >= 0 & var <= 2 ~ 1,
        var > 2 & var <= 4 ~ 2,
        var > 4 & var <= 6 ~ 3,
        var > 6 & var <= 8 ~ 4,
        var > 8 ~ 5
      )
    )
    
    pal_gradient <- colorNumeric(
      c("#666666","#92d050","#fec000","#e8132b","#920000","#9bc2e6"),
      domain = c(0,5)
    )
    
    legend_colors = c("#920000","#e8132b","#fec000","#92d050")
    legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_VHR"),
                      lang_label_tls(LANG_TLS,"cut_offs_HR"),
                      lang_label_tls(LANG_TLS,"cut_offs_MR"),
                      lang_label_tls(LANG_TLS,"cut_offs_LR"))
    
    if (length(ZERO_POB_LIST) > 0) {
      legend_colors = c(legend_colors,"#9bc2e6")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
    }
    
    shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s",
                           map_data$ADMIN2,
                           map_data$ADMIN1,
                           lang_label_tls(LANG_TLS,"vul_groups"),
                           map_data$var_word
    ) %>% lapply(HTML)
    
    
    # MAPA
    map <- leaflet(map_data,options = leafletOptions(doubleClickZoom = T, attributionControl = F, zoomSnap=0.1, zoomDelta=0.1)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(
        fillColor   = ~pal_gradient(var_num),
        fillOpacity = 0.8,
        dashArray   = "",
        weight      = 1,
        color       = "#333333",
        opacity     = 1,
        highlight = highlightOptions(
          weight = 2,
          color = "#333333",
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE),
        label = shape_label,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>% 
      addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",vul_title_map(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,pres_type),"</strong>"))) %>%
      addLegend(title = lang_label_tls(LANG_TLS,"legend_risk_class"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
  }

  map <- map %>% addResetMapButton()
  return(map)
  
}


