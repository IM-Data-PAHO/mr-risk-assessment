##############################################################
# Herramienta digital Análisis de Riesgo SR - thre_asse.R
# Organización Panamericana de la Salud
# Autor: Luis Quezada
# Última fecha de modificación: 2023-08-17
# R 4.3.0
##############################################################


amenaza_title_map <- function(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var) {
  YEAR_1=YEAR_LIST[1];YEAR_2=YEAR_LIST[2];YEAR_3=YEAR_LIST[3];YEAR_4=YEAR_LIST[4];YEAR_5=YEAR_LIST[5];
  var_text <- case_when(
    var == "TOTAL_PR" ~ paste0(lang_label_tls(LANG_TLS,"thre_title_map_total_pr")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_1," - ",YEAR_5,")"),
    var == "dens_pob_PR" ~ paste0(lang_label_tls(LANG_TLS,"thre_title_map_pop_dens")," ",admin1_transform(LANG_TLS,COUNTRY_NAME,admin1)," (",YEAR_1," - ",YEAR_5,")")
  )
  return(var_text)
}


amenaza_plot_map_data <- function(LANG_TLS,COUNTRY_NAME,YEAR_LIST,ZERO_POB_LIST,CUT_OFFS,map_data,data,var_to_summarise,admin1,admin1_id,admin1_geo_id_df) {
  
  indicator <- "THRE_ASSE"
  data <- data %>% select(-ADMIN1,-ADMIN2)
  map_data <- full_join(map_data,data,by="GEO_ID")
  
  map_data$`ADMIN1 GEO_ID`[is.na(map_data$`ADMIN1 GEO_ID`) & map_data$ADMIN1 == admin1] <- admin1_geo_id_df$`ADMIN1 GEO_ID`[admin1_geo_id_df$ADMIN1 == admin1]
  
  map_data <- map_data %>% rename("var"=var_to_summarise)
  
  if (admin1_id == 0) {
    map_data <- map_data %>% select(ADMIN1,ADMIN2,var,geometry,GEO_ID)
  } else {
    map_data <- map_data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(ADMIN1,ADMIN2,var,geometry,GEO_ID)
  }
  
  if (var_to_summarise == "TOTAL_PR") {
    
    map_data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,indicator,map_data$var)
    map_data$risk_level[map_data$GEO_ID %in% ZERO_POB_LIST] <- "NO_HAB"
    
    map_data <- map_data %>% mutate(
      risk_level_num = case_when(
        is.na(risk_level) ~ 0,
        risk_level == lang_label_tls(LANG_TLS,"LR") ~ 1,
        risk_level == lang_label_tls(LANG_TLS,"MR") ~ 2,
        risk_level == lang_label_tls(LANG_TLS,"HR") ~ 3,
        risk_level == lang_label_tls(LANG_TLS,"VHR") ~ 4,
        risk_level == "NO_HAB" ~ 5
      ),
      risk_level_word = case_when(
        is.na(risk_level) ~ lang_label_tls(LANG_TLS,"no_data"),
        risk_level == "NO_HAB" ~ lang_label_tls(LANG_TLS,"no_hab"),
        T ~ risk_level
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
    
    if (0 %in% map_data$risk_level_num) {
      legend_colors = c("#666666",legend_colors)
      legend_values = c(lang_label_tls(LANG_TLS,"no_data"),legend_values)
    }
    
    if (length(ZERO_POB_LIST) > 0) {
      legend_colors = c(legend_colors,"#9bc2e6")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
    }
    
    shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s<br/>%s: %s",
                           map_data$ADMIN2,
                           map_data$ADMIN1,
                           lang_label_tls(LANG_TLS,"risk_points"),
                           map_data$var,
                           lang_label_tls(LANG_TLS,"risk_level"),
                           map_data$risk_level_word
    ) %>% lapply(HTML)
    
    # MAPA
    map <- leaflet(map_data,options = leafletOptions(doubleClickZoom = T, attributionControl = F, zoomSnap=0.1, zoomDelta=0.1)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(
        fillColor   = ~pal_gradient(risk_level_num),
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
      addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",amenaza_title_map(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
      addLegend(title = lang_label_tls(LANG_TLS,"legend_risk_class"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
  }
  
  if (var_to_summarise %in% c("dens_pob_PR")) {
    map_data$var <- round(map_data$var,1)
    
    pal_gradient <- colorNumeric(
      c("#92d050","#fec000","#e8132b","#920000","#9bc2e6"),
      domain = c(0,4)
    )
    
    legend_colors = c("#920000","#e8132b","#fec000","#92d050")
    legend_values = c(lang_label_tls(LANG_TLS,"cut_offs_VHR"),
                      lang_label_tls(LANG_TLS,"cut_offs_HR"),
                      lang_label_tls(LANG_TLS,"cut_offs_MR"),
                      lang_label_tls(LANG_TLS,"cut_offs_LR"))
    
    if (4 %in% map_data$var | length(ZERO_POB_LIST) > 0) {
      legend_colors = c(legend_colors,"#9bc2e6")
      legend_values = c(legend_values,lang_label_tls(LANG_TLS,"no_hab"))
    }
    
    map_data$var[is.na(map_data$var)] <- 4
    map_data$var_word = as.character(map_data$var)
    map_data$var_word[map_data$var == "4"] = "0"
    
    shape_label <- sprintf("<strong>%s</strong>, %s<br/>%s: %s",
                           map_data$ADMIN2,
                           map_data$ADMIN1,
                           lang_label_tls(LANG_TLS,"risk_points"),
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
      addLegend(layerId = "map_title","topright",color = "white", opacity = 0,labels=HTML(paste0("<strong>",amenaza_title_map(LANG_TLS,COUNTRY_NAME,YEAR_LIST,admin1,var_to_summarise),"</strong>"))) %>%
      addLegend(title = lang_label_tls(LANG_TLS,"thre_pop_dens"),colors = legend_colors,labels = legend_values, opacity = 0.5, position = 'topright')
  }
  
  return(map)
  
}

amenaza_get_data_table <- function(LANG_TLS,CUT_OFFS,data,admin1_id) {
  
  data$risk_level <- get_risk_level(LANG_TLS,CUT_OFFS,"THRE_ASSE",data$TOTAL_PR)
  
  data <- data %>% select(`ADMIN1 GEO_ID`,ADMIN1,ADMIN2,TOTAL_PR,risk_level,dens_pob,dens_pob_PR,pob_vulnerable_PR) %>%
    mutate(
      dens_pob = round((dens_pob),1)
    )
  
  if (admin1_id == 0) {
    data <- data %>% select(-`ADMIN1 GEO_ID`)
    colnames(data) <- c(lang_label_tls(LANG_TLS,"table_admin1_name"),lang_label_tls(LANG_TLS,"table_admin2_name"),
                        lang_label_tls(LANG_TLS,"total_pr"),lang_label_tls(LANG_TLS,"risk_level"),
                        lang_label_tls(LANG_TLS,"thre_table_pop_dens"),
                        lang_label_tls(LANG_TLS,"thre_table_pop_dens_pr"),
                        lang_label_tls(LANG_TLS,"thre_table_vul_pr")
                        )
    
  } else {
    data <- data %>% filter(`ADMIN1 GEO_ID` == admin1_id) %>% select(-ADMIN1,-`ADMIN1 GEO_ID`)
    colnames(data) <- c(lang_label_tls(LANG_TLS,"table_admin2_name"),
                        lang_label_tls(LANG_TLS,"total_pr"),lang_label_tls(LANG_TLS,"risk_level"),
                        lang_label_tls(LANG_TLS,"thre_table_pop_dens"),
                        lang_label_tls(LANG_TLS,"thre_table_pop_dens_pr"),
                        lang_label_tls(LANG_TLS,"thre_table_vul_pr")
    )
  }
  
  datos_table <- data %>%
    datatable(
      rownames = F,
      extensions = 'Buttons',
      options = list(
        scrollX=TRUE, scrollCollapse=TRUE,
        language = list(
          info = paste0(lang_label_tls(LANG_TLS,"data_table_showing")," _START_ ",lang_label_tls(LANG_TLS,"data_table_to")," _END_ ",lang_label_tls(LANG_TLS,"data_table_of")," _TOTAL_ ",lang_label_tls(LANG_TLS,"data_table_rows")),
          paginate = list(previous = lang_label_tls(LANG_TLS,"data_table_prev"), `next` = lang_label_tls(LANG_TLS,"data_table_next"))
        ),
        searching = TRUE,fixedColumns = TRUE,autoWidth = FALSE,
        ordering = TRUE,scrollY = TRUE,pageLength = 8,
        columnDefs = list(list(className = 'dt-right', targets = 0:(ncol(data)-1))),
        dom = 'Brtip',
        buttons = list(
          list(extend = "copy",text = lang_label_tls(LANG_TLS,"button_copy")),
          list(extend='csv',filename=paste(lang_label_tls(LANG_TLS,"data"),lang_label_tls(LANG_TLS,"THRE_ASSE"),admin1_id)),
          list(extend='excel', filename=paste(lang_label_tls(LANG_TLS,"data"),lang_label_tls(LANG_TLS,"THRE_ASSE"),admin1_id))
        ),
        class = "display"
      )
    ) %>% formatStyle(
      lang_label_tls(LANG_TLS,"risk_level"),
      backgroundColor = styleEqual(
        c(lang_label_tls(LANG_TLS,"LR"),lang_label_tls(LANG_TLS,"MR"),
          lang_label_tls(LANG_TLS,"HR"),lang_label_tls(LANG_TLS,"VHR")),
        c("rgba(146, 208, 80, 0.7)","rgba(254, 192, 0, 0.7)",
          "rgba(232, 19, 43, 0.7)","rgba(146, 0, 0, 0.7)"))
    ) %>% formatStyle(
      c(lang_label_tls(LANG_TLS,"total_pr"),
        lang_label_tls(LANG_TLS,"thre_table_pop_dens_pr"),
        lang_label_tls(LANG_TLS,"thre_table_vul_pr")),
      backgroundColor = "#e3e3e3"
    )
  
  return(datos_table)
}

