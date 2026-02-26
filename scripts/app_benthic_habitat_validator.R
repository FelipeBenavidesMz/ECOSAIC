library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(indicspecies)
library(vegan)
library(StatMatch)
library(scales)
library(shinyjs)

# Global settings
options(shiny.maxRequestSize = 500*1024^2)
options(warn = -1)

# ============================================================================
# AUTOMATIC PATH DETECTION (LOCAL vs SERVER)
# ============================================================================
detectar_rutas <- function() {
  if (file.exists("species_occurrences_with_cluster.csv")) {
    return(list(
      csv_cluster = "species_occurrences_with_cluster.csv",
      variables   = "ECOSAIC_LatentFeatures_EnvVariables_SOMclass_ColombianPacific.csv",
      literatura  = "species_environmental_preferences_literature.csv"
    ))
  } else {
    return(list(
      csv_cluster = "F:/ECOSAIC/data/outputs/species_occurrences_with_cluster.csv",
      variables   = "F:/ECOSAIC/data/outputs/ECOSAIC_LatentFeatures_EnvVariables_SOMclass_ColombianPacific.csv",
      literatura  = "F:/ECOSAIC/data/inputs/species_environmental_preferences_literature.csv"
    ))
  }
}

rutas_archivos <- detectar_rutas()

# UI
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      .main-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .main-header h2 {
        color: white;
        margin: 0;
        text-align: center;
        font-weight: bold;
      }
      .action-buttons {
        background: white;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 10px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        text-align: center;
      }
      .action-buttons .btn {
        margin: 5px;
        min-width: 250px;
        font-size: 16px;
        padding: 12px 24px;
      }
      .section-panel {
        background: white;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 10px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        display: none;
      }
      .section-panel.active {
        display: block;
      }
      .info-badge {
        display: inline-block;
        padding: 5px 10px;
        border-radius: 5px;
        margin: 5px;
        font-weight: bold;
      }
      .wellPanel {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
      }
    "))
  ),
  
  # HEADER
  div(class = "main-header",
      h2("🐠 Habitat Preference Analysis for Benthic Species of the Colombian Pacific")
  ),
  
  # MAIN ACTION BUTTONS
  div(class = "action-buttons",
      fluidRow(
        column(6,
               actionButton("btn_configuracion", 
                            "⚙️ Configuration", 
                            class = "btn-primary btn-lg btn-block",
                            icon = icon("cog"))
        ),
        column(6,
               actionButton("btn_procesar", 
                            "▶️ Process Data", 
                            class = "btn-success btn-lg btn-block",
                            icon = icon("play"))
        )
      )
  ),
  
  # CONFIGURATION PANEL
  div(id = "panel_configuracion", class = "section-panel",
      h3("⚙️ Parameter Configuration"),
      fluidRow(
        column(6,
               wellPanel(
                 h4("Habitat Filters"),
                 checkboxGroupInput("habitat_filter",
                                    "Habitat type:",
                                    choices = c("Benthic-associated" = "Bentónico",
                                                "Strictly pelagic"   = "Pelágico_puro"),
                                    selected = c("Bentónico"))
               )
        ),
        column(6,
               wellPanel(
                 h4("Specialisation Level"),
                 checkboxGroupInput("especialization_filter",
                                    "Select levels:",
                                    choices = c("Specialist (B_std < 0.3)"  = "Especialista",
                                                "Selective (B_std 0.3-0.6)" = "Selectivo",
                                                "Generalist (B_std >= 0.6)" = "Generalista"),
                                    selected = c("Especialista", "Selectivo"))
               )
        )
      ),
      fluidRow(
        column(12,
               wellPanel(
                 h4("Cluster Composition Parameters"),
                 fluidRow(
                   column(6,
                          numericInput("umbral_composicion_min", 
                                       "Minimum number of occurrences per cluster:", 
                                       value = 10, min = 1, step = 1)
                   ),
                   column(6,
                          numericInput("umbral_composicion_pct", 
                                       "Minimum % composition per cluster:", 
                                       value = 10, min = 0, max = 100, step = 1)
                   )
                 ),
                 p(style = "font-size: 11px; color: #666;",
                   "Both thresholds are applied simultaneously (AND). Species must satisfy both criteria.")
               )
        )
      ),
      div(style = "text-align: center; margin-top: 20px;",
          actionButton("aplicar_config", "✅ Apply Configuration and Continue", class = "btn-success btn-lg")
      )
  ),
  
  # PROCESSING PANEL
  div(id = "panel_procesamiento", class = "section-panel",
      h3("▶️ Data Processing"),
      wellPanel(
        h4("Summary of Current Configuration"),
        verbatimTextOutput("resumen_config"),
        hr(),
        actionButton("procesar", "🚀 RUN PROCESSING", class = "btn-success btn-lg btn-block"),
        hr(),
        h4("Processing Status"),
        verbatimTextOutput("resumen")
      )
  ),
  
  # RESULTS — single panel
  div(id = "panel_resultados", class = "section-panel",
      h3("🔬 Species Composition and Environmental Preferences per Cluster"),
      wellPanel(
        h4("Calculate Dominant Species"),
        p("Thresholds defined in Configuration will be applied when calculating dominant species."),
        actionButton("calcular_composicion", 
                     "🧬 Calculate Dominant Species", 
                     class = "btn-info btn-lg"),
        br(), br(),
        uiOutput("info_composicion_umbral")
      ),
      br(),
      wellPanel(
        h4("Results: Dominant Species per Cluster"),
        p(strong("Specificity (A):"), "Proportion of cluster occurrences belonging to this species."),
        p(strong("Fidelity (B):"), "Proportion of total species occurrences found in this cluster."),
        p(strong("IndVal:"), "Indicator Value = sqrt(Specificity x Fidelity)."),
        p(strong("OVERLAP metric:"), "Proportion of the observed range overlapping the literature range."),
        p(strong("* Main Indicator:"), "Species with highest IndVal per cluster."),
        DTOutput("tabla_composicion_umbral"),
        br(),
        downloadButton("download_composicion_umbral", "📥 Download Composition Table", class = "btn-primary")
      )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Panel visibility control
  observeEvent(input$btn_configuracion, {
    shinyjs::hide("panel_procesamiento")
    shinyjs::hide("panel_resultados")
    shinyjs::show("panel_configuracion")
  })
  
  observeEvent(input$btn_procesar, {
    shinyjs::hide("panel_configuracion")
    shinyjs::hide("panel_resultados")
    shinyjs::show("panel_procesamiento")
  })
  
  observeEvent(input$aplicar_config, {
    showNotification("✅ Configuration applied successfully", type = "message", duration = 3)
    shinyjs::hide("panel_configuracion")
    shinyjs::show("panel_procesamiento")
  })
  
  datos_procesados <- reactiveValues(
    tabla_integrada = NULL,
    tabla_completa = NULL,
    especies_filtradas = NULL,
    datos_con_cluster = NULL,
    composicion_cluster_umbral = NULL,
    datos_filtrados_validacion = NULL,
    variables_oceano = NULL,
    variables_por_cluster = NULL,
    rangos_por_cluster = NULL,
    rangos_literatura = NULL
  )
  
  normalizar_nombre_cientifico <- function(nombre) {
    if (is.na(nombre) || nombre == "") return(nombre)
    nombre <- trimws(nombre)
    nombre <- gsub("-", "_", nombre)
    nombre <- gsub("\\s+", "_", nombre)
    palabras <- strsplit(nombre, "_")[[1]]
    if (length(palabras) == 0) return(nombre)
    palabras[1] <- paste0(toupper(substring(palabras[1], 1, 1)), tolower(substring(palabras[1], 2)))
    if (length(palabras) > 1) {
      palabras[2:length(palabras)] <- tolower(palabras[2:length(palabras)])
    }
    return(paste(palabras, collapse = "_"))
  }
  
  formatear_rango_literatura <- function(valor) {
    if (is.na(valor) || valor == "" || valor == "NA") return(NA_character_)
    valor_char <- as.character(valor)
    valor_char <- trimws(valor_char)
    if (valor_char == "" || valor_char == "NA") return(NA_character_)
    if (grepl("^\\[.*\\]$", valor_char)) return(valor_char)
    return(paste0("[", valor_char, "]"))
  }
  
  extract_range_safe <- function(range_str) {
    range_clean <- gsub("\\[|\\]", "", range_str)
    pattern <- "(-?\\d+\\.?\\d*)\\s*-\\s*(-?\\d+\\.?\\d*)"
    matches <- regmatches(range_clean, regexec(pattern, range_clean))
    if(length(matches[[1]]) >= 3) {
      return(c(min = as.numeric(matches[[1]][2]), 
               max = as.numeric(matches[[1]][3])))
    }
    nums <- as.numeric(unlist(strsplit(range_clean, "-")))
    if(length(nums) == 2) {
      return(c(min = nums[1], max = nums[2]))
    } else {
      return(c(min = nums[1], max = nums[length(nums)]))
    }
  }
  
  calcular_overlap <- function(obs_min, obs_max, lit_min, lit_max) {
    intersection <- max(0, min(obs_max, lit_max) - max(obs_min, lit_min))
    obs_range <- obs_max - obs_min
    if(obs_range == 0) {
      return(ifelse(obs_min >= lit_min & obs_min <= lit_max, 1, 0))
    }
    return(intersection / obs_range)
  }
  
  calcular_indice_overlap <- function(overlap_temp, overlap_sal, overlap_prof) {
    valores <- c(overlap_temp, overlap_sal, overlap_prof)
    valores_validos <- valores[!is.na(valores)]
    if (length(valores_validos) == 0) return(NA_real_)
    return(mean(valores_validos))
  }
  
  clasificar_overlap <- function(indice) {
    if (is.na(indice)) return("Not calculated")
    if (indice >= 0.90) return("VERY HIGH")
    if (indice >= 0.70) return("HIGH")
    if (indice >= 0.50) return("MODERATE")
    return("LOW")
  }
  
  clasificar_habitat <- function(class, order, family, genus, scientific_name) {
    if (is.na(class)) class <- ""
    if (is.na(order)) order <- ""
    if (is.na(family)) family <- ""
    if (is.na(genus)) genus <- ""
    if (is.na(scientific_name)) scientific_name <- ""
    class_lower <- tolower(as.character(class))
    order_lower <- tolower(as.character(order))
    family_lower <- tolower(as.character(family))
    genus_lower <- tolower(as.character(genus))
    if (class_lower == "mammalia") return("Pelágico_puro")
    if (class_lower == "aves") return("Pelágico_puro")
    if (class_lower == "cephalopoda" && grepl("loligo|doryteuthis|dosidicus", genus_lower)) return("Pelágico_puro")
    if (class_lower %in% c("actinopteri", "elasmobranchii")) {
      if (order_lower %in% c("scombriformes") || family_lower %in% c("scombridae")) return("Pelágico_puro")
    }
    if (order_lower == "clupeiformes") return("Pelágico_puro")
    if (class_lower %in% c("hydrozoa", "scyphozoa", "cubozoa")) return("Pelágico_puro")
    if (class_lower == "thaliacea") return("Pelágico_puro")
    return("Bentónico")
  }
  
  # Configuration summary
  output$resumen_config <- renderPrint({
    cat("=== CURRENT CONFIGURATION ===\n\n")
    cat("Habitat type   :", paste(input$habitat_filter, collapse = ", "), "\n")
    cat("Specialisation :", paste(input$especialization_filter, collapse = ", "), "\n")
    cat("\nCluster composition thresholds:\n")
    cat("  - Minimum occurrences :", input$umbral_composicion_min, "\n")
    cat("  - Minimum composition  :", input$umbral_composicion_pct, "%\n")
  })
  
  # PROCESS DATA
  observeEvent(input$procesar, {
    withProgress(message = 'Processing data...', value = 0, {
      
      incProgress(0.1, detail = "Validating input files...")
      
      csv_cluster_path <- rutas_archivos$csv_cluster
      variables_path <- rutas_archivos$variables
      literatura_path <- rutas_archivos$literatura
      
      if (!file.exists(csv_cluster_path) || !file.exists(variables_path)) {
        showNotification("One or more required files were not found.", type = "error", duration = 10)
        return(NULL)
      }
      
      incProgress(0.2, detail = "Loading species occurrences with cluster assignment...")
      
      datos_con_cluster <- tryCatch({
        read.csv(csv_cluster_path, h = TRUE, sep = ',', stringsAsFactors = FALSE,
                 fileEncoding = "UTF-8", na.strings = c("", "NA")) %>%
          filter(!is.na(cluster))
      }, error = function(e) {
        showNotification(paste("Error loading occurrences:", e$message), type = "error", duration = 10)
        return(NULL)
      })
      
      req(datos_con_cluster)
      datos_procesados$datos_con_cluster <- datos_con_cluster
      
      incProgress(0.3, detail = "Loading ECOSAIC environmental variables...")
      
      variables_oceano <- tryCatch({
        read.csv(variables_path, h = TRUE, sep = ',', stringsAsFactors = FALSE,
                 fileEncoding = "UTF-8", na.strings = c("", "NA"))
      }, error = function(e) {
        showNotification(paste("Error loading variables:", e$message), type = "error", duration = 10)
        return(NULL)
      })
      
      req(variables_oceano)
      
      incProgress(0.4, detail = "Loading species environmental preferences from literature...")
      
      rangos_literatura <- tryCatch({
        if (file.exists(literatura_path)) {
          lit_data_raw <- read.csv(literatura_path, header = TRUE, sep = ",",
                                   stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                                   na.strings = c("", "NA"), colClasses = "character")
          result <- lit_data_raw %>%
            mutate(scientific_name = sapply(scientific_name, normalizar_nombre_cientifico),
                   Temp_lit = sapply(Temp_lit, formatear_rango_literatura),
                   Sal_lit = sapply(Sal_lit, formatear_rango_literatura),
                   Prof_lit = sapply(Prof_lit, formatear_rango_literatura)) %>%
            select(scientific_name, Temp_lit, Sal_lit, Prof_lit) %>%
            filter(!is.na(scientific_name), scientific_name != "", scientific_name != "scientific_name") %>%
            distinct(scientific_name, .keep_all = TRUE)
          result
        } else {
          NULL
        }
      }, error = function(e) {
        return(NULL)
      })
      
      datos_procesados$rangos_literatura <- rangos_literatura
      datos_procesados$variables_oceano <- variables_oceano
      
      incProgress(0.5, detail = "Classifying species by habitat type...")
      
      clasificacion_especies <- datos_con_cluster %>%
        select(scientific_name, class, order, family, genus) %>%
        distinct() %>%
        mutate(
          class = ifelse(is.na(class), "", class),
          order = ifelse(is.na(order), "", order),
          family = ifelse(is.na(family), "", family),
          genus = ifelse(is.na(genus), "", genus),
          scientific_name = ifelse(is.na(scientific_name), "", scientific_name)
        ) %>%
        rowwise() %>%
        mutate(tipo_habitat = clasificar_habitat(class, order, family, genus, scientific_name)) %>%
        ungroup() %>%
        select(scientific_name, tipo_habitat) %>%
        distinct(scientific_name, .keep_all = TRUE)
      
      conteo_especies <- datos_con_cluster %>%
        group_by(scientific_name) %>%
        summarise(n_ocurrencias = n(), .groups = 'drop')
      
      incProgress(0.6, detail = "Applying occurrence filter (>= 10 records)...")
      
      especies_seleccionadas <- conteo_especies %>%
        filter(n_ocurrencias >= 10) %>%
        pull(scientific_name)
      
      datos_filtrados <- datos_con_cluster %>%
        filter(scientific_name %in% especies_seleccionadas)
      
      incProgress(0.7, detail = "Calculating Levins standardised niche breadth...")
      
      datos_por_cluster_completo <- datos_filtrados %>%
        group_by(scientific_name, cluster) %>%
        summarise(n_cluster = n(), .groups = 'drop') %>%
        group_by(scientific_name) %>%
        mutate(total_especie = sum(n_cluster), proporcion = n_cluster / total_especie,
               porcentaje = round(proporcion * 100, 2)) %>%
        arrange(scientific_name, desc(porcentaje))
      
      metricas_levins_completo <- datos_por_cluster_completo %>%
        group_by(scientific_name) %>%
        summarise(total_ocurrencias = first(total_especie), n_clusters_totales = n(),
                  simpson_index = sum(proporcion^2), levins_B = 1 / sum(proporcion^2),
                  levins_B_std = (1 / sum(proporcion^2)) / n(), .groups = 'drop') %>%
        mutate(nivel_preferencia = case_when(
          levins_B_std < 0.3 ~ "Especialista",
          levins_B_std >= 0.3 & levins_B_std < 0.6 ~ "Selectivo",
          TRUE ~ "Generalista"))
      
      datos_por_cluster_filtrado <- datos_por_cluster_completo %>%
        filter(porcentaje >= 10)
      
      especies_sin_clusters <- metricas_levins_completo %>%
        anti_join(datos_por_cluster_filtrado %>% select(scientific_name) %>% distinct(),
                  by = "scientific_name") %>%
        pull(scientific_name)
      
      if(length(especies_sin_clusters) > 0) {
        cluster_dominante_backup <- datos_por_cluster_completo %>%
          filter(scientific_name %in% especies_sin_clusters) %>%
          group_by(scientific_name) %>%
          slice_max(porcentaje, n = 1, with_ties = FALSE) %>%
          ungroup()
        datos_por_cluster_filtrado <- bind_rows(datos_por_cluster_filtrado, cluster_dominante_backup)
      }
      
      clusters_preferentes_info <- datos_por_cluster_filtrado %>%
        group_by(scientific_name) %>%
        summarise(n_clusters_preferentes = n(),
                  clusters_preferentes = paste(cluster, collapse = ", "),
                  porcentajes_preferentes = paste(paste0(porcentaje, "%"), collapse = ", "),
                  suma_porcentajes_preferentes = sum(porcentaje), .groups = 'drop')
      
      incProgress(0.8, detail = "Summarising environmental variables per cluster...")
      
      # Columns used: som_opt.unit.classif, LE1_FisicoBio_norm, LE2_Hidro_norm,
      #               Temperature, Salinity, L_t_m (local terrain mean)
      variables_por_cluster <- variables_oceano %>%
        group_by(cluster = som_opt.unit.classif) %>%
        summarise(
          LE1_median_cluster   = median(LE1_FisicoBio_norm, na.rm = TRUE),
          LE2_median_cluster   = median(LE2_Hidro_norm,     na.rm = TRUE),
          temp_median_cluster  = median(Temperature,        na.rm = TRUE),
          sal_median_cluster   = median(Salinity,           na.rm = TRUE),
          depth_median_cluster = median(abs(L_t_m),         na.rm = TRUE),
          n_pixels = n(), 
          .groups = 'drop'
        )
      
      datos_procesados$variables_por_cluster <- variables_por_cluster
      
      rangos_por_cluster <- variables_oceano %>%
        group_by(cluster = som_opt.unit.classif) %>%
        summarise(
          temp_min  = min(Temperature, na.rm = TRUE),
          temp_max  = max(Temperature, na.rm = TRUE),
          sal_min   = min(Salinity,    na.rm = TRUE),
          sal_max   = max(Salinity,    na.rm = TRUE),
          depth_min = min(abs(L_t_m),  na.rm = TRUE),
          depth_max = max(abs(L_t_m),  na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(temp_range  = paste0("[", round(temp_min,  2), "-", round(temp_max,  2), "]"),
               sal_range   = paste0("[", round(sal_min,   2), "-", round(sal_max,   2), "]"),
               depth_range = paste0("[", round(depth_min, 2), "-", round(depth_max, 2), "]"))
      
      datos_procesados$rangos_por_cluster <- rangos_por_cluster
      
      variables_especies_global <- datos_por_cluster_filtrado %>%
        select(scientific_name, cluster) %>%
        left_join(variables_por_cluster, by = "cluster") %>%
        group_by(scientific_name) %>%
        summarise(clusters_preferentes_lista = paste(sort(unique(cluster)), collapse = ", "), .groups = 'drop')
      
      incProgress(0.9, detail = "Building integrated results table...")
      
      tabla_integrada_completa <- metricas_levins_completo %>%
        left_join(clusters_preferentes_info, by = "scientific_name") %>%
        left_join(variables_especies_global, by = "scientific_name") %>%
        left_join(clasificacion_especies, by = "scientific_name") %>%
        distinct(scientific_name, .keep_all = TRUE) %>%
        arrange(levins_B_std)
      
      datos_procesados$tabla_completa <- tabla_integrada_completa
      
      tabla_integrada_final <- tabla_integrada_completa %>%
        filter(tipo_habitat %in% input$habitat_filter,
               nivel_preferencia %in% input$especialization_filter)
      
      incProgress(1, detail = "Complete!")
      
      datos_procesados$tabla_integrada <- tabla_integrada_final
      datos_procesados$especies_filtradas <- especies_seleccionadas
      datos_procesados$datos_filtrados_validacion <- datos_con_cluster %>%
        filter(scientific_name %in% tabla_integrada_final$scientific_name)
      datos_procesados$composicion_cluster_umbral <- NULL
      
      showNotification("✅ Data processed successfully", type = "message", duration = 5)
      
      shinyjs::hide("panel_procesamiento")
      shinyjs::show("panel_resultados")
    })
  })
  
  # CALCULATE DOMINANT SPECIES
  observeEvent(input$calcular_composicion, {
    req(datos_procesados$datos_filtrados_validacion)
    req(datos_procesados$rangos_por_cluster)
    
    withProgress(message = 'Calculating dominant species...', value = 0, {
      
      incProgress(0.3, detail = "Applying thresholds...")
      
      datos_filtrados_composicion <- datos_procesados$datos_filtrados_validacion
      rangos_por_cluster <- datos_procesados$rangos_por_cluster
      rangos_literatura <- datos_procesados$rangos_literatura
      
      if (nrow(datos_filtrados_composicion) == 0) {
        showNotification("No data available with the current filters.", type = "warning", duration = 10)
        return(NULL)
      }
      
      incProgress(0.5, detail = "Computing species totals...")
      
      total_por_especie <- datos_filtrados_composicion %>%
        group_by(scientific_name) %>%
        summarise(total_especie = n(), .groups = 'drop')
      
      incProgress(0.6, detail = "Computing specificity, fidelity and IndVal per cluster...")
      
      composicion_cluster_umbral <- datos_filtrados_composicion %>%
        group_by(cluster, scientific_name) %>%
        summarise(n_ocurrencias = n(), .groups = 'drop') %>%
        left_join(total_por_especie, by = "scientific_name") %>%
        group_by(cluster) %>%
        mutate(total_cluster = sum(n_ocurrencias),
               especificidad = round(n_ocurrencias / total_cluster * 100, 2)) %>%
        ungroup() %>%
        mutate(
          fidelidad = round(n_ocurrencias / total_especie * 100, 2),
          IndVal = round(sqrt(especificidad * fidelidad), 2)
        ) %>%
        filter(n_ocurrencias >= input$umbral_composicion_min & especificidad >= input$umbral_composicion_pct) %>%
        left_join(rangos_por_cluster %>% select(cluster, temp_range, sal_range, depth_range), by = "cluster")
      
      incProgress(0.8, detail = "Computing overlap with literature ranges...")
      
      if (!is.null(rangos_literatura) && nrow(rangos_literatura) > 0) {
        composicion_cluster_umbral <- composicion_cluster_umbral %>%
          left_join(rangos_literatura, by = "scientific_name")
        
        composicion_cluster_umbral <- composicion_cluster_umbral %>%
          rowwise() %>%
          mutate(
            overlap_temp = if(!is.na(temp_range) && !is.na(Temp_lit)) {
              temp_obs <- extract_range_safe(temp_range)
              temp_lit <- extract_range_safe(Temp_lit)
              calcular_overlap(temp_obs[1], temp_obs[2], temp_lit[1], temp_lit[2])
            } else { NA_real_ },
            overlap_sal = if(!is.na(sal_range) && !is.na(Sal_lit)) {
              sal_obs <- extract_range_safe(sal_range)
              sal_lit <- extract_range_safe(Sal_lit)
              calcular_overlap(sal_obs[1], sal_obs[2], sal_lit[1], sal_lit[2])
            } else { NA_real_ },
            overlap_depth = if(!is.na(depth_range) && !is.na(Prof_lit)) {
              depth_obs <- extract_range_safe(depth_range)
              depth_lit <- extract_range_safe(Prof_lit)
              calcular_overlap(depth_obs[1], depth_obs[2], depth_lit[1], depth_lit[2])
            } else { NA_real_ },
            indice_overlap = calcular_indice_overlap(overlap_temp, overlap_sal, overlap_depth),
            categoria_overlap = clasificar_overlap(indice_overlap)
          ) %>%
          ungroup()
      }
      
      if ("IndVal" %in% names(composicion_cluster_umbral)) {
        especie_indicadora_principal <- composicion_cluster_umbral %>%
          group_by(cluster) %>%
          slice_max(IndVal, n = 1, with_ties = FALSE) %>%
          ungroup() %>%
          select(cluster, scientific_name) %>%
          mutate(es_indicadora_principal = "* Main Indicator")
        
        composicion_cluster_umbral <- composicion_cluster_umbral %>%
          left_join(especie_indicadora_principal, by = c("cluster", "scientific_name")) %>%
          mutate(es_indicadora_principal = ifelse(is.na(es_indicadora_principal), "", es_indicadora_principal))
      }
      
      composicion_cluster_umbral <- composicion_cluster_umbral %>%
        arrange(cluster, desc(IndVal)) %>%
        distinct(cluster, scientific_name, .keep_all = TRUE)
      
      incProgress(1, detail = "Complete!")
      
      datos_procesados$composicion_cluster_umbral <- composicion_cluster_umbral
      
      n_especies <- length(unique(composicion_cluster_umbral$scientific_name))
      n_clusters <- length(unique(composicion_cluster_umbral$cluster))
      
      showNotification(
        paste0("✅ Dominant species calculated: ", n_especies, " species across ", n_clusters, " clusters"),
        type = "message", duration = 5
      )
    })
  })
  
  # OUTPUTS
  output$resumen <- renderPrint({
    if (is.null(datos_procesados$tabla_integrada)) {
      cat("Waiting... Press 'RUN PROCESSING' to start the analysis.\n")
    } else {
      cat("=== PROCESSING COMPLETE ===\n\n")
      cat("Species analysed:", nrow(datos_procesados$tabla_integrada), "\n")
      if (!is.null(datos_procesados$tabla_completa)) {
        n_bentonicas <- sum(datos_procesados$tabla_completa$tipo_habitat == "Bentónico", na.rm = TRUE)
        n_pelagicas  <- sum(datos_procesados$tabla_completa$tipo_habitat == "Pelágico_puro", na.rm = TRUE)
        cat("  Benthic :", n_bentonicas, "\n")
        cat("  Pelagic :", n_pelagicas, "\n\n")
        cat("Results are available below.\n")
      }
    }
  })
  
  output$info_composicion_umbral <- renderUI({
    if (is.null(datos_procesados$composicion_cluster_umbral)) {
      div(style = "background-color: #fff3cd; border: 2px solid #ffc107; border-radius: 10px; padding: 15px; text-align: center;",
          h5(style = "color: #856404;", paste0("Press 'Calculate Dominant Species' to generate results with thresholds: >=",
                                               input$umbral_composicion_min, " occurrences AND >=", input$umbral_composicion_pct, "% composition.")))
    } else {
      n_especies_umbral <- length(unique(datos_procesados$composicion_cluster_umbral$scientific_name))
      n_clusters_umbral <- length(unique(datos_procesados$composicion_cluster_umbral$cluster))
      div(fluidRow(
        column(6, div(style = "background-color: #e1f5fe; border: 2px solid #0277bd; border-radius: 10px; padding: 15px; text-align: center;",
                      h5(style = "color: #01579b; margin: 0;", paste0("Species (>=", input$umbral_composicion_min, " occ. AND >=", input$umbral_composicion_pct, "%)")),
                      h3(style = "color: #01579b;", n_especies_umbral))),
        column(6, div(style = "background-color: #fff9c4; border: 2px solid #f9a825; border-radius: 10px; padding: 15px; text-align: center;",
                      h5(style = "color: #f57f17; margin: 0;", "Clusters with Dominant Species"),
                      h3(style = "color: #f57f17;", n_clusters_umbral)))))
    }
  })
  
  output$tabla_composicion_umbral <- renderDT({
    if (is.null(datos_procesados$composicion_cluster_umbral)) {
      return(datatable(data.frame(Message = "Press 'Calculate Dominant Species' above to generate the table."), 
                       options = list(dom = 't'), rownames = FALSE))
    }
    
    has_lit <- "Temp_lit" %in% names(datos_procesados$composicion_cluster_umbral)
    has_overlap <- "overlap_temp" %in% names(datos_procesados$composicion_cluster_umbral)
    has_indice <- "indice_overlap" %in% names(datos_procesados$composicion_cluster_umbral)
    has_indval <- "IndVal" %in% names(datos_procesados$composicion_cluster_umbral)
    has_indicadora_principal <- "es_indicadora_principal" %in% names(datos_procesados$composicion_cluster_umbral)
    
    if (has_lit && has_overlap && has_indice && has_indval && has_indicadora_principal) {
      tabla_display <- datos_procesados$composicion_cluster_umbral %>%
        select(cluster, scientific_name, es_indicadora_principal, n_ocurrencias, total_cluster, 
               especificidad, fidelidad, IndVal,
               temp_range, Temp_lit, overlap_temp,
               sal_range, Sal_lit, overlap_sal,
               depth_range, Prof_lit, overlap_depth,
               indice_overlap, categoria_overlap) %>%
        distinct(cluster, scientific_name, .keep_all = TRUE)
    } else if (has_lit && has_overlap && has_indice && has_indval) {
      tabla_display <- datos_procesados$composicion_cluster_umbral %>%
        select(cluster, scientific_name, n_ocurrencias, total_cluster, especificidad, fidelidad, IndVal,
               temp_range, Temp_lit, overlap_temp,
               sal_range, Sal_lit, overlap_sal,
               depth_range, Prof_lit, overlap_depth,
               indice_overlap, categoria_overlap) %>%
        distinct(cluster, scientific_name, .keep_all = TRUE)
    } else {
      tabla_display <- datos_procesados$composicion_cluster_umbral %>%
        select(cluster, scientific_name, n_ocurrencias, total_cluster,
               temp_range, sal_range, depth_range) %>%
        distinct(cluster, scientific_name, .keep_all = TRUE)
    }
    
    dt <- datatable(tabla_display, rownames = FALSE, filter = 'top',
                    options = list(pageLength = 25, scrollX = TRUE, searchHighlight = TRUE,
                                   order = list(list(0, 'asc')))) %>%
      formatStyle('cluster',
                  backgroundColor = styleInterval(seq(1, 20, by = 2), 
                                                  rep(c('#f5f5f5', '#e0e0e0'), length.out = 11)))
    
    if (has_indicadora_principal) {
      dt <- dt %>%
        formatStyle('es_indicadora_principal',
                    backgroundColor = styleEqual('* Main Indicator', '#ffd700'),
                    fontWeight = styleEqual('* Main Indicator', 'bold'),
                    color = styleEqual('* Main Indicator', '#b8860b'))
    }
    
    if (has_indval) {
      dt <- dt %>%
        formatRound(c('especificidad', 'fidelidad', 'IndVal'), 2) %>%
        formatStyle('especificidad',
                    background = styleColorBar(c(0, 100), '#4caf50'),
                    backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') %>%
        formatStyle('fidelidad',
                    background = styleColorBar(c(0, 100), '#2196f3'),
                    backgroundSize = '100% 90%', backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center') %>%
        formatStyle('IndVal',
                    backgroundColor = styleInterval(c(25, 50, 70), 
                                                    c('#ffffff', '#fff3cd', '#a5d6a7', '#66bb6a')),
                    fontWeight = 'bold')
    }
    
    if (has_overlap) {
      dt <- dt %>%
        formatRound(c('overlap_temp', 'overlap_sal', 'overlap_depth'), 3) %>%
        formatStyle('overlap_temp',
                    backgroundColor = styleInterval(c(0.5, 0.7, 0.9), 
                                                    c('#ffebee', '#fff9c4', '#c8e6c9', '#a5d6a7'))) %>%
        formatStyle('overlap_sal',
                    backgroundColor = styleInterval(c(0.5, 0.7, 0.9), 
                                                    c('#ffebee', '#fff9c4', '#c8e6c9', '#a5d6a7'))) %>%
        formatStyle('overlap_depth',
                    backgroundColor = styleInterval(c(0.5, 0.7, 0.9), 
                                                    c('#ffebee', '#fff9c4', '#c8e6c9', '#a5d6a7')))
    }
    
    if (has_indice) {
      dt <- dt %>%
        formatRound('indice_overlap', 3) %>%
        formatStyle('indice_overlap',
                    backgroundColor = styleInterval(c(0.5, 0.7, 0.9), 
                                                    c('#ffcdd2', '#fff59d', '#c5e1a5', '#81c784'))) %>%
        formatStyle('categoria_overlap',
                    backgroundColor = styleEqual(c('LOW', 'MODERATE', 'HIGH', 'VERY HIGH'),
                                                 c('#ffcdd2', '#fff59d', '#c5e1a5', '#81c784')),
                    fontWeight = 'bold')
    }
    
    return(dt)
  })
  
  output$download_composicion_umbral <- downloadHandler(
    filename = function() { 
      paste0("ECOSAIC_dominant_species_min", input$umbral_composicion_min,
             "occ_min", input$umbral_composicion_pct, "pct_",
             paste(input$habitat_filter, collapse = "_"), "_",
             paste(input$especialization_filter, collapse = "_"), "_",
             Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(datos_procesados$composicion_cluster_umbral)) {
        tabla_export <- datos_procesados$composicion_cluster_umbral %>% 
          distinct(cluster, scientific_name, .keep_all = TRUE)
        write.csv(tabla_export, file, row.names = FALSE)
      }
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)