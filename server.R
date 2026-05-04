function(input, output, session) {

  ## Identidad del usuario autenticado ----
  usuario <- reactive({
    if (is.null(session$user)) "HCYATE" else str_to_upper(session$user)
  })
  grupo <- reactive({
    if (is.null(session$group)) "ANALÍTICA" else str_to_upper(session$group)
  })
  
  ## Control de visibilidad del sidebar segun grupo ----
  observeEvent(grupo(), {
    if (grupo() %in% GRUPOS_FULL) {
      visibles   <- TODOS_WRAPPERS
      primer_tab <- "consolidado"
    } else {
      visibles   <- ACCESO_TABS[[grupo()]]
      primer_tab <- sub("wrap_", "", visibles[1])
    }
    
    # Mostrar solo los items del menu accesibles para el usuario
    lapply(TODOS_WRAPPERS, function(w) shinyjs::toggle(w, condition = w %in% visibles))
    
    # Navegar al primer tab accesible del usuario
    updatebs4TabItems(session, "sidebarMenu", selected = primer_tab)
  })
  
  ## Sucursal activa derivada del tab seleccionado (reemplaza input$Sucursal) ----
  sucursal_activa <- reactive({
    unname(TAB_A_SUCURSAL[req(input$sidebarMenu)])
  })
  
  ## Datos reactivos ----
  
  # Fechas
  fecha_ej       <- reactive({ PrimerDia(input$Fecha) })
  fecha_mes_ant  <- reactive(PrimerDia(fecha_ej()) - months(1))
  fecha_anho_ant <- reactive(fecha_ej() - years(1))
  
  # Reactivo de datos Arenales
  data_are <- reactive({
    req(sucursal_activa() == "ARENALES")
    fec     <- fecha_ej()
    mes_ant <- fecha_mes_ant()
    
    acum_vig <- datos_are %>% filter(Anho == year(fec),     Mes <= month(fec))
    acum_mes <- datos_are %>% filter(Anho == year(mes_ant), Mes <= month(mes_ant))
    
    list(vig      = acum_vig %>% filter(Mes == month(fec)),
         acum_vig = acum_vig,
         mes_ant  = acum_mes %>% filter(Mes == month(mes_ant)),
         acum_mes = acum_mes)
  })
  # Reactivo de datos Trilladoras
  data_trl <- reactive({
    suc <- sucursal_activa()
    req(suc != "ARENALES")
    fec <- fecha_ej()
    ant <- fecha_anho_ant()
    
    base <- if (suc == "CONSOLIDADO") {
      datos_trl %>% filter(Sucursal != "ARENALES")
    } else {
      datos_trl %>% filter(Sucursal == suc)
    }
    
    hist     <- base %>% filter(Fecha <= fec)
    acum_vig <- hist %>% filter(Anho == year(fec), Mes <= month(fec))
    acum_ant <- hist %>% filter(Anho == year(ant),  Mes <= month(fec))
    
    list(vig      = acum_vig %>% filter(Mes == month(fec)),
         acum_vig = acum_vig,
         ant      = acum_ant %>% filter(Mes == month(fec)),
         acum_ant = acum_ant
         )
  })
  observeEvent(data_trl(), {
    assign("Trilladoras", data_trl(), envir = .GlobalEnv)
  })
  
  # Outputs ----
  ## Consolidado ----
  Consolidado("Consolidado", 
              fecha_ej, 
              data_trl,
              ind_ent  = list(vig      = Indicadores_ent_vig,
                              acum     = Indicadores_ent_acum,
                              vig_ant  = Indicadores_ent_vig_ant,
                              acum_ant = Indicadores_ent_acum_ant),
              det_prov = detalle_prov,
              det_mun  = detalle_mun)
  
  ## Header ----
  output$user <- renderUI({
    FormatearTexto(paste(usuario()) %>% HTML, negrita = T, tamano_pct = 0.75, alineacion = "center", color = "#999")
  })
  }