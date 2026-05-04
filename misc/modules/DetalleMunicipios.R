# Pre-cómputo global ----

.agregar_dep <- memoise::memoise(function(df, col_valor) {
  df %>%
    group_by(CodDep = substr(CodMun, 1L, 2L)) %>%
    summarise("{col_valor}" := sum(.data[[col_valor]], na.rm = TRUE), .groups = "drop")
})
.agregar_mun <- memoise::memoise(function(df, col_valor) {
  df %>%
    group_by(CodMun, MunPro, NomDepPro) %>%
    summarise("{col_valor}" := sum(.data[[col_valor]], na.rm = TRUE), .groups = "drop") %>%
    mutate(CodDep = substr(CodMun, 1L, 2L))
})
.sel_kilo <- function(df, nm) df %>% select(MunPro, Kilos) %>% rename(!!nm := Kilos)


# Constantes ----
.icono_trl <- awesomeIcons(markerColor = "#DA291C", iconColor = "white", 
                           library = "fa", icon = "industry")


# MapaCoropleDpto ----

MapaCoropleDpto <- function(datos, col_valor, n_bins, escala, sufijo,
                            layer_id = NULL, marcadores = NULL) {
  datos    <- datos %>% rename(Valor = all_of(col_valor))
  geo_join <- geo_dpto %>% left_join(datos, by = c("dpto_ccdgo" = "CodDep"))
  geo_con  <- geo_join %>% filter(!is.na(Valor))
  geo_sin  <- geo_join %>% filter(is.na(Valor))
  
  pal <- colorBin("GnBu", domain = geo_con$Valor, bins = n_bins, na.color = "#A6A09B")
  
  labels_html <- unname(lapply(seq_len(nrow(geo_con)), function(i) {
    htmltools::HTML(sprintf(
      "<strong>%s</strong><br/>%s: %s", geo_con$dpto_cnmbr[i], col_valor,
      format(round(geo_con$Valor[i]), big.mark = ".", decimal.mark = ",", scientific = FALSE)
    ))
  }))
  
  mapa <- leaflet(options = leafletOptions(minZoom = 5, maxZoom = 8)) %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    addProviderTiles("Stadia.StamenTonerLabels") %>%
    setView(lng = -74.3, lat = 4.5, zoom = 6) %>%
    addPolygons(data = geo_sin, fillColor = "#FAFAF9", fillOpacity = 0.4,
                color = "#000000", weight = 1, opacity = 1) %>%
    addPolygons(
      data = geo_con, fillColor = ~pal(Valor), fillOpacity = 0.6,
      color = "#000000", weight = 0.5, opacity = 1,
      layerId = if (!is.null(layer_id)) geo_con[[layer_id]] else NULL,
      label = labels_html,
      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "4px 8px"),
                                  textsize = "13px", direction = "auto"),
      highlight = highlightOptions(weight = 2, color = "#000000",
                                   fillOpacity = 0.75, bringToFront = TRUE)
    ) %>%
    addLegend(pal = pal, values = geo_con$Valor, position = "bottomright", title = col_valor,
              labFormat = labelFormat(between = " – ", suffix = paste0(" ", sufijo), digits = 2,
                                      transform = function(x) unname(round(x / escala, digits = 2))))
  
  if (!is.null(marcadores) && nrow(marcadores) > 0) {
    mapa <- mapa %>%
      addAwesomeMarkers(data = marcadores, lng = ~lng, lat = ~lat, icon = .icono_trl,
                        label = ~Sucursal,
                        labelOptions = labelOptions(direction = "top", offset = c(0L, -10L)),
                        popup = ~paste0("<b>Trilladora ", Sucursal, "</b>"), group = "trilladoras")
  }
  mapa
}


# MapaCoropleMun ----

MapaCoropleMun <- function(datos, cod_dpto, col_valor, n_bins, escala, sufijo,
                           marcadores = NULL) {
  centro <- geo_centroides %>% filter(dpto_ccdgo == cod_dpto)
  
  geo_filt <- geo_mun %>%
    filter(dpto_ccdgo == cod_dpto) %>%
    mutate(CodMun5 = str_pad(as.character(mpio_cdpmp), 5L, pad = "0", side = "left"))
  
  geo_join <- geo_filt %>% left_join(datos, by = c("CodMun5" = "CodMun"))
  geo_con  <- geo_join %>%
    filter(!is.na(.data[[col_valor]])) %>%
    mutate(.Valor. = .data[[col_valor]])
  geo_sin  <- geo_join %>% filter(is.na(.data[[col_valor]]))
  
  vals      <- unname(geo_con[[col_valor]])
  n_bins_ef <- max(2L, min(n_bins, length(unique(vals))))
  pal       <- colorBin("GnBu", domain = vals, bins = n_bins_ef, na.color = "#A6A09B")
  
  labels_html <- unname(lapply(seq_len(nrow(geo_con)), function(i) {
    htmltools::HTML(sprintf(
      "<strong>%s</strong><br/>%s<br/>%s: %s",
      geo_con$MunPro[i], geo_con$NomDepPro[i], col_valor,
      format(round(vals[i]), big.mark = ".", decimal.mark = ",", scientific = FALSE)
    ))
  }))
  
  mapa <- leaflet(options = leafletOptions(minZoom = 8, maxZoom = 9)) %>%
    addProviderTiles("Esri.WorldGrayCanvas") %>%
    addProviderTiles("Stadia.StamenTonerLabels") %>%
    setView(lng = centro$lng[1], lat = centro$lat[1], zoom = 8) %>%
    addPolygons(data = geo_sin, fillColor = "#FAFAF9", fillOpacity = 0.4,
                color = "#000000", weight = 1, opacity = 1) %>%
    addPolygons(
      data = geo_con, fillColor = ~pal(.Valor.), fillOpacity = 0.6,
      color = "#000000", weight = 0.6, opacity = 1,
      label = labels_html,
      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "4px 8px"),
                                  textsize = "13px", direction = "auto"),
      highlight = highlightOptions(weight = 2, color = "#000000",
                                   fillOpacity = 0.75, bringToFront = TRUE)
    ) %>%
    addLegend(pal = pal, values = vals, position = "bottomright", title = col_valor,
              labFormat = labelFormat(between = " – ", suffix = paste0(" ", sufijo), digits = 2,
                                      transform = function(x) unname(round(x / escala, digits = 2))))
  
  if (!is.null(marcadores) && nrow(marcadores) > 0) {
    mapa <- mapa %>%
      addAwesomeMarkers(data = marcadores, lng = ~lng, lat = ~lat, icon = .icono_trl,
                        label = ~Sucursal,
                        labelOptions = labelOptions(direction = "top", offset = c(0L, -10L)),
                        popup = ~paste0("<b>Trilladora ", Sucursal, "</b>"), group = "trilladoras")
  }
  mapa
}


# MapaDoble ----

MapaDobleUI <- function(id, altura = "520px") {
  ns <- NS(id)
  tagList(
    waiter::useWaiter(), shinyjs::useShinyjs(),
    fluidRow(
      column(6,
             bs4Dash::bs4Card(
               width = 12, collapsible = FALSE, elevation = 1,
               title  = uiOutput(ns("titulo_dpto"), inline = TRUE),
               footer = uiOutput(ns("footer_dpto")),
               leaflet::leafletOutput(ns("mapa_dpto"), height = altura)
             )
      ),
      column(6,
             div(id = ns("panel_mun_wrap"), uiOutput(ns("panel_mun")))
      )
    )
  )
}
MapaDoble <- function(id, datos, col_valor = "Kilos", n_bins = 5L, escala = 1e6,
                      sufijo = "Millones", titulo_dpto = "Compras por Departamento", 
                      titulo_mun  = "Compras por Municipio", 
                      footer_dpto = "Haga clic en un departamento para ver el detalle municipal.",
                      marcadores  = reactive(data.frame())
                      ) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    w_dpto <- waiter::Waiter$new(id = ns("mapa_dpto"), html = preloader_calculando$html,
                                 color = preloader_calculando$color)
    w_mun  <- waiter::Waiter$new(id = ns("panel_mun_wrap"), html = preloader_calculando$html,
                                 color = preloader_calculando$color)
    
    agg_dep  <- reactive(.agregar_dep(datos(), col_valor))
    agg_mun  <- reactive(.agregar_mun(datos(), col_valor))
    dpto_sel <- reactiveVal(NULL)
    
    observeEvent(input$mapa_dpto_shape_click, {
      click <- input$mapa_dpto_shape_click
      dpto_sel(if (identical(click$id, dpto_sel())) NULL else click$id)
    })
    
    nombre_dpto <- reactive({
      req(dpto_sel())
      agg_mun() %>% filter(CodDep == dpto_sel()) %>% pull(NomDepPro) %>% first()
    })
    
    mun_sel <- reactive({
      req(dpto_sel())
      agg_mun() %>% filter(CodDep == dpto_sel()) %>% select(-CodDep)
    })
    
    output$titulo_dpto <- renderUI(strong(titulo_dpto))
    
    output$footer_dpto <- renderUI({
      if (is.null(dpto_sel())) return(span(footer_dpto, class = "text-muted small"))
      tagList(
        icon("circle-dot"), " ", strong(nombre_dpto()),
        span(" seleccionado — haga clic de nuevo para deseleccionar.",
             class = "text-muted small")
      )
    })
    
    output$titulo_mun <- renderUI(strong(paste(titulo_mun, "—", nombre_dpto())))
    output$footer_mun <- renderUI(
      span(paste0(nombre_dpto(), " · Cifras en ", sufijo), class = "text-muted small")
    )
    
    observeEvent(list(agg_dep(), marcadores()), { w_dpto$show() }, ignoreInit = FALSE)
    observeEvent(input$mapa_dpto_bounds, { w_dpto$hide() }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    output$mapa_dpto <- leaflet::renderLeaflet({
      MapaCoropleDpto(agg_dep(), col_valor = col_valor, n_bins = n_bins,
                      escala = escala, sufijo = sufijo,
                      layer_id = "dpto_ccdgo", marcadores = marcadores())
    }) %>% bindCache(agg_dep(), marcadores())
    
    output$panel_mun <- renderUI({
      if (is.null(dpto_sel())) {
        div(
          style = paste0("min-height:520px; display:flex; flex-direction:column;",
                         "align-items:center; justify-content:center;",
                         "border:2px dashed #dee2e6; border-radius:8px; color:#6c757d;"),
          icon("hand-pointer", class = "fa-2x mb-3"),
          p("Seleccione un departamento en el mapa de la izquierda", class = "mb-1 text-center"),
          p("para visualizar el detalle a nivel municipal.",
            class = "text-center text-muted small mb-0")
        )
      } else {
        bs4Dash::bs4Card(
          width = 12, collapsible = FALSE, elevation = 1,
          title  = uiOutput(ns("titulo_mun"), inline = TRUE),
          footer = uiOutput(ns("footer_mun")),
          leaflet::leafletOutput(ns("mapa_mun"), height = "520px")
        )
      }
    })
    
    observeEvent(dpto_sel(), {
      req(dpto_sel()); w_mun$show()
    }, ignoreInit = TRUE)
    observeEvent(input$mapa_mun_bounds, { w_mun$hide() }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    output$mapa_mun <- leaflet::renderLeaflet({
      req(dpto_sel(), nrow(mun_sel()) > 0)
      MapaCoropleMun(mun_sel(), cod_dpto = dpto_sel(), col_valor = col_valor,
                     n_bins = n_bins, escala = escala, sufijo = sufijo,
                     marcadores = marcadores())
    }) %>% bindCache(dpto_sel(), mun_sel(), marcadores())
  })
}


# DetalleMun ----
DetalleMunUI <- function(id) {
  ns <- NS(id)
  tagList(
    MapaDobleUI(ns("mapa_det")), br(),
    TablaReactableUI(ns("tbl_det"), mostrar_nota = FALSE,
                     footer = paste0("Columna ordenable dando clic en el encabezado",
                                     " · Use la búsqueda para filtrar"))
  )
}
DetalleMun <- function(id, fec, det_mun, suc_r) {
  moduleServer(id, function(input, output, session) {
    
    .labels <- function() {
      anho     <- year(fec())
      mes_lbl  <- str_to_title(format(fec(), "%b. %Y"))
      mes_ant  <- str_to_title(format(fec() - years(1L), "%b. %Y"))
      acum_lbl <- paste0("Acum. Ene–", str_to_title(format(fec(), "%b. %Y")))
      acum_ant <- paste0("Acum. Ene–", str_to_title(format(fec() - years(1L), "%b. %Y")))
      list(
        mes_vig    = paste0("Kilos ", mes_lbl),
        acum_vig   = paste0("Kilos ", acum_lbl),
        mes_ant    = paste0("Kilos ", mes_ant),
        acum_ant   = paste0("Kilos ", acum_ant),
        tot_ant    = paste0("Kilos Total ", anho - 1L),
        var_mes    = paste0("Var. Mes (", format(fec(), "%b"), ")"),
        var_acum   = "Var. Acum. Año",
        pct_avance = paste0("% Acum. / Total ", anho - 1L)
      )
    }
    
    .kilos_mun <- function(filas) {
      if (suc_r() != "TOTAL") filas <- filas %>% filter(Sucursal == suc_r())
      filas %>%
        group_by(NomDepPro, MunPro, LatPro, LngPro) %>%
        summarise(Kilos = sum(Kilos, na.rm = TRUE), .groups = "drop")
    }
    
    datos_mapa_doble <- reactive({
      req(suc_r())
      df <- det_mun %>% filter(Anho == year(fec()), Mes <= month(fec()))
      if (suc_r() != "TOTAL") df <- df %>% filter(Sucursal == suc_r())
      df
    })
    
    marcadores_r <- reactive({
      if (suc_r() == "TOTAL") GeoTrilladoras else GeoTrilladoras %>% filter(Sucursal == suc_r())
    })
    
    MapaDoble("mapa_det", datos = datos_mapa_doble, col_valor = "Kilos", escala = 1e6,
              titulo_dpto = "Kilos acumulados por Departamento",
              titulo_mun  = "Kilos acumulados por Municipio",
              footer_dpto = "Haga clic en un departamento para ver el detalle municipal.",
              marcadores  = marcadores_r)
    
    datos_comp <- reactive({
      req(suc_r())
      anho <- year(fec())
      mes  <- month(fec())
      lbl  <- .labels()
      
      mes_vig  <- det_mun %>% filter(Anho == anho,      Mes == mes)  %>% .kilos_mun()
      acum_vig <- det_mun %>% filter(Anho == anho,      Mes <= mes)  %>% .kilos_mun()
      mes_ant  <- det_mun %>% filter(Anho == anho - 1L, Mes == mes)  %>% .kilos_mun()
      acum_ant <- det_mun %>% filter(Anho == anho - 1L, Mes <= mes)  %>% .kilos_mun()
      tot_ant  <- det_mun %>% filter(Anho == anho - 1L)              %>% .kilos_mun()
      
      mes_vig %>%
        rename(!!lbl$mes_vig := Kilos) %>%
        full_join(.sel_kilo(acum_vig, lbl$acum_vig), by = "MunPro") %>%
        full_join(.sel_kilo(mes_ant,  lbl$mes_ant),  by = "MunPro") %>%
        full_join(.sel_kilo(acum_ant, lbl$acum_ant), by = "MunPro") %>%
        full_join(.sel_kilo(tot_ant,  lbl$tot_ant),  by = "MunPro") %>%
        mutate(
          !!lbl$var_mes    :=
            (.data[[lbl$mes_vig]]  - .data[[lbl$mes_ant]])  / .data[[lbl$mes_ant]],
          !!lbl$var_acum   :=
            (.data[[lbl$acum_vig]] - .data[[lbl$acum_ant]]) / .data[[lbl$acum_ant]],
          !!lbl$pct_avance := .data[[lbl$acum_vig]] / .data[[lbl$tot_ant]]
        ) %>%
        arrange(desc(coalesce(.data[[lbl$mes_vig]], 0)))
    })
    
    .especs <- function() {
      lbl <- .labels()
      
      .ck <- function(nm) colDef(name = nm, align = "right",
                                 format = colFormat(separators = TRUE, digits = 0L))
      
      .cv <- function(nm) colDef(
        name = nm, align = "right", format = colFormat(percent = TRUE, digits = 1L),
        style = function(value) {
          if (!is.finite(value)) return(list(color = "#bbb"))
          list(color = if (value >= 0) "#198754" else "#dc3545", fontWeight = "600")
        }
      )
      
      rlang::list2(
        MunPro    = colDef(name = "Municipio",    align = "left",
                           minWidth = 140L, style = list(fontWeight = "500")),
        NomDepPro = colDef(name = "Departamento", align = "left", minWidth = 140L),
        LatPro    = colDef(show = FALSE),
        LngPro    = colDef(show = FALSE),
        !!lbl$mes_vig    := .ck(lbl$mes_vig),
        !!lbl$acum_vig   := .ck(lbl$acum_vig),
        !!lbl$mes_ant    := .ck(lbl$mes_ant),
        !!lbl$acum_ant   := .ck(lbl$acum_ant),
        !!lbl$tot_ant    := .ck(lbl$tot_ant),
        !!lbl$var_mes    := .cv(lbl$var_mes),
        !!lbl$var_acum   := .cv(lbl$var_acum),
        !!lbl$pct_avance := .cv(lbl$pct_avance)
      )
    }
    
    TablaReactable("tbl_det", data = datos_comp, modo_seleccion = "ninguno",
                   col_header_n = 1L, sortable = TRUE, searchable = TRUE, compact = TRUE,
                   mostrar_nota = FALSE, mostrar_badge = FALSE, col_specs = .especs())
  })
}