# Helpers MapaDoble ----

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


# Constantes ----
.icono_trl <- awesomeIcons(markerColor = "#DA291C", iconColor = "white",
                           library = "fa", icon = "industry")


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
