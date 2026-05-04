# Módulo de detalle de proveedores ----
# Pareto acumulado + Treemap de participación + tabla comparativa.

DetalleProvUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, plotly::plotlyOutput(ns("grafico_pareto"),  height = "450px")),
      column(6, plotly::plotlyOutput(ns("grafico_treemap"), height = "450px"))
    ),
    br(),
    TablaReactableUI(ns("tbl_det"), mostrar_nota = FALSE, estilo = "minimal2")
  )
}

DetalleProv <- function(id, fec, det_prov, suc_r) {
  moduleServer(id, function(input, output, session) {
    
    # Labels explícitos de columna ----
    .labels <- function() {
      anho    <- year(fec())
      mes_lbl <- str_to_title(format(fec(), "%b. %Y"))
      mes_ant <- str_to_title(format(fec() - years(1L), "%b. %Y"))
      acum_lbl <- paste0("Acum. Ene–", str_to_title(format(fec(), "%b. %Y")))
      acum_ant <- paste0("Acum. Ene–", str_to_title(format(fec() - years(1L), "%b. %Y")))
      tot_ant  <- paste0("Total ", anho - 1L)
      list(mes_vig   = paste0("Kilos ", mes_lbl),
           acum_vig  = paste0("Kilos ", acum_lbl),
           mes_ant   = paste0("Kilos ", mes_ant),
           acum_ant  = paste0("Kilos ", acum_ant),
           tot_ant   = paste0("Kilos ", tot_ant),
           var_mes    = paste0("Var. Mes (", format(fec(), "%b"), ")"),
           var_acum   = "Var. Acum. Año",
           pct_avance = paste0("% Acum. / Total ", anho - 1L),
           pct_mes    = paste0("% Part. Mes ", format(fec(), "%b. %Y")),
           pct_acum   = paste0("% Part. Acum. Ene–", str_to_title(format(fec(), "%b. %Y"))))
    }
    
    # Corte de kilos por proveedor ----
    .kilos_prov <- function(filas) {
      if (suc_r() != "TOTAL") filas <- filas %>% filter(Sucursal == suc_r())
      filas %>%
        group_by(PerRazSoc) %>%
        summarise(Kilos = sum(Kilos, na.rm = TRUE),
                  Score = max(Score,  na.rm = TRUE),
                  .groups = "drop")
    }
    
    # Pareto de un período: ordena desc y calcula acumulado ----
    .pareto <- function(anho_f, mes_f, tipo = "acum") {
      filas <- if (tipo == "mes") {
        det_prov %>% filter(Anho == anho_f, Mes == mes_f)
      } else {
        det_prov %>% filter(Anho == anho_f, Mes <= mes_f)
      }
      filas %>%
        .kilos_prov() %>%
        arrange(desc(Kilos)) %>%
        mutate(N         = row_number(),
               KilosAcum = cumsum(Kilos),
               Acum      = KilosAcum / sum(Kilos))
    }
    
    # Dataset tabla ----
    datos <- reactive({
      req(suc_r())
      anho <- year(fec())
      mes  <- month(fec())
      lbl  <- .labels()
      
      mes_vig  <- det_prov %>% filter(Anho == anho,      Mes == mes)  %>% .kilos_prov()
      acum_vig <- det_prov %>% filter(Anho == anho,      Mes <= mes)  %>% .kilos_prov()
      mes_ant  <- det_prov %>% filter(Anho == anho - 1L, Mes == mes)  %>% .kilos_prov()
      acum_ant <- det_prov %>% filter(Anho == anho - 1L, Mes <= mes)  %>% .kilos_prov()
      tot_ant  <- det_prov %>% filter(Anho == anho - 1L)              %>% .kilos_prov()
      
      mes_vig %>%
        rename(!!lbl$mes_vig := Kilos, Score_Act = Score) %>%
        full_join(acum_vig %>% select(PerRazSoc, Kilos) %>%
                    rename(!!lbl$acum_vig := Kilos), by = "PerRazSoc") %>%
        full_join(mes_ant  %>% select(PerRazSoc, Kilos) %>%
                    rename(!!lbl$mes_ant  := Kilos), by = "PerRazSoc") %>%
        full_join(acum_ant %>% select(PerRazSoc, Kilos) %>%
                    rename(!!lbl$acum_ant := Kilos), by = "PerRazSoc") %>%
        full_join(tot_ant  %>% select(PerRazSoc, Kilos) %>%
                    rename(!!lbl$tot_ant  := Kilos), by = "PerRazSoc") %>%
        mutate(
          !!lbl$var_mes    := (.data[[lbl$mes_vig]]  - .data[[lbl$mes_ant]])  /
            .data[[lbl$mes_ant]],
          !!lbl$var_acum   := (.data[[lbl$acum_vig]] - .data[[lbl$acum_ant]]) /
            .data[[lbl$acum_ant]],
          !!lbl$pct_avance := .data[[lbl$acum_vig]] / .data[[lbl$tot_ant]],
          !!lbl$pct_mes    := .data[[lbl$mes_vig]]  / sum(.data[[lbl$mes_vig]],  na.rm = TRUE),
          !!lbl$pct_acum   := .data[[lbl$acum_vig]] / sum(.data[[lbl$acum_vig]], na.rm = TRUE),
          Score_Act = if_else(is.finite(Score_Act), Score_Act, NA_real_)
        ) %>%
        arrange(desc(coalesce(.data[[lbl$mes_vig]], 0))) %>%
        select(PerRazSoc, Score_Act,
               !!lbl$mes_vig, !!lbl$pct_mes, !!lbl$mes_ant, !!lbl$var_mes,
               !!lbl$acum_vig, !!lbl$pct_acum, !!lbl$acum_ant, !!lbl$var_acum,
               everything())
    })
    
    # Gráfico: Curva de Pareto ----
    output$grafico_pareto <- plotly::renderPlotly({
      req(suc_r())
      anho <- year(fec())
      mes  <- month(fec())
      mes_lbl  <- format(fec(), "%b. %Y")
      mes_lbl2 <- format(fec() - years(1L), "%b. %Y")
      
      acum_vig <- .pareto(anho,      mes, "acum")
      mes_vig  <- .pareto(anho,      mes, "mes")
      acum_ant <- .pareto(anho - 1L, mes, "acum")
      mes_ant  <- .pareto(anho - 1L, mes, "mes")
      
      n80 <- min(acum_vig$N[acum_vig$Acum >= 0.80], na.rm = TRUE)
      
      # Hover con Kilos Acumulados totales hasta ese punto
      hover_tpl <- paste0(
        "<b>%{x} proveedores</b><br>",
        "Part. Acum.: %{y:.1%}<br>",
        "Kilos Acum.: %{customdata:,.0f}<extra></extra>"
      )
      
      plotly::plot_ly() %>%
        plotly::add_lines(
          data = acum_vig, x = ~N, y = ~Acum, customdata = ~KilosAcum,
          name = paste0("Acum. Ene–", mes_lbl),
          line = list(color = "#DA291C", width = 2.5),
          hovertemplate = hover_tpl
        ) %>%
        plotly::add_lines(
          data = mes_vig, x = ~N, y = ~Acum, customdata = ~KilosAcum,
          name = paste0("Mes ", mes_lbl),
          line = list(color = "#DA291C", width = 1.5, dash = "dash"),
          hovertemplate = hover_tpl
        ) %>%
        plotly::add_lines(
          data = acum_ant, x = ~N, y = ~Acum, customdata = ~KilosAcum,
          name = paste0("Acum. Ene–", mes_lbl2),
          line = list(color = "#888", width = 2, dash = "dot"),
          hovertemplate = hover_tpl
        ) %>%
        plotly::add_lines(
          data = mes_ant, x = ~N, y = ~Acum, customdata = ~KilosAcum,
          name = paste0("Mes ", mes_lbl2),
          line = list(color = "#bbb", width = 1.5, dash = "dashdot"),
          hovertemplate = hover_tpl
        ) %>%
        plotly::add_segments(x = n80, xend = n80, y = 0,    yend = 0.80,
                             line = list(color = "#DA291C", dash = "dash", width = 1),
                             showlegend = FALSE) %>%
        plotly::add_segments(x = 0,   xend = n80, y = 0.80, yend = 0.80,
                             line = list(color = "#DA291C", dash = "dash", width = 1),
                             showlegend = FALSE) %>%
        plotly::add_annotations(
          x = n80, y = 0.80,
          text = paste0(n80, " prov. \u2192 80%"),
          showarrow = TRUE, arrowhead = 2, ax = 30, ay = -30,
          font = list(size = 11, color = "#DA291C", weight = 700)
        ) %>%
        plotly::layout(
          title  = list(text = "Curva de Pareto — Proveedores",
                        font = list(size = 13), x = 0),
          xaxis  = list(title = "Número de proveedores", zeroline = FALSE),
          yaxis  = list(title = "% Kilos acumulados",
                        tickformat = ".0%", range = c(0, 1.02)),
          legend = list(orientation = "h", y = -0.22, font = list(size = 10)),
          margin = list(t = 40, b = 70),
          paper_bgcolor = "white", plot_bgcolor = "white"
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Gráfico: Treemap ----
    output$grafico_treemap <- plotly::renderPlotly({
      req(suc_r())
      anho <- year(fec())
      mes  <- month(fec())
      
      df <- det_prov %>%
        filter(Anho == anho, Mes <= mes) %>%
        .kilos_prov() %>%
        filter(Kilos > 0) %>%
        mutate(Score = if_else(is.finite(Score), Score, NA_real_),
               Pct   = Kilos / sum(Kilos))
      
      plotly::plot_ly(
        data       = df,
        type       = "treemap",
        labels     = ~PerRazSoc,
        parents    = "",
        values     = ~Kilos,
        customdata = ~Score,
        textinfo   = "label+percent root",
        hovertemplate = paste0(
          "<b>%{label}</b><br>",
          "Kilos: %{value:,.0f}<br>",
          "Participación: %{percentRoot:.1%}<br>",
          "Score: %{customdata:.0f}<extra></extra>"
        ),
        marker = list(
          colors     = ~Score,
          colorscale = list(c(0, "#084081"), c(0.25, "#2b8cbe"), c(0.5, "#7bccc4"),
                            c(0.75, "#a8ddb5"), c(1, "#238b45")),
          showscale  = TRUE,
          colorbar   = list(title = list(text = "Score", font = list(size = 11)),
                            len = 0.5, thickness = 12)
        )
      ) %>%
        plotly::layout(
          title  = list(text = "Participación por Proveedor — Kilos Acumulados",
                        font = list(size = 13), x = 0),
          margin = list(t = 40)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    # Col specs tabla ----
    .especs <- function() {
      lbl <- .labels()
      .col_kilos <- function(nm) {
        colDef(name = nm, align = "right",
               format = colFormat(separators = TRUE, digits = 0L))
      }
      .col_var <- function(nm) {
        colDef(
          name   = nm, align = "right",
          format = colFormat(percent = TRUE, digits = 1L),
          style  = function(value) {
            if (!is.finite(value)) return(list(color = "#bbb"))
            list(color = if (value >= 0) "#198754" else "#dc3545", fontWeight = "600")
          }
        )
      }
      .col_pct <- function(nm) {
        colDef(name = nm, align = "right",
               format = colFormat(percent = TRUE, digits = 1L))
      }
      
      rlang::list2(
        PerRazSoc         = colDef(name = "Proveedor", align = "left", minWidth = 240L,
                                   style = list(fontWeight = "500")),
        Score_Act         = colDef(name = "Score", align = "right",
                                   format = colFormat(separators = TRUE, digits = 0L)),
        !!lbl$mes_vig    := .col_kilos(lbl$mes_vig),
        !!lbl$pct_mes    := .col_pct(lbl$pct_mes),
        !!lbl$mes_ant    := .col_kilos(lbl$mes_ant),
        !!lbl$var_mes    := .col_var(lbl$var_mes),
        !!lbl$acum_vig   := .col_kilos(lbl$acum_vig),
        !!lbl$pct_acum   := .col_pct(lbl$pct_acum),
        !!lbl$acum_ant   := .col_kilos(lbl$acum_ant),
        !!lbl$var_acum   := .col_var(lbl$var_acum),
        !!lbl$tot_ant    := colDef(show = FALSE),
        !!lbl$pct_avance := colDef(show = FALSE)
      )
    }
    
    TablaReactable("tbl_det", data = datos,
                   modo_seleccion = "ninguno", col_header_n = 1L,
                   sortable = TRUE, searchable = TRUE,
                   compact = TRUE, mostrar_nota = FALSE, mostrar_badge = FALSE,
                   col_specs = .especs())
  })
}