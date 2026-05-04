# DetalleMun ----
.sel_kilo <- function(df, nm) df %>% select(MunPro, Kilos) %>% rename(!!nm := Kilos)

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
