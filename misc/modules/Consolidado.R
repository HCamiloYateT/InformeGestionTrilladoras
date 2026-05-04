# Funciones y Helpers ----

# Selecciona ventana temporal, filtra dimensiones opcionales, agrega por Sucursal + TOTAL y pivota.
PrepMetrica <- function(lis, acumulado, sucursal, seccion = NULL, subseccion = NULL,
                        detalle = NULL, item = NULL, fun = sum, label_item = NULL) {
  bd <- if (acumulado) "acum_vig" else "vig"
  df <- lis[[bd]] %>%
    filter(
      if (!is.null(seccion))    Seccion    %in% seccion    else TRUE,
      if (!is.null(subseccion)) Subseccion %in% subseccion else TRUE,
      if (!is.null(detalle))    Detalle    %in% detalle    else TRUE,
      if (!is.null(item))       Item       %in% item       else TRUE
    ) %>%
    filter(Sucursal != "TOTAL") %>%   # excluye TOTAL pre-computado del ETL
    select(Sucursal, Item, Valor)
  
  resultado <- if (!sucursal) {
    df %>%
      group_by(Item) %>%
      summarise(TOTAL = fun(Valor, na.rm = TRUE), .groups = "drop") %>%
      arrange(Item)
  } else {
    niveles_suc <- c(levels(df$Sucursal) %>% setdiff("TOTAL"), "TOTAL")
    df %>%
      mutate(Sucursal = as.character(Sucursal)) %>%
      bind_rows(mutate(., Sucursal = "TOTAL")) %>%
      group_by(Sucursal, Item) %>%
      summarise(Valor = fun(Valor, na.rm = TRUE), .groups = "drop") %>%
      mutate(Sucursal = factor(Sucursal, levels = niveles_suc)) %>%
      arrange(Sucursal) %>%
      pivot_wider(names_from = Sucursal, values_from = Valor,
                  values_fill = 0) %>%
      arrange(Item)
  }
  if (!is.null(label_item)) mutate(resultado, Item = label_item) else resultado
}

# Agrega fila de cociente entre grupos de ítems; item_num e item_den aceptan vectores.
AgregarCociente <- function(df, item_num, item_den, label = "CUMPLIMIENTO %", col_item = "Item") {
  col_item <- sym(col_item)
  num <- df %>% filter(!!col_item %in% item_num) %>% select(where(is.numeric)) %>% colSums(na.rm = TRUE)
  den <- df %>% filter(!!col_item %in% item_den) %>% select(where(is.numeric)) %>% colSums(na.rm = TRUE)
  bind_rows(df, as_tibble_row(num / den) %>% mutate(!!col_item := label))
}


# UI del Módulo ----
ConsolidadoUI <- function(id) {
  ns <- NS(id)
  
  .card <- function(card_id, titulo, ...) {
    bs4Dash::bs4Card(id = ns(card_id), title = titulo, status = "danger",
                     solidHeader = FALSE, collapsible = TRUE, collapsed = FALSE,
                     width = 12, ...)
  }
  
  tagList(
    fluidRow(column(12, uiOutput(ns("titulo_analisis")))),
    br(),
    fluidRow(
      style = "display:flex; align-items:center;",
      column(2,
             racafeShiny::BotonesRadiales(inputId = ns("modo_periodo"),
                                          choices = list("Acumulado" = "acumulado", "Mensual" = "mensual"),
                                          selected = "acumulado", alineacion = "left",
                                          color_activo = "#DA291C", color_inactivo = "#FFF")
      ),
      column(2,
             racafeShiny::BotonesRadiales(inputId = ns("modo_agregacion"),
                                          choices = list("Sucursal" = "sucursal", "Total" = "total"),
                                          selected = "sucursal", alineacion = "left",
                                          color_activo = "#DA291C", color_inactivo = "#FFF")
      ),
      column(8,
             div(style = "display:flex; justify-content:flex-end;",
                 racafeShiny::Boton(id = ns("btn_expandir"), label = "Contraer",
                                    icono = "compress", size = "sm",
                                    color_fondo = "#6c757d", color_hover = "#DA291C"))
      )
    ),
    br(),
    
    # Cuerpo ----
    .card("CONS_Compras", "Compras",
          fluidRow(column(12, TablaReactableUI(ns("tbl_Compras"),
                                               mostrar_nota = TRUE, footer = "Compras en kilos", estilo = "minimal2")))
    ),
    .card("CONS_Entradas", "Entradas",
          fluidRow(column(12, TablaReactableUI(ns("tbl_Entradas"),
                                               mostrar_nota = TRUE, footer = "Entradas en kilos", estilo = "minimal2"))),
          br(),
          FormatearTexto("Indicadores de Proveedores", tamano_pct = 1.1),
          fluidRow(column(12, TablaReactableUI(ns("tbl_ProveedoresInd"),
                                               mostrar_nota = TRUE,
                                               footer = "Al seleccionar una sucursal se despliega el detalle de proveedores.",
                                               estilo = "minimal2"))),
          br(),
          FormatearTexto("Indicadores de Municipios", tamano_pct = 1.1),
          fluidRow(column(12, TablaReactableUI(ns("tbl_MunicipiosInd"),
                                               mostrar_nota = TRUE,
                                               footer = paste("Al seleccionar una sucursal se despliega el detalle de municipios.",
                                                              "La renderización del mapa puede tomar unos segundos."),
                                               estilo = "minimal2")))
    ),
    .card("CONS_Produccion", "Producción",
          fluidRow(column(12, TablaReactableUI(ns("tbl_Produccion"),
                                               mostrar_nota = TRUE, footer = "Producción en sacos de 70 Kgs", estilo = "minimal2"))),
          br(),
          FormatearTexto("Diferenciados sobre Total Excelsos", tamano_pct = 1.1),
          uiOutput(ns("ui_produccion_ind"))
    ),
    .card("CONS_Despachos", "Despachos",
          fluidRow(column(12, TablaReactableUI(ns("tbl_Despachos"),
                                               mostrar_nota = TRUE, footer = "Despachos en sacos de 70 Kgs", estilo = "minimal2"))),
          br(),
          FormatearTexto("Diferenciados sobre Total Excelsos", tamano_pct = 1.1),
          uiOutput(ns("ui_despachos_dif")),
          br(),
          FormatearTexto("Preparaciones Superiores", tamano_pct = 1.1),
          uiOutput(ns("ui_despachos_ind"))
    ),
    .card("CONS_Rechazos", "Rechazos",
          fluidRow(column(12, TablaReactableUI(ns("tbl_Rechazos"),
                                               mostrar_nota = TRUE, footer = "Rechazos en sacos de 70 Kgs", estilo = "minimal2"))),
          fluidRow(column(12, TablaReactableUI(ns("tbl_RechazosInd"),
                                               mostrar_nota = TRUE,
                                               footer = "% sacos rechazados / total despachados. Meta: ≤ 0.5% (verde) · ≤ 2.0% (amarillo) · > 2.0% (rojo).",
                                               estilo = "minimal2")))
    ),
    .card("CONS_Reclamaciones", "Reclamaciones",
          fluidRow(column(12, TablaReactableUI(ns("tbl_Reclamos"),
                                               mostrar_nota = TRUE, footer = "Reclamos en sacos de 70 Kgs", estilo = "minimal2"))),
          fluidRow(column(12, TablaReactableUI(ns("tbl_ReclamosInd"),
                                               mostrar_nota = TRUE,
                                               footer = "% sacos con reclamo / total despachados.",
                                               estilo = "minimal2")))
    ),
    .card("CONS_Ingresos", "Ingresos",
          fluidRow(column(12, TablaReactableUI(ns("tbl_Ingreso"),
                                               mostrar_nota = TRUE,
                                               footer = "Valor en miles de $COP. Calculado como el valor estándar por saco despachado",
                                               estilo = "minimal2")))
    ),
    .card("CONS_CostosFijos", "Costos Fijos (Miles de $COP)",
          fluidRow(column(12, TablaReactableUI(ns("tbl_CostosFijos"),
                                               mostrar_nota = TRUE, footer = "Valor en miles de $COP.", estilo = "minimal2"))),
          br(),
          FormatearTexto("Ejecución de Presupuesto", tamano_pct = 1.1),
          fluidRow(column(12, TablaReactableUI(ns("tbl_CostosFijosInd"),
                                               mostrar_nota = TRUE,
                                               footer = "Porcentaje calculado respecto al presupuesto aprobado",
                                               estilo = "minimal2")))
    ),
    .card("CONS_CostosVariables", "Costos Variables",
          fluidRow(column(12, TablaReactableUI(ns("tbl_CostosVariables"),
                                               mostrar_nota = TRUE,
                                               footer = paste(
                                                 "Energía en KW; costos en miles de $COP; KW/saco y movilización/saco en valores unitarios.",
                                                 "Ratios calculados sobre producción total del período."
                                               ),
                                               estilo = "minimal2"))),
          uiOutput(ns("nota_meta_cv"))
    ),
    .card("CONS_UtilidadOperacional", "Utilidad Operacional (Miles de $COP)",
          fluidRow(column(12, TablaReactableUI(ns("tbl_UtOperacional"),
                                               mostrar_nota = TRUE, footer = "Valor en miles de $COP.", estilo = "minimal2")))
    ),
    .card("CONS_OtrosIngresos", "Otros Ingresos (Miles de $COP)",
          fluidRow(column(12, TablaReactableUI(ns("tbl_OtrosIngresos"),
                                               mostrar_nota = TRUE, footer = "Valor en miles de $COP.", estilo = "minimal2")))
    ),
    .card("CONS_OtrosCostos", "Otros Costos (Miles de $COP)",
          fluidRow(column(12, TablaReactableUI(ns("tbl_OtrosCostos"),
                                               mostrar_nota = TRUE, footer = "Valor en miles de $COP.", estilo = "minimal2")))
    ),
    .card("CONS_CostoPuntoCompra", "Costos Punto de Compra (Miles de $COP)",
          fluidRow(column(12, TablaReactableUI(ns("tbl_PuntoCompra"),
                                               mostrar_nota = TRUE, footer = "Valor en miles de $COP.", estilo = "minimal2")))
    ),
    .card("CONS_UtilidadCorte", "Utilidad Antes del Corte (Miles de $COP)",
          fluidRow(column(12, TablaReactableUI(ns("tbl_UtCorte"),
                                               mostrar_nota = TRUE, footer = "Valor en miles de $COP.", estilo = "minimal2")))
    ),
    .card("CONS_ResultadoCorte", "Resultado del Corte (Miles de $COP)",
          fluidRow(column(12, TablaReactableUI(ns("tbl_Cortes"),
                                               mostrar_nota = TRUE, footer = "Valor en miles de $COP.", estilo = "minimal2")))
    ),
    .card("CONS_UtilidadNeta", "Utilidad Neta (Miles de $COP)",
          fluidRow(column(12, TablaReactableUI(ns("tbl_UtNeta"),
                                               mostrar_nota = TRUE, footer = "Valor en miles de $COP.", estilo = "minimal2")))
    ),
    .card("CONS_TalentoHumano", "Indicadores de Talento Humano",
          fluidRow(column(12, TablaReactableUI(ns("tbl_TalentoHumano"),
                                               mostrar_nota = TRUE, estilo = "minimal2")))
    )
  )
}


# Server del Módulo ----
# desp_sup: objeto DespachoSuperiores exportado por el ETL (Sucursal, Fecha, Numerador, Denominador)
Consolidado <- function(id, fec, dat, ind_ent, det_prov, det_mun,
                        desp_sup = DespachoSuperiores) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Constantes ----
    IDS_CARDS <- c("CONS_Compras", "CONS_Entradas", "CONS_Produccion",
                   "CONS_Despachos", "CONS_Rechazos", "CONS_Reclamaciones",
                   "CONS_Ingresos", "CONS_CostosFijos", "CONS_CostosVariables",
                   "CONS_UtilidadOperacional", "CONS_OtrosIngresos",
                   "CONS_OtrosCostos", "CONS_CostoPuntoCompra",
                   "CONS_UtilidadCorte", "CONS_ResultadoCorte",
                   "CONS_UtilidadNeta", "CONS_TalentoHumano")
    ITEMS_PROV_IND <- c("NÚMERO DE PROVEEDORES", "PARETO DE PROVEEDORES",
                        "SCORE DE CUMPLIMIENTO PONDERADO")
    ITEMS_MUN_IND  <- c("NÚMERO DE MUNICIPIOS", "PARETO DE MUNICIPIOS")
    
    # Meta dinámica de preparaciones superiores desde Analitica (carga estática, no reactiva)
    meta_superiores <- tryCatch(
      CargarDatos("ANMETASUPREMOS") %>%
        filter(Fecha == max(Fecha), Item == "PORCENTAJE DE SUPREMOS",
               Sucursal == "TOTAL") %>%
        pull(Valor),
      error = function(e) NA_real_
    )
    
    # Niveles canónicos de producto (igual que NIVELES_PRODUCTOS del ETL)
    NIVELES_PRODUCTOS <- c("EXCELSOS", "SUPREMOS, DUNKIN Y MILLSTONE",
                           "PRODUCTO DE COLOMBIA", "PRACTICES", "4C Y CRECER",
                           "CERTIFICADOS", "REGIONALES", "PERFIL DE TAZA", "FINCA",
                           "A LA MEDIDA", "CONSUMOS Y PASILLAS SACOS DE 70KG",
                           "TRILLAS PARA TERCEROS", "TOTAL")
    
    # Niveles de sucursales (igual que NIVELES_SUCS del ETL)
    NIVELES_SUCS <- c("BACHUÉ", "MEDELLÍN", "POPAYÁN", "ARMENIA",
                      "PEREIRA", "BUCARAMANGA", "HUILA", "TOTAL")
    
    # Numerador y denominador del indicador de diferenciados / excelsos
    ITEMS_SUP_NUM <- c("SUPREMOS, DUNKIN Y MILLSTONE", "PRODUCTO DE COLOMBIA",
                       "PRACTICES", "4C Y CRECER", "CERTIFICADOS", "REGIONALES",
                       "PERFIL DE TAZA", "FINCA", "A LA MEDIDA")
    ITEMS_SUP_DEN <- c("EXCELSOS", "SUPREMOS, DUNKIN Y MILLSTONE",
                       "PRODUCTO DE COLOMBIA", "PRACTICES", "4C Y CRECER",
                       "CERTIFICADOS", "REGIONALES", "PERFIL DE TAZA",
                       "FINCA", "A LA MEDIDA")
    
    # Niveles de ítems de costos fijos Trilladoras (igual que NIVELES_CF_ITEM del ETL)
    NIVELES_CF_ITEM <- c("MANO DE OBRA DIRECTA", "MANTENIMIENTO SUCURSALES",
                         "FUNCIONAMIENTO", "VIGILANCIA", "PREDIAL Y ALUMBRADO",
                         "SERVICIOS PÚBLICOS", "SEGUROS SUCURSALES",
                         "COSTOS DE VIAJE SUCURSALES", "COSTOS LEGALES SUCURSALES",
                         "TOTAL")
    
    # Orden canónico de ítems de costos variables (display)
    NIVELES_CV <- c("ENERGÍA (KW)", "COSTO DE ENERGÍA (MILES $COP)", "KW POR SACO",
                    "NÚMERO DE MOVILIZADORES", "COSTO DE MOVILIZACIÓN (MILES $COP)",
                    "COSTO DE MOVILIZACIÓN POR SACO ($COP)")
    
    # Metas de KW/saco y costo de movilización/saco — TOTAL sucursal como umbral global
    meta_kw_total  <- tryCatch(metas_kw_mov %>% filter(Sucursal == "TOTAL") %>%
                                 pull(`KW POR SACO`),
                               error = function(e) NA_real_)
    meta_mov_total <- tryCatch(metas_kw_mov %>% filter(Sucursal == "TOTAL") %>%
                                 pull(`COSTO DE MOVILIZACIÓN POR SACO`),
                               error = function(e) NA_real_)
    
    # Especificaciones de columna compartidas ----
    especs <- list(
      Item        = colDef(name = "", align = "left", minWidth = 300L,
                           style = list(fontWeight = "bold", textTransform = "uppercase")),
      ARMENIA     = DefinirColumnaHtml("ARMENIA"),
      BACHUÉ      = DefinirColumnaHtml("BACHUÉ"),
      BUCARAMANGA = DefinirColumnaHtml("BUCARAMANGA"),
      HUILA       = DefinirColumnaHtml("HUILA"),
      MEDELLÍN    = DefinirColumnaHtml("MEDELLÍN"),
      PEREIRA     = DefinirColumnaHtml("PEREIRA"),
      POPAYÁN     = DefinirColumnaHtml("POPAYÁN"),
      TOTAL       = colDef(name = "TOTAL", align = "right", html = TRUE,
                           style = list(fontWeight = "bold", background = "#f8f9fa"),
                           headerStyle = list(fontWeight = "bold"))
    )
    
    # Reglas de formato ----
    reglas_compras <- list(
      "COMPRAS REALIZADAS"     = list(formato = "coma"),
      "PRESUPUESTO DE COMPRAS" = list(formato = "coma"),
      "CUMPLIMIENTO %"         = list(formato = "porcentaje", negrita = TRUE, meta = 1)
    )
    reglas_prov_ind <- list(
      "NÚMERO DE PROVEEDORES"           = list(formato = "coma"),
      "PARETO DE PROVEEDORES"           = list(formato = "coma"),
      "SCORE DE CUMPLIMIENTO PONDERADO" = list(formato = "coma")
    )
    reglas_mun_ind  <- list(
      "NÚMERO DE MUNICIPIOS" = list(formato = "coma"),
      "PARETO DE MUNICIPIOS" = list(formato = "coma")
    )
    # meta_superiores: mayor es mejor → inverso = FALSE
    reglas_despachos_ind <- list(
      "% PREPARACIONES SUPERIORES" = list(formato = "porcentaje", negrita = TRUE,
                                          meta = meta_superiores, inverso = FALSE)
    )
    reglas_diferenciados <- list(
      "DIFERENCIADOS SOBRE TOTAL EXCELSOS" = list(formato = "porcentaje", negrita = TRUE)
    )
    # Rechazos: mayor porcentaje es peor → valor > meta = rojo (inverso = TRUE)
    reglas_rechazos_ind <- list(
      "% RECHAZOS SOBRE DESPACHOS" = list(formato = "porcentaje", negrita = TRUE,
                                          meta = 0.005, inverso = TRUE)
    )
    # Fix 4: reclamos sin escala de color
    reglas_reclamos_ind <- list(
      "% RECLAMOS SOBRE DESPACHOS" = list(formato = "porcentaje", negrita = TRUE)
    )
    
    # Factory de reglas para tablas de detalle por producto.
    .reglas_prod <- function(fmt) c(
      setNames(rep(list(list(formato = fmt)), length(NIVELES_PRODUCTOS) - 1L),
               head(NIVELES_PRODUCTOS, -1L)),
      list("TOTAL" = list(formato = fmt, negrita = TRUE))
    )
    reglas_prod_des     <- .reglas_prod("coma")
    reglas_ingresos     <- .reglas_prod("miles0")
    reglas_costos_fijos <- c(
      setNames(rep(list(list(formato = "miles0")), length(NIVELES_CF_ITEM) - 1L),
               head(NIVELES_CF_ITEM, -1L)),
      list("TOTAL" = list(formato = "miles0", negrita = TRUE))
    )
    # Fix 6: KW/saco y Mov/saco → valor < meta = verde → inverso = FALSE
    reglas_costos_variables <- list(
      "ENERGÍA (KW)"                          = list(formato = "coma"),
      "COSTO DE ENERGÍA (MILES $COP)"         = list(formato = "miles0"),
      "KW POR SACO"                           = list(formato = "decimal", negrita = TRUE,
                                                     meta = meta_kw_total,  inverso = FALSE),
      "NÚMERO DE MOVILIZADORES"               = list(formato = "coma"),
      "COSTO DE MOVILIZACIÓN (MILES $COP)"    = list(formato = "miles0"),
      "COSTO DE MOVILIZACIÓN POR SACO ($COP)" = list(formato = "decimal", negrita = TRUE,
                                                     meta = meta_mov_total, inverso = FALSE)
    )
    # Utilidades en miles0 sin decimales; escala verde ≥ 0 / roja < 0
    .regla_ut <- function(item) setNames(list(list(formato = "miles0", meta = 0, inverso = FALSE)), item)
    
    ## Factory para tablas de utilidad — redondea a enteros antes de formatear ----
    .tabla_ut_rv <- function(seccion, subseccion, label) reactive({
      req(nrow(dat()$vig) > 0)
      dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = seccion,
                    subseccion = subseccion, item = "TOTAL", label_item = label) %>%
        mutate(across(where(is.numeric), ~round(.x, 0L))) %>%
        FormatearFila(reglas = .regla_ut(label), col_item = "Item")
    })
    
    # Helpers locales ----
    
    ## TablaReactable con defaults del módulo ----
    .tabla <- function(id, data) {
      TablaReactable(id, data = data, modo_seleccion = "ninguno",
                     col_header_n = 1L, sortable = FALSE, searchable = FALSE,
                     compact = TRUE, mostrar_nota = TRUE, mostrar_badge = TRUE,
                     col_specs = especs)
    }
    
    ## Regla formato coma para un solo ítem ----
    .regla_coma <- function(item) setNames(list(list(formato = "coma")), item)
    
    ## Columna clickeable: dispara input${input_id} con el nombre de sucursal ----
    .col_clic <- function(suc, input_id, extra = list()) {
      id_input <- ns(input_id)
      do.call(colDef, modifyList(list(
        name = suc, align = "right", html = TRUE,
        cell = function(value) {
          sprintf('<span onclick="Shiny.setInputValue(\'%s\',\'%s\',{priority:\'event\'})"
                     style="cursor:pointer;display:block;">%s</span>',
                  id_input, suc, value)
        }
      ), extra))
    }
    
    ## Especs con clicks para tablas de indicadores ----
    .especs_clic <- function(input_id) {
      ext <- list(style = list(fontWeight = "bold", background = "#f8f9fa"))
      list(
        Item        = colDef(name = "", align = "left", minWidth = 300L,
                             style = list(fontWeight = "bold", textTransform = "uppercase")),
        ARMENIA     = .col_clic("ARMENIA",     input_id),
        BACHUÉ      = .col_clic("BACHUÉ",      input_id),
        BUCARAMANGA = .col_clic("BUCARAMANGA", input_id),
        HUILA       = .col_clic("HUILA",       input_id),
        MEDELLÍN    = .col_clic("MEDELLÍN",    input_id),
        PEREIRA     = .col_clic("PEREIRA",     input_id),
        POPAYÁN     = .col_clic("POPAYÁN",     input_id),
        TOTAL       = .col_clic("TOTAL",       input_id, extra = ext)
      )
    }
    
    ## Extrae y pivota indicadores precalculados del ETL (TOTAL no es aditivo) ----
    .prep_ind <- function(items) {
      df <- (if (es_acumulado()) ind_ent$acum else ind_ent$vig) %>%
        filter(Item %in% items) %>% select(Sucursal, Item, Valor)
      if (!es_sucursal()) {
        df %>% filter(Sucursal == "TOTAL") %>% select(Item, TOTAL = Valor)
      } else {
        df %>% pivot_wider(names_from = Sucursal, values_from = Valor)
      }
    }
    
    ## Ratio entre TOTAL de dos secciones del ETL (num/den) ----
    ## Sustituye NaN e Inf por 0 para sucursales sin despachos en el período.
    .prep_ratio <- function(sec_num, subsec_num, sec_den, subsec_den, label) {
      f_num <- dat() %>% PrepMetrica(es_acumulado(), es_sucursal(), seccion = sec_num,
                                     subseccion = subsec_num, item = "TOTAL") %>% select(where(is.numeric))
      f_den <- dat() %>% PrepMetrica(es_acumulado(), es_sucursal(), seccion = sec_den,
                                     subseccion = subsec_den, item = "TOTAL") %>% select(where(is.numeric))
      (f_num / f_den) %>%
        mutate(across(everything(), ~ifelse(is.nan(.x) | is.infinite(.x), 0, .x))) %>%
        mutate(Item = label) %>% relocate(Item)
    }
    
    ## Indicador de preparaciones superiores desde DespachoSuperiores (ETL) ----
    ## Acumulado: suma Num/Den de todos los meses del año hasta fec();
    ## Mensual: solo el mes de fec(). TOTAL = cociente de sumas globales (no media de ratios).
    .prep_despachos_ind <- function() {
      meses <- if (es_acumulado()) seq_len(month(fec())) else month(fec())
      df <- desp_sup %>%
        filter(year(Fecha) == year(fec()), month(Fecha) %in% meses) %>%
        group_by(Sucursal) %>%
        summarise(Num = sum(Numerador), Den = sum(Denominador), .groups = "drop")
      base <- bind_rows(
        df,
        df %>% summarise(Num = sum(Num), Den = sum(Den)) %>% mutate(Sucursal = "TOTAL")
      ) %>%
        mutate(Item = "% PREPARACIONES SUPERIORES", Indicador = Num / Den) %>%
        select(Sucursal, Item, Indicador)
      if (!es_sucursal()) {
        base %>% filter(Sucursal == "TOTAL") %>% select(Item, TOTAL = Indicador)
      } else {
        base %>%
          mutate(Sucursal = factor(Sucursal, levels = NIVELES_SUCS, ordered = TRUE)) %>%
          arrange(Sucursal) %>%
          pivot_wider(names_from = Sucursal, values_from = Indicador, values_fill = 0)
      }
    }
    
    ## Factory de reactivos por producto: respeta NIVELES_PRODUCTOS (factor del ETL) ----
    .prep_prod <- function(seccion, subseccion, reglas) reactive({
      req(nrow(dat()$vig) > 0)
      dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = seccion,
                    subseccion = subseccion, item = NIVELES_PRODUCTOS) %>%
        mutate(Item = factor(Item, levels = NIVELES_PRODUCTOS, ordered = TRUE)) %>%
        arrange(Item) %>%
        mutate(Item = as.character(Item)) %>%   # FormatearFila requiere character
        FormatearFila(reglas = reglas, col_item = "Item")
    })
    
    ## Indicador de diferenciados sobre total de excelsos (desde dat(), para Prod y Desp) ----
    ## Calcula AgregarCociente sobre el detalle por producto ya agregado en dat().
    .prep_diferenciados <- function(seccion, subseccion) reactive({
      req(nrow(dat()$vig) > 0)
      dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = seccion,
                    subseccion = subseccion, item = NIVELES_PRODUCTOS) %>%
        mutate(Item = as.character(Item)) %>%
        AgregarCociente(item_num = ITEMS_SUP_NUM, item_den = ITEMS_SUP_DEN,
                        label = "DIFERENCIADOS SOBRE TOTAL EXCELSOS") %>%
        filter(Item == "DIFERENCIADOS SOBRE TOTAL EXCELSOS") %>%
        FormatearFila(reglas = reglas_diferenciados, col_item = "Item")
    })
    
    ## Label de período para títulos de modal ----
    .label_periodo <- function() {
      if (es_acumulado()) paste0("Acum. ", year(fec())) else
        str_to_title(format(fec(), "%B %Y"))
    }
    
    # renderUI con footer dinámico para tablas cuya meta viene del server
    .footer_dif <- function() {
      meta_txt <- if (!is.na(meta_superiores))
        paste0("Meta: ≥ ", round(meta_superiores * 100, 1), "%")
      else "Meta: según configuración vigente"
      paste(
        "Numerador: Supremos/Dunkin/Millstone, Producto de Colombia, Practices,",
        "4C y Crecer, Certificados, Regionales, Perfil de Taza, Finca, A la Medida.",
        "Denominador: Excelsos + Numerador.", meta_txt
      )
    }
    .footer_sup <- function() {
      meta_txt <- if (!is.na(meta_superiores))
        paste0("Meta: ≥ ", round(meta_superiores * 100, 1), "%")
      else "Meta: según configuración vigente"
      paste("Sacos de categorías superiores sobre el total de excelsos y superiores.", meta_txt)
    }
    output$ui_produccion_ind <- renderUI(
      fluidRow(column(12, TablaReactableUI(ns("tbl_ProduccionInd"),
                                           mostrar_nota = TRUE, footer = .footer_dif(), estilo = "minimal2")))
    )
    output$ui_despachos_dif <- renderUI(
      fluidRow(column(12, TablaReactableUI(ns("tbl_DespachosDif"),
                                           mostrar_nota = TRUE, footer = .footer_dif(), estilo = "minimal2")))
    )
    output$ui_despachos_ind <- renderUI(
      fluidRow(column(12, TablaReactableUI(ns("tbl_DespachosInd"),
                                           mostrar_nota = TRUE, footer = .footer_sup(), estilo = "minimal2")))
    )
    
    # Párrafo resumen de metas por sucursal para costos variables — agrupa sucursales con igual meta
    output$nota_meta_cv <- renderUI({
      tbl <- tryCatch(
        metas_kw_mov %>%
          filter(Sucursal %in% setdiff(NIVELES_SUCS, "TOTAL")) %>%
          mutate(Sucursal = factor(Sucursal, levels = NIVELES_SUCS, ordered = TRUE)) %>%
          arrange(Sucursal),
        error = function(e) NULL
      )
      if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
      
      total_row <- tryCatch(
        metas_kw_mov %>% filter(Sucursal == "TOTAL"),
        error = function(e) NULL
      )
      
      # Agrupa sucursales con idéntica meta KW y Mov en un solo token
      resumen_kw <- tbl %>%
        group_by(kw = round(`KW POR SACO`, 2)) %>%
        summarise(sucs = paste(Sucursal, collapse = "/"), .groups = "drop") %>%
        mutate(txt = paste0(sucs, ": ≤", kw)) %>% pull(txt)
      
      resumen_mov <- tbl %>%
        group_by(mov = round(`COSTO DE MOVILIZACIÓN POR SACO`)) %>%
        summarise(sucs = paste(Sucursal, collapse = "/"), .groups = "drop") %>%
        mutate(txt = paste0(sucs, ": ≤$", format(mov, big.mark = ","))) %>% pull(txt)
      
      total_txt <- if (!is.null(total_row) && nrow(total_row) > 0) {
        paste0("Total — KW/saco: ≤", round(total_row$`KW POR SACO`, 2),
               " · Mov/saco: ≤$", format(round(total_row$`COSTO DE MOVILIZACIÓN POR SACO`), big.mark = ","))
      } else NULL
      
      lineas <- c(
        paste0("KW/saco — ", paste(resumen_kw, collapse = " · ")),
        paste0("Mov/saco ($COP) — ", paste(resumen_mov, collapse = " · ")),
        total_txt
      )
      tags$p(style = "font-size:0.75rem; color:#6c757d; margin:4px 0 0 0; line-height:1.6;",
             HTML(paste(lineas[!is.null(lineas)], collapse = "<br/>")))
    })
    
    # Reactivos booleanos ----
    es_acumulado <- reactive(input$modo_periodo    == "acumulado")
    es_sucursal  <- reactive(input$modo_agregacion == "sucursal")
    
    # Control de expansión/colapso de tarjetas ----
    expandido <- reactiveVal(TRUE)
    observeEvent(input$btn_expandir, {
      colapsar <- expandido()
      lapply(IDS_CARDS, function(card_id)
        bs4Dash::updatebs4Card(id = card_id, action = "toggle", session = session))
      updateActionButton(session, "btn_expandir",
                         label = if (colapsar) "Expandir"  else "Contraer",
                         icon  = icon(if (colapsar) "expand" else "compress"))
      expandido(!colapsar)
    })
    
    # Outputs ----
    
    ## Título reactivo ----
    output$titulo_analisis <- renderUI({
      tags$div(
        tags$h4(style = "margin: 0; color: #000; font-weight: 600;",
                paste("Informe de Gestión de Trilladoras —",
                      str_to_title(format(fec(), "%B de %Y")))),
        tags$span(style = "font-size: 0.85rem; color: #555; font-weight: 400;",
                  paste(if (es_acumulado()) "Acumulado" else "Mensual", "·",
                        if (es_sucursal()) "por Sucursal" else "Total"))
      )
    })
    
    ## Compras ----
    datos_tbl_compras <- reactive({
      req(nrow(dat()$vig) > 0)
      dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = "COMPRAS",
                    detalle = "TOTAL",
                    item = c("COMPRAS REALIZADAS", "PRESUPUESTO DE COMPRAS")) %>%
        AgregarCociente("COMPRAS REALIZADAS", "PRESUPUESTO DE COMPRAS",
                        label = "CUMPLIMIENTO %") %>%
        FormatearFila(reglas = reglas_compras, col_item = "Item")
    })
    .tabla("tbl_Compras", data = datos_tbl_compras)
    
    ## Entradas ----
    datos_tbl_entradas <- reactive({
      req(nrow(dat()$vig) > 0)
      dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = "ENTRADAS",
                    subseccion = "ENTRADAS", item = "TOTAL", label_item = "ENTRADAS") %>%
        FormatearFila(reglas = .regla_coma("ENTRADAS"), col_item = "Item")
    })
    .tabla("tbl_Entradas", data = datos_tbl_entradas)
    
    ## Indicadores de Proveedores ----
    datos_tbl_prov_ind <- reactive({
      req(nrow(dat()$vig) > 0)
      .prep_ind(ITEMS_PROV_IND) %>%
        FormatearFila(reglas = reglas_prov_ind, col_item = "Item")
    })
    TablaReactable("tbl_ProveedoresInd", data = datos_tbl_prov_ind,
                   modo_seleccion = "ninguno", col_header_n = 1L,
                   sortable = FALSE, searchable = FALSE, compact = TRUE,
                   mostrar_nota = TRUE, mostrar_badge = TRUE,
                   col_specs = .especs_clic("click_suc_prov"))
    
    # Registro anticipado — debe ocurrir antes del primer click
    suc_prov_sel <- reactiveVal(NULL)
    DetalleProv("det_prov", fec = fec, det_prov = det_prov, suc_r = suc_prov_sel)
    observeEvent(input$click_suc_prov, {
      suc_prov_sel(input$click_suc_prov)
      showModal(modalDialog(
        title = tags$div(
          tags$span(style = "font-weight:600;",
                    paste("Detalle de Proveedores —", input$click_suc_prov)),
          tags$span(style = "font-size:0.82rem; color:#888; margin-left:10px;",
                    .label_periodo())
        ),
        size = "xl", easyClose = TRUE, footer = NULL,
        DetalleProvUI(ns("det_prov"))
      ))
    })
    
    ## Indicadores de Municipios ----
    datos_tbl_mun_ind <- reactive({
      req(nrow(dat()$vig) > 0)
      .prep_ind(ITEMS_MUN_IND) %>%
        FormatearFila(reglas = reglas_mun_ind, col_item = "Item")
    })
    TablaReactable("tbl_MunicipiosInd", data = datos_tbl_mun_ind,
                   modo_seleccion = "ninguno", col_header_n = 1L,
                   sortable = FALSE, searchable = FALSE, compact = TRUE,
                   mostrar_nota = TRUE, mostrar_badge = TRUE,
                   col_specs = .especs_clic("click_suc_mun"))
    
    suc_mun_sel <- reactiveVal(NULL)
    DetalleMun("det_mun", fec = fec, det_mun = det_mun, suc_r = suc_mun_sel)
    observeEvent(input$click_suc_mun, {
      suc_mun_sel(input$click_suc_mun)
      showModal(modalDialog(
        title = tags$div(
          tags$span(style = "font-weight:600;",
                    paste("Detalle de Municipios —", input$click_suc_mun)),
          tags$span(style = "font-size:0.82rem; color:#888; margin-left:10px;",
                    .label_periodo())
        ),
        size = "xl", easyClose = TRUE, footer = NULL,
        DetalleMunUI(ns("det_mun"))
      ))
    })
    
    ## Producción, Despachos, Rechazos, Reclamos e Ingresos — detalle por producto ----
    ## .prep_prod respeta el orden de NIVELES_PRODUCTOS (factor ordenado desde el ETL).
    datos_tbl_produccion <- .prep_prod("PRODUCCION", "PRODUCCION", reglas_prod_des)
    datos_tbl_despachos  <- .prep_prod("DESPACHOS",  "DESPACHOS",  reglas_prod_des)
    datos_tbl_rechazos   <- .prep_prod("RECHAZOS",   "RECHAZOS",   reglas_prod_des)
    datos_tbl_reclamos   <- .prep_prod("RECLAMOS",   "RECLAMOS",   reglas_prod_des)
    datos_tbl_ingreso    <- .prep_prod("INGRESOS",   "INGRESOS",   reglas_ingresos)
    
    .tabla("tbl_Produccion", data = datos_tbl_produccion)
    .tabla("tbl_Despachos",  data = datos_tbl_despachos)
    .tabla("tbl_Rechazos",   data = datos_tbl_rechazos)
    .tabla("tbl_Reclamos",   data = datos_tbl_reclamos)
    .tabla("tbl_Ingreso",    data = datos_tbl_ingreso)
    
    ## Indicador Diferenciados / Total Excelsos — Producción y Despachos ----
    datos_tbl_produccion_ind <- .prep_diferenciados("PRODUCCION", "PRODUCCION")
    datos_tbl_despachos_dif  <- .prep_diferenciados("DESPACHOS",  "DESPACHOS")
    .tabla("tbl_ProduccionInd", data = datos_tbl_produccion_ind)
    .tabla("tbl_DespachosDif",  data = datos_tbl_despachos_dif)
    
    ## Indicador Preparaciones Superiores (DespachoSuperiores del ETL) ----
    datos_tbl_despachos_ind <- reactive({
      req(nrow(dat()$vig) > 0)
      .prep_despachos_ind() %>%
        FormatearFila(reglas = reglas_despachos_ind, col_item = "Item")
    })
    .tabla("tbl_DespachosInd", data = datos_tbl_despachos_ind)
    
    ## Indicadores de Rechazos y Reclamos sobre Despachos ----
    datos_tbl_rechazos_ind <- reactive({
      req(nrow(dat()$vig) > 0)
      .prep_ratio("RECHAZOS", "RECHAZOS", "DESPACHOS", "DESPACHOS",
                  "% RECHAZOS SOBRE DESPACHOS") %>%
        FormatearFila(reglas = reglas_rechazos_ind, col_item = "Item")
    })
    datos_tbl_reclamos_ind <- reactive({
      req(nrow(dat()$vig) > 0)
      .prep_ratio("RECLAMOS", "RECLAMOS", "DESPACHOS", "DESPACHOS",
                  "% RECLAMOS SOBRE DESPACHOS") %>%
        FormatearFila(reglas = reglas_reclamos_ind, col_item = "Item")
    })
    .tabla("tbl_RechazosInd", data = datos_tbl_rechazos_ind)
    .tabla("tbl_ReclamosInd", data = datos_tbl_reclamos_ind)
    
    ## Costos Fijos — detalle por ítem en miles de $COP ----
    datos_tbl_costos_fijos <- reactive({
      req(nrow(dat()$vig) > 0)
      dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = "COSTOS",
                    subseccion = "FIJOS", detalle = "TOTAL",
                    item = NIVELES_CF_ITEM) %>%
        mutate(Item = factor(Item, levels = NIVELES_CF_ITEM, ordered = TRUE)) %>%
        arrange(Item) %>%
        mutate(Item = as.character(Item)) %>%
        FormatearFila(reglas = reglas_costos_fijos, col_item = "Item")
    })
    .tabla("tbl_CostosFijos", data = datos_tbl_costos_fijos)
    
    ## Ejecución de Presupuesto de Costos Fijos ----
    ## Presupuesto mensual = PresupuestoMes del ETL; acumulado = suma mensual × n_meses en dat()
    datos_tbl_costos_fijos_ind <- reactive({
      req(nrow(dat()$vig) > 0)
      bd <- if (es_acumulado()) "acum_vig" else "vig"
      label_ppto <- if (es_acumulado()) {
        paste0("PRESUPUESTO (ACUM. ", month(fec()), " MESES)")
      } else "PRESUPUESTO (MES)"
      
      real <- dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = "COSTOS",
                    subseccion = "FIJOS", detalle = "TOTAL", item = "TOTAL",
                    label_item = "EJECUCIÓN")
      
      # PresupuestoMes se suma a lo largo de los meses en acum_vig → total acumulado correcto
      ppto_raw <- dat()[[bd]] %>%
        filter(Seccion == "COSTOS", Subseccion == "FIJOS",
               Detalle == "TOTAL", Item == "TOTAL", Sucursal != "TOTAL")
      
      ppto <- if (es_sucursal()) {
        niveles_suc <- c(levels(ppto_raw$Sucursal) %>% setdiff("TOTAL"), "TOTAL")
        bind_rows(
          ppto_raw %>%
            mutate(Sucursal = as.character(Sucursal)) %>%
            group_by(Sucursal) %>%
            summarise(Valor = sum(PresupuestoMes, na.rm = TRUE), .groups = "drop"),
          ppto_raw %>%
            summarise(Valor = sum(PresupuestoMes, na.rm = TRUE)) %>%
            mutate(Sucursal = "TOTAL")
        ) %>%
          mutate(Sucursal = factor(Sucursal, levels = niveles_suc)) %>%
          arrange(Sucursal) %>%
          pivot_wider(names_from = Sucursal, values_from = Valor, values_fill = 0) %>%
          mutate(Item = label_ppto)
      } else {
        ppto_raw %>%
          summarise(TOTAL = sum(PresupuestoMes, na.rm = TRUE)) %>%
          mutate(Item = label_ppto)
      }
      
      bind_rows(real, ppto) %>%
        AgregarCociente("EJECUCIÓN", label_ppto, label = "CUMPLIMIENTO %") %>%
        FormatearFila(reglas = c(
          list("EJECUCIÓN"      = list(formato = "miles0"),
               "CUMPLIMIENTO %" = list(formato = "porcentaje", negrita = TRUE,
                                       meta = 1, inverso = TRUE)),
          setNames(list(list(formato = "miles0")), label_ppto)
        ), col_item = "Item")
    })
    .tabla("tbl_CostosFijosInd", data = datos_tbl_costos_fijos_ind)
    
    ## Costos Variables — ratios derivados de componentes; movilizadores en promedio ----
    ## KW/saco y Mov/saco se calculan desde sumas de componentes (no desde items ratio del ETL).
    datos_tbl_costos_variables <- reactive({
      req(nrow(dat()$vig) > 0)
      .pm <- function(item, fun = sum) dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = "COSTOS",
                    subseccion = "VARIABLE", detalle = "TOTAL", item = item, fun = fun)
      
      kw   <- .pm("ENERIGA KW")             %>% select(where(is.numeric))
      ener <- .pm("ENERGÍA ELÉCTRICA")       %>% select(where(is.numeric))
      mov  <- .pm("MOVILIZACIONES")          %>% select(where(is.numeric))
      movd <- .pm("MOVILIZADORES", mean)     %>% select(where(is.numeric))
      prod <- dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = "PRODUCCION",
                    subseccion = "PRODUCCION", detalle = "TOTAL", item = "TOTAL") %>%
        select(where(is.numeric))
      
      kw_saco  <- as_tibble_row(unlist(kw)  / unlist(prod))
      mov_saco <- as_tibble_row(unlist(mov) / unlist(prod))
      
      bind_rows(
        kw        %>% mutate(Item = "ENERGÍA (KW)"),
        ener      %>% mutate(Item = "COSTO DE ENERGÍA (MILES $COP)"),
        kw_saco   %>% mutate(Item = "KW POR SACO"),
        movd      %>% mutate(Item = "NÚMERO DE MOVILIZADORES"),
        mov       %>% mutate(Item = "COSTO DE MOVILIZACIÓN (MILES $COP)"),
        mov_saco  %>% mutate(Item = "COSTO DE MOVILIZACIÓN POR SACO ($COP)")
      ) %>%
        relocate(Item) %>%
        mutate(Item = factor(Item, levels = NIVELES_CV, ordered = TRUE)) %>%
        arrange(Item) %>%
        mutate(Item = as.character(Item)) %>%
        FormatearFila(reglas = reglas_costos_variables, col_item = "Item")
    })
    .tabla("tbl_CostosVariables", data = datos_tbl_costos_variables)
    
    ## Utilidad Operacional ----
    datos_tbl_ut_operacional  <- .tabla_ut_rv("UTILIDAD", "OPERACIONAL",    "UTILIDAD OPERACIONAL")
    datos_tbl_otros_ingresos  <- .tabla_ut_rv("INGRESOS", "OTROS INGRESOS", "OTROS INGRESOS")
    datos_tbl_ut_corte        <- .tabla_ut_rv("UTILIDAD", "ANTES DEL CORTE","UTILIDAD ANTES DEL CORTE")
    datos_tbl_cortes          <- .tabla_ut_rv("CORTES",   "CORTES",         "RESULTADO DEL CORTE")
    datos_tbl_ut_neta         <- .tabla_ut_rv("UTILIDAD", "UTILIDAD NETA",  "UTILIDAD NETA")
    
    .tabla("tbl_UtOperacional", data = datos_tbl_ut_operacional)
    .tabla("tbl_OtrosIngresos", data = datos_tbl_otros_ingresos)
    .tabla("tbl_UtCorte",       data = datos_tbl_ut_corte)
    .tabla("tbl_Cortes",        data = datos_tbl_cortes)
    .tabla("tbl_UtNeta",        data = datos_tbl_ut_neta)
    
    ## Otros Costos ----
    datos_tbl_otros_costos <- reactive({
      req(nrow(dat()$vig) > 0)
      dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = "COSTOS",
                    subseccion = "OTROS COSTOS", item = "TOTAL",
                    label_item = "OTROS COSTOS") %>%
        mutate(across(where(is.numeric), ~round(.x, 0L))) %>%
        FormatearFila(reglas = setNames(list(list(formato = "miles0")), "OTROS COSTOS"),
                      col_item = "Item")
    })
    .tabla("tbl_OtrosCostos", data = datos_tbl_otros_costos)
    
    ## Costos Punto de Compra — complete para todas las sucursales (no todas tienen PC) ----
    datos_tbl_punto_compra <- reactive({
      req(nrow(dat()$vig) > 0)
      df <- dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = "COSTOS PC",
                    subseccion = "TOTAL", item = "TOTAL", detalle = "TOTAL",
                    label_item = "COSTOS PUNTO DE COMPRA")
      # Rellena sucursales sin punto de compra y reordena columnas
      faltantes <- setdiff(setdiff(NIVELES_SUCS, "TOTAL"), names(df))
      for (s in faltantes) df[[s]] <- 0L
      cols_ord  <- c("Item", intersect(NIVELES_SUCS, names(df)))
      df %>%
        select(all_of(cols_ord)) %>%
        mutate(across(where(is.numeric), ~round(.x, 0L))) %>%
        FormatearFila(reglas = setNames(list(list(formato = "miles0")), "COSTOS PUNTO DE COMPRA"),
                      col_item = "Item")
    })
    .tabla("tbl_PuntoCompra", data = datos_tbl_punto_compra)
    
    ## Talento Humano — Planta de Personal en promedio; resto en suma ----
    datos_tbl_talento_humano <- reactive({
      req(nrow(dat()$vig) > 0)
      planta <- dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = "TALENTO HUMANO",
                    subseccion = "TALENTO HUMANO", detalle = "TOTAL",
                    item = "PLANTA DE PERSONAL TOTAL", fun = mean)
      resto  <- dat() %>%
        PrepMetrica(es_acumulado(), es_sucursal(), seccion = "TALENTO HUMANO",
                    subseccion = "TALENTO HUMANO", detalle = "TOTAL",
                    item = c("HORAS EXTRAS", "HORAS EXTRAS - CANTIDAD EMPLEADOS",
                             "COSTO $ HORAS EXTRAS", "NÚMERO DE ACCIDENTES",
                             "CANTIDAD EN DÍAS DE AUSENCIA - ACCIDENTES"))
      bind_rows(planta, resto) %>%
        FormatearFila(reglas = list(
          "PLANTA DE PERSONAL TOTAL"                  = list(formato = "coma"),
          "HORAS EXTRAS"                              = list(formato = "coma"),
          "HORAS EXTRAS - CANTIDAD EMPLEADOS"         = list(formato = "coma"),
          "COSTO $ HORAS EXTRAS"                      = list(formato = "dinero"),
          "NÚMERO DE ACCIDENTES"                      = list(formato = "coma"),
          "CANTIDAD EN DÍAS DE AUSENCIA - ACCIDENTES" = list(formato = "coma")
        ), col_item = "Item")
    })
    .tabla("tbl_TalentoHumano", data = datos_tbl_talento_humano)
    
  })
}