.badge_catalog <- list(
  prototipo     = list(label = "PROTOTIPO", clase = "app-badge--prototipo"),
  pruebas       = list(label = "PRUEBAS", clase = "app-badge--pruebas"),
  staging       = list(label = "STAGING", clase = "app-badge--staging"),
  produccion    = list(label = "PRODUCCION", clase = "app-badge--produccion"),
  demo          = list(label = "DEMO", clase = "app-badge--demo"),
  mantenimiento = list(label = "MANT.", clase = "app-badge--mantenimiento"),
  ninguno       = list(label = "", clase = "app-badge--ninguno")
)

app_badge <- function(estado = badge_estado) {
  env    <- tolower(trimws(badge_estado))
  config <- .badge_catalog[[env]]
  
  if (is.null(config)) {
    warning(sprintf(
      "[app_badge] Estado '%s' no reconocido. Usando 'prototipo' como fallback.", env
    ))
    config <- .badge_catalog[["prototipo"]]
  }
  
  tags$span(
    class = paste("app-badge", config$clase),
    style = "display:inline-flex; align-items:center;",
    config$label
  )
}

header <- bs4DashNavbar(status = "white", border = FALSE, sidebarIcon = icon("bars"),
                        title = dashboardBrand(title = tit_app,
                                               href = "https://analitica.racafe.com/PortalAnalitica/",
                                               image = "https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/img/logo2.png"),
                        controlbarIcon = icon("gears"),
                        leftUi = tagList(
                          tags$li(class = "dropdown",
                                  app_badge(badge_estado),
                                  style = "display:flex;align-items:center; gap:8px;padding:8px 12px;cursor:default;",
                                  tags$span(uiOutput("user")),
                                  racafeShiny::Boton("BTN_Actualizar", label = NULL, icono = "sync",
                                                     size = "xxs", title = "Actualizar", color_fondo = "#6c757d", 
                                                     color_hover = "#DA291C"),
                                  racafeShiny::BotonDescarga("Descargar", size = "xxs", title = "Descargar", 
                                                             color_fondo = "#6c757d")
                                  )
                          ),
                        rightUi = tagList(
                          tags$li(class = "dropdown",
                                  style = "display:flex;align-items:center; gap:8px;padding:8px 12px;cursor:default;"
                                  )
                          )
                        )
