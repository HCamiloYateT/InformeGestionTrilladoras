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
