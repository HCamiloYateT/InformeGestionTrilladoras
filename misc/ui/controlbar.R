controlbar <- bs4DashControlbar(id = "controlbar", skin = "light", pinned = NULL,
                                overlay = FALSE, width = "290px", type = "tabs",
                                title = "Control",
                                controlbarMenu(id = "Filtros", type = "tabs",
                                               controlbarItem(title = shiny::tagList(shiny::icon("filter"), " Filtros"),
                                                              shiny::tags$div(class = "ctrl-calendar-wrap",
                                                                InputFecha(id = "Fecha", value = fecha_ej, tipo = "mes",
                                                                           min_date = min(datos_trl$Fecha), 
                                                                           max_date = fecha_ej,
                                                                           inline   = TRUE, width = )
                                                                )
                                                              )
                                               )
                                )
