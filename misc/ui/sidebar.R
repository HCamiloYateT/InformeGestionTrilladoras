sidebar <- bs4DashSidebar(status = "danger", expandOnHover = FALSE,
                          bs4SidebarMenu(
                            id = "sidebarMenu",
                            div(id = "wrap_consolidado",
                                bs4SidebarMenuItem("Consolidado",  tabName = "consolidado",  icon = icon("layer-group"))),
                            div(id = "wrap_arenales",
                                bs4SidebarMenuItem("Arenales",     tabName = "arenales",     icon = icon("warehouse"))),
                            div(id = "wrap_bachue",
                                bs4SidebarMenuItem("Bachué",       tabName = "bachue",       icon = icon("industry"))),
                            div(id = "wrap_medellin",
                                bs4SidebarMenuItem("Medellín",     tabName = "medellin",     icon = icon("industry"))),
                            div(id = "wrap_popayan",
                                bs4SidebarMenuItem("Popayán",      tabName = "popayan",      icon = icon("industry"))),
                            div(id = "wrap_armenia",
                                bs4SidebarMenuItem("Armenia",      tabName = "armenia",      icon = icon("industry"))),
                            div(id = "wrap_pereira",
                                bs4SidebarMenuItem("Pereira",      tabName = "pereira",      icon = icon("industry"))),
                            div(id = "wrap_bucaramanga",
                                bs4SidebarMenuItem("Bucaramanga",  tabName = "bucaramanga",  icon = icon("industry"))),
                            div(id = "wrap_huila",
                                bs4SidebarMenuItem("Huila",        tabName = "huila",        icon = icon("industry")))
                            )
                          )
