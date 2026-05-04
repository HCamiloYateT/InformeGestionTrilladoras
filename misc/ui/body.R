body <- bs4DashBody(
  includeCSS("https://raw.githubusercontent.com/HCamiloYateT/Compartido/refs/heads/main/Styles/style.css"),
  use_waiter(),
  useShinyjs(),
  bs4TabItems(
    bs4TabItem(tabName = "consolidado",  
               ConsolidadoUI("Consolidado")
               ),
    bs4TabItem(tabName = "arenales",     h6("Arenales")),
    bs4TabItem(tabName = "bachue",       h6("Bachué")),
    bs4TabItem(tabName = "medellin",     h6("Medellín")),
    bs4TabItem(tabName = "popayan",      h6("Popayán")),
    bs4TabItem(tabName = "armenia",      h6("Armenia")),
    bs4TabItem(tabName = "pereira",      h6("Pereira")),
    bs4TabItem(tabName = "bucaramanga",  h6("Bucaramanga")),
    bs4TabItem(tabName = "huila",        h6("Huila"))
    )
  )
