ult_act <- if (file.exists("data/data.RData")) {
  format(file.info("data/data.RData")$mtime, "%d %b %Y %I:%M:%S %p")
} else {
  NA_character_
}


footer <- bs4DashFooter(
  left = tags$div(
    tags$a(style = paste("display:flex;align-items:center;", "gap:14px;padding:8px 12px;cursor:default;"),
           tags$span(style = "font-size:0.79rem;color:#999;",
                     "Última Actualización:"),
           tags$span(style = "font-size:0.79rem;color:#999;",
                     ult_act
           )
    )
  ),
  right = tags$img(
    src = "https://raw.githubusercontent.com/HCamiloYateT/Compartido/main/img/logo.png",
    style = "height:30px;"
  )
)
