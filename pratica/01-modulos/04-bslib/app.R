library(shiny)
library(bslib)

ui <- page_navbar(
  title = "Painel da SSP",
  nav_panel(
    title = "Ocorrências"
  ),
  nav_panel(
    title = "Municípios"
  ),
  nav_panel(
    title = "Delegacias"
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)