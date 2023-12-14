library(shiny)

ui <- fluidPage(
  tags$p("Parágrafo 1", id = "alvo"),
  tags$p("Parágrafo 2"),
  tags$button(onClick = "deixarVermelho()", "Deixar vermelho"),
  tags$script(
    'deixarVermelho = function() {
      document.getElementById("alvo").style = "color: red";
    }'
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)