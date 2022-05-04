# Fazer esse app usando Golem

library(shiny)
library(dplyr)
library(echarts4r)

cetesb <- readr::read_rds(here::here("dados/cetesb.rds"))

ui <- fluidPage(
  titlePanel("Poluição do ar"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "estacao",
        label = "Selecione uma estação",
        choices = sort(unique(cetesb$estacao_cetesb))
      ),
      selectInput(
        "poluente",
        label = "Selecione um poluente",
        choices = c("Carregando" = "...")
      )
    ),
    mainPanel(
      echarts4rOutput("grafico")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    poluentes <- cetesb |> 
      filter(estacao_cetesb == input$estacao) |> 
      pull(poluente) |> 
      unique()
    updateSelectInput(
      inputId = "poluente",
      choices = poluentes
    )
  })
  
  output$grafico <- renderEcharts4r({
    cetesb |> 
      filter(
        estacao_cetesb == input$estacao,
        poluente == input$poluente
      ) |> 
      group_by(data) |> 
      summarise(media = mean(concentracao, na.rm = TRUE)) |> 
      e_chart(x = data) |> 
      e_line(serie = media)
  })
  
}

shinyApp(ui, server)

