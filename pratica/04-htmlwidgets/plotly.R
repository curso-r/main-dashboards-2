# Fazer esse app como uma página de um dashboard feito usando Golem

library(shiny)
library(dplyr)
library(ggplot2)

pnud <- readr::read_rds(here::here("dados/pnud_min.rds"))

ui <- fluidPage(
  h1("PNUD - Programa das Nações Unidas para o Desenvolvimento"),
  hr(),
  fluidRow(
    column(
      width = 3,
      selectInput(
        "ano",
        label = "Selecione um ano",
        choices = unique(pnud$ano),
        width = "90%"
      )
    )
  ),
  fluidRow(
    column(
      width = 8,
      plotly::plotlyOutput("grafico")
    ),
    column(
      width = 4,
      reactable::reactableOutput("tabela")
    )
  )
)

server <- function(input, output, session) {
  
  pnud_filtrada <- reactive({
    pnud |> 
      filter(ano == input$ano)
  })
  
  output$grafico <- plotly::renderPlotly({
    p <- pnud_filtrada() |> 
      ggplot(aes(x = rdpc, y = espvida)) +
      geom_point()
    
    plotly::ggplotly(p)
  })
  
  output$tabela <- reactable::renderReactable({
    
    clique <- plotly::event_data("plotly_click")
    
    if (is.null(clique)) {
      NULL
    } else {
      pnud_filtrada() |> 
        slice(clique$pointNumber+1) |>
        select(
          muni_nm,
          espvida,
          rdpc,
          idhm,
          gini
        ) |> 
        mutate(across(.fns = as.character)) |> 
        tidyr::pivot_longer(
          cols = everything(),
          names_to = "var",
          values_to = "val"
        ) |> 
        reactable::reactable(
          columns = list(
            var = reactable::colDef(
              name = ""
            ),
            val = reactable::colDef(
              name = ""
            )
          )
        )
    }
    
  })
  
  
  
}

shinyApp(ui, server)