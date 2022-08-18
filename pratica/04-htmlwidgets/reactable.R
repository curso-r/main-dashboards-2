# Fazer esse app como uma página de um dashboard feito usando Golem

library(shiny)
library(leaflet)
library(dplyr)
library(reactable)

pnud <- readr::read_rds(here::here("dados/pnud_min.rds"))

ui <- fluidPage(
  h1("PNUD - Programa das Nações Unidas para o Desenvolvimento"),
  hr(),
  fluidRow(
    column(
      width = 4,
      selectInput(
        "ano",
        label = "Selecione um ano",
        choices = unique(pnud$ano),
        selected = max(pnud$ano),
        width = "100%"
      )
    ),
    column(
      width = 4,
      selectInput(
        "variavel",
        label = "Selecione uma métrica",
        choices = c(
          "IDHM" = "idhm",
          "Expectativa de vida" = "espvida",
          "Renda per capita" = "rdpc",
          "Índice de GINI" = "gini"
        ),
        width = "100%"
      )
    )
  ),
  fluidRow(
    column(
      width = 12,
      reactableOutput("tabela")
    ),
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      leafletOutput("mapa", height = "500px"),
      br()
    )
  )
)

server <- function(input, output, session) {
  
  tabela <- reactive({
    pnud |> 
      filter(
        ano == input$ano
      ) |> 
      arrange(across(one_of(input$variavel), desc)) |> 
      slice(1:10)
  })
  
  output$tabela <- renderReactable({
    tabela() |> 
      select(muni_nm, one_of(input$variavel), espvida, idhm, rdpc, gini) |> 
      reactable(
        selection = "multiple",
        defaultSelected = 1
      )
  })
  
  output$mapa <- renderLeaflet({
    
    linha <- reactable::getReactableState("tabela", name = "selected")
    
    req(linha)
    
    tabela() |>
      slice(linha) |> 
      leaflet() |>
      addTiles() |> 
      addAwesomeMarkers(
        lng = ~ lon,
        lat = ~ lat,
        label = ~ muni_nm,
        icon = awesomeIcons()
      )
    
  })
  
  
}

shinyApp(ui, server)