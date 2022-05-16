# Fazer esse app como uma página de um dashboard feito usando Golem

library(shiny)
library(leaflet)
library(dplyr)
library(reactable)

ui <- fluidPage(
  h1("PNUD - Programa das Nações Unidas para o Desenvolvimento"),
  hr(),
  fluidRow(
    column(
      width = 4,
      selectInput(
        "ano",
        label = "Selecione um ano",
        choices = c(1991, 200, 2010),
        selected = 2010,
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
  
  con <- RSQLite::dbConnect(RSQLite::SQLite(), here::here("dados/pnud_min.sqlite"))
  pnud <- dplyr::tbl(con, "pnud")
  
  tabela <- reactive({
    
    ano_selecionado <- as.character(input$ano)
    variavel_selecionada <- input$variavel
    
    pnud |> 
      filter(
        ano == ano_selecionado
      ) |> 
      arrange(desc(.data[[variavel_selecionada]])) |> 
      collect() |> 
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