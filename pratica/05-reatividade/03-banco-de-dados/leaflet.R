# Fazer esse app como uma página de um dashboard feito usando Golem

library(shiny)
library(leaflet)
library(dplyr)
library(reactable)
library(sf)

geo_estados <- readr::read_rds(here::here("dados/geo_estados.rds"))

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
      width = 6,
      leafletOutput("mapa")
    ),
    column(
      width = 6,
      reactableOutput("tabela")
    )
  )
)

server <- function(input, output, session) {
  
  con <- RSQLite::dbConnect(RSQLite::SQLite(), here::here("dados/pnud_min.sqlite"))
  pnud <- dplyr::tbl(con, "pnud")
  
  output$mapa <- renderLeaflet({
    
    ano_selecionado <- as.character(input$ano)
    variavel_selecionada <- input$variavel
  
    tab_mapa <- pnud |> 
      filter(ano == ano_selecionado) |> 
      group_by(uf_sigla) |> 
      summarise(across(variavel_selecionada, mean, .names = "media")) |>
      collect() |>
      left_join(geo_estados, by = c("uf_sigla" = "abbrev_state")) |> 
      sf::st_as_sf() 
    
    cores <- leaflet::colorNumeric(
      palette = rev(viridis::plasma(8)), 
      domain = tab_mapa$media
    )
      
    tab_mapa %>% 
      leaflet() %>% 
      addTiles() %>% 
      addPolygons(
        layerId = ~uf_sigla,
        fillColor = ~cores(media), 
        color = "black",
        fillOpacity = 0.8,
        weight = 1, 
        label  = ~name_state
      ) %>% 
      leaflet::addLegend(
        pal = cores, 
        values = ~media, 
        opacity = 0.7, 
        title = NULL,
        position = "bottomright"
      )
  })
  
  output$tabela <- renderReactable({
    
    ano_selecionado <- as.character(input$ano)
    variavel_selecionada <- input$variavel
    
    if (is.null(input$mapa_shape_click)) {
      estado <- "RJ"
    } else {
      estado <- input$mapa_shape_click
    }
    
    pnud |> 
      filter(
        uf_sigla == estado,
        ano == ano_selecionado
      ) |> 
      select(muni_nm, one_of(variavel_selecionada), espvida, idhm, rdpc, gini) |> 
      arrange(across(one_of(variavel_selecionada), desc)) |> 
      collect() |> 
      reactable()
    
  })
}

shinyApp(ui, server)