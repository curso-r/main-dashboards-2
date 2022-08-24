library(shiny)

pnud <- readr::read_rds(here::here("dados/pnud_min.rds"))

usar_bs4Dash <- function() {
  if (!requireNamespace(package = "bs4Dash")) 
    message("Package 'bs4Dash' is required to run this function")
  deps <- htmltools::findDependencies(
    bs4Dash::bs4DashPage(
      header = bs4Dash::bs4DashNavbar(), 
      sidebar = bs4Dash::bs4DashSidebar(), 
      body = bs4Dash::bs4DashBody())
  )
  htmltools::attachDependencies(tags$div(), value = deps)
}

ui <- fluidPage(
  h1("Reactable"),
  usar_bs4Dash(),
  hr(),
  fluidRow(
    bs4Dash::bs4Card(
      title = "Filtros",
      width = 12,
      fluidRow(
        column(
          width = 4,
          selectInput(
            "ano",
            label = "Selecione um ano",
            choices = unique(pnud$ano),
            width = "90%"
          )
        ),
        column(
          width = 4,
          selectInput(
            "metrica",
            label = "Selecione uma métrica",
            choices = c(
              "IDHM" = "idhm",
              "Expectativa de vida" = "espvida",
              "Renda per capita" = "rdpc",
              "Índice de GINI" = "gini"
            ),
            width = "90%"
          )
        )
      )
    )
  ),
  br(),
  fluidRow(
    column(
      width = 6,
      reactable::reactableOutput("tabela", height = "500px")
    ),
    column(
      width = 6,
      leaflet::leafletOutput("mapa", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  ns <- session$ns
  
  pnud_top10 <- reactive({
    pnud |>
      dplyr::filter(
        ano == input$ano
      ) |>
      dplyr::arrange(dplyr::desc(.data[[input$metrica]])) |>
      dplyr::slice(1:10)
  })
  
  output$tabela <- reactable::renderReactable({
    linhas_selecionadas <- isolate(
      reactable::getReactableState("tabela", "selected")
    )
    if (is.null(linhas_selecionadas)) {
      linhas_selecionadas <- 1
    }
    pnud_top10() |>
      dplyr::select(muni_nm, one_of(input$metrica), espvida, idhm, rdpc, gini) |>
      reactable::reactable(
        selection = "multiple",
        defaultSelected = linhas_selecionadas
      )
  })
  
  output$mapa <- leaflet::renderLeaflet({
    linhas_selecionadas <- reactable::getReactableState("tabela", "selected")
    
    req(linhas_selecionadas)
    
    print("Rodei o mapa")
    
    if (is.null(linhas_selecionadas)) {
      NULL
    } else {
      pnud_top10() |>
        dplyr::slice(linhas_selecionadas) |>
        leaflet::leaflet() |>
        leaflet::addTiles() |>
        leaflet::addAwesomeMarkers(
          lng = ~ lon,
          lat = ~ lat,
          label = ~ muni_nm
        )
    }
  })
}

shinyApp(ui, server)  