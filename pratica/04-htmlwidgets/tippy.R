# Fazer esse app como uma página de um dashboard feito usando Golem

# https://atomiks.github.io/tippyjs/

library(shiny)
library(dplyr)

pnud <- readr::read_rds(here::here("dados/pnud_min.rds"))

ui <- fluidPage(
  h1("PNUD - Programa das Nações Unidas para o Desenvolvimento"),
  hr(),
  fluidRow(
    column(
      width = 3,
      div(
        id = "filtro_ano",
        selectInput(
          "ano",
          label = "Selecione um ano",
          choices = unique(pnud$ano),
          width = "90%"
        )
      )
    ),
    column(
      width = 3,
      selectInput(
        "regiao",
        label = "Selecione uma região",
        choices = sort(unique(pnud$regiao_nm)),
        width = "90%"
      )
    ),
    column(
      width = 3,
      selectInput(
        "estado",
        label = "Selecione um estado",
        choices = c("Carregando" = "..."),
        width = "90%"
      )
    ),
    column(
      width = 3,
      selectInput(
        "municipio",
        label = "Selecione um município",
        choices = c("Carregando" = "..."),
        width = "100%"
      )
    )
  ),
  fluidRow(
    column(
      offset = 5,
      width = 2,
      actionButton("pesquisar", label = "Pesquisar")
      # actionButton("pesquisar", label = "Pesquisar") |> 
      #   tippy::with_tippy("Clique aqui para atualizar o resultado")
    )
  ),
  br(),
  br(),
  fluidRow(
    column(
      width = 12,
      reactable::reactableOutput("tabela")
    )
  ),
  tags$script(src = "https://unpkg.com/@popperjs/core@2"),
  tags$script(src = "https://unpkg.com/tippy.js@6"),
  tags$script(src = "tooltip.js")
)

server <- function(input, output, session) {
  
  observe({
    ufs <- pnud |> 
      filter(regiao_nm == input$regiao) |> 
      pull(uf_sigla) |> 
      sort()
    
    updateSelectInput(
      inputId = "estado",
      choices = ufs
    )
  })
  
  observe({
    municipios <- pnud |> 
      filter(uf_sigla == input$estado) |> 
      select(muni_nm, muni_id) |> 
      arrange(muni_nm) |> 
      tibble::deframe()
    
    updateSelectInput(
      inputId = "municipio",
      choices = municipios
    )
  })
  
  
  # Fazer sem o botão primeiro ----------------------------------------------
  
  # output$tabela <- reactable::renderReactable({
  #   pnud |> 
  #     filter(muni_id == input$municipio, ano == input$ano) |> 
  #     select(
  #       Município = muni_nm,
  #       Populaçãp = pop,
  #       IDHM = idhm,
  #       `Esp. Vida` = espvida,
  #       `Renda per capita` = rdpc,
  #       GINI = gini
  #     ) |> 
  #     reactable::reactable()
  # })
  

  # Depois fazer versão com botão -------------------------------------------
  
  tabela_filtrada <- eventReactive(input$pesquisar, {
    pnud |> 
      filter(muni_id == input$municipio, ano == input$ano)
  })
  
  output$tabela <- reactable::renderReactable({
    tabela_filtrada() |> 
      select(
        Município = muni_nm,
        Populaçãp = pop,
        IDHM = idhm,
        `Esp. Vida` = espvida,
        `Renda per capita` = rdpc,
        GINI = gini
      ) |> 
      reactable::reactable()
  })
  
}

shinyApp(ui, server)