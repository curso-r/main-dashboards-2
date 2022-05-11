library(shiny)

ui <- fluidPage(
  h1("Filtros hierárquicos"),
  hr(),
  fluidRow(
    column(
      width = 4,
      selectInput(
        "regiao",
        label = "Selecione a região",
        choices = c(Carregando = "")
      )
    ),
    column(
      width = 4,
      selectInput(
        "muni",
        label = "Selecione o município",
        choices = c(Carregando = "")
      )
    ),
    column(
      width = 4,
      selectInput(
        "delegacia",
        label = "Selecione a delegacia",
        choices = c(Carregando = "")
      )
    )
  ),
  fluidPage(
    column(
      width = 12,
      echarts4r::echarts4rOutput("grafico")
    )
  )
)

server <- function(input, output, session) {
  
  ssp <- readr::read_rds(here::here("dados/ssp.rds"))
  
  tab_id_delegacia <- ssp |> 
    dplyr::distinct(municipio_nome, delegacia_nome) |> 
    dplyr::mutate(delegacia_id = 1:dplyr::n())
  
  ssp <- ssp |> 
    dplyr::left_join(
      tab_id_delegacia,
      by = c("municipio_nome", "delegacia_nome")
    )
  
  updateSelectInput(
    inputId = "regiao",
    choices = sort(unique(ssp$regiao_nome))
  )
  
  observe({
    
    req(input$regiao)
    
    opcoes <- ssp |> 
      dplyr::filter(regiao_nome == input$regiao) |> 
      dplyr::pull(municipio_nome) |> 
      unique()
    
    updateSelectInput(
      inputId = "muni",
      choices = opcoes
    )
    
  })
  
  observeEvent(input$muni, {
    
    req(input$muni)
    
    opcoes <- ssp |> 
      dplyr::filter(municipio_nome == input$muni) |> 
      dplyr::select(delegacia_nome, delegacia_id) |> 
      dplyr::distinct() |> 
      tibble::deframe()
    
    updateSelectInput(
      inputId = "delegacia",
      choices = opcoes
    )
    
  })
  
  output$grafico <- echarts4r::renderEcharts4r({
    
    req(input$delegacia)
    
    ssp |> 
      dplyr::filter(
        regiao_nome == isolate(input$regiao),
        municipio_nome == isolate(input$muni),
        delegacia_id == input$delegacia
      ) |> 
      dplyr::group_by(ano) |> 
      dplyr::summarise(
        roubo_veiculo = mean(roubo_veiculo, na.rm = TRUE)
      ) |> 
      dplyr::mutate(ano = as.character(ano)) |> 
      echarts4r::e_chart(x = ano) |> 
      echarts4r::e_bar(serie = roubo_veiculo)
  })
  
}

shinyApp(ui, server)