mod_filtro_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        selectInput(
          ns("regiao"),
          label = "Selecione a região",
          choices = c(Carregando = "")
        )
      ),
      column(
        width = 4,
        selectInput(
          ns("muni"),
          label = "Selecione o município",
          choices = c(Carregando = "")
        )
      ),
      column(
        width = 4,
        shinyWidgets::radioGroupButtons(
          inputId = ns("vis_escolhida"),
          label = "Selecione o tipo de gráfico",
          choiceValues = c("barras", "linhas"),
          choiceNames = list(
            icon("chart-bar"),
            icon("chart-line")
          ),
          size = "lg",
          selected = "barras"
        )
      )
    )
  )
}

mod_filtro_server <- function(id, ssp) {
  moduleServer(id, function(input, output, session) {
    
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
    
    valores <- reactive({
      req(input$muni)
      list(
        regiao = isolate(input$regiao),
        muni = input$muni,
        vis_escolhida = input$vis_escolhida
      )
    })
    
    return(valores)
    
  })
}


