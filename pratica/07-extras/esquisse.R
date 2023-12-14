library(shiny)

pnud <- readRDS(here::here("dados/pnud_min.rds"))

ui <- fluidPage(
  titlePanel("Esquisse"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "estado",
        label = "Selecione um estado",
        choices = sort(unique(pnud$uf_sigla))
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title = "Gráfico",
          esquisse::esquisse_ui("esquisse")
        ),
        tabPanel(
          title = "Código",
          verbatimTextOutput("codigo")
        )
      )
      
    )
  )
)

server <- function(input, output, session) {
  
  dados <- reactiveValues(data = pnud, name = "pnud")
  
  observe({
      dados$data <- dplyr::filter(pnud, uf_sigla == input$estado)
  })
  
  resultado <- esquisse::esquisse_server(
    id = "esquisse",
    data_rv = dados
  )
  
  output$codigo <- renderText({
    resultado$code_plot
  })
}

shinyApp(ui, server, options = list(launch.browser = FALSE, port = 4242))