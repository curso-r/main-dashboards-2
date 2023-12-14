library(shiny)

dados <- readr::read_rds(here::here("dados/pkmn.rds"))

ui <- fluidPage(
  style = "padding-bottom: 200px;",
  theme = bslib::bs_theme(version = 4),
  h1("Relatório em R Markdown"),
  hr(),
  h3("Sobre este app"),
  includeMarkdown("lorem_ipsum.md"),
  hr(),
  fluidRow(
    column(
      width = 3,
      offset = 4,
      selectInput(
        "pokemon",
        label = "Selecione um pokemon",
        choices = unique(dados$pokemon)
      )
    ),
    column(
      width = 2,
      class = "align-self-center",
      downloadButton("gerar", "Gerar Relatório")
    )
  )
)

server <- function(input, output, session) {
  
  output$gerar <- downloadHandler(
    filename = function() {glue::glue("relatorio_{input$pokemon}.pdf")},
    content = function(file) {
      
      arquivo_html <- tempfile(fileext = ".html")
      
      withProgress({
        
        incProgress(0.3)
        
        rmarkdown::render(
          input = "relatorio.Rmd",
          output_file = arquivo_html,
          params = list(pokemon = input$pokemon)
        )
        
        incProgress(0.4)
        
        pagedown::chrome_print(
          input = arquivo_html,
          output = file
        )
        
      })
      
      
      
    }
  )
  
}

shinyApp(ui, server)