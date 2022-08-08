# 1. Transformar a UI em uma função criada neste mesmo arquivo
# 2. Colocar essa função dentro de uma pasta R e mostrar que o app 
# carrega o código automaticamente

library(shiny)

imdb <- readr::read_rds(here::here("dados/imdb.rds"))

generos <- imdb |> 
  dplyr::pull(generos) |> 
  stringr::str_split(pattern = ", ") |> 
  unlist() |> 
  unique() |> 
  sort()

ui <- fluidPage(
  h1("IMDB"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "genero",
        label = "Selecione um gênero",
        choices = generos
      ),
    ),
    mainPanel(
      plotOutput("grafico_1"),
      plotOutput("grafico_2")
    )
  )
)

server <- function(input, output, session) {
  
  imdb_filtrada <- reactive({
    imdb |> 
      dplyr::filter(stringr::str_detect(generos, input$genero))
  })
  
  output$grafico_1 <- renderPlot({
    imdb_filtrada() |> 
      ggplot2::ggplot(ggplot2::aes(x = ano)) +
      ggplot2::geom_bar()
  })
  
  output$grafico_2 <- renderPlot({
    imdb_filtrada() |>
      dplyr::group_by(ano) |> 
      dplyr::summarise(receita_media = mean(receita, na.rm = TRUE)) |> 
      ggplot2::ggplot(ggplot2::aes(x = ano, y = receita_media)) +
      ggplot2::geom_col()
  })
  
}

shinyApp(ui, server)