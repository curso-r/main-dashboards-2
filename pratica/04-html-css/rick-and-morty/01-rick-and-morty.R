library(shiny)
library(dplyr)
library(echarts4r)

# font get schwifty: https://fontmeme.com/fontes/fonte-get-schwifty/

dados <- readr::read_rds(here::here("dados/rick_and_morty.rds"))

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "custom.css")
  ),
  h1("Rick and Morty"),
  hr(),
  fluidRow(
    column(
      width = 4,
      selectInput(
        inputId = "temporada",
        label = "Selecione a temporada",
        choices = unique(dados$num_temporada)
      )
    )
  ),
  fluidRow(
    column(
      offset = 2,
      width = 10,
      div(
        class = "areaPlot",
        echarts4rOutput("grafico", height = "500px")
      )
    )
  ),
  img(src = "rick.png", class = "rick")
)

server <- function(input, output, session) {
  output$grafico <- renderEcharts4r({
    dados |> 
      filter(num_temporada == input$temporada) |>
      arrange(desc(num_dentro_temporada)) |> 
      mutate(titulo = glue::glue("{num_dentro_temporada} - {titulo}")) |> 
      e_chart(x = qtd_espectadores_EUA, reorder = FALSE) |>
      e_bar(serie = titulo) |> 
      e_x_axis(
        name = "Número de espectadores (milhões)",
        nameLocation = "center",
        nameGap = 40,
        axisLabel = list(color = "white"),
        nameTextStyle = list(color = "white", fontSize = 16)
      ) |> 
      e_y_axis_(type = "category", axisLabel = list(color = "white")) |> 
      e_grid(containLabel = TRUE) |> 
      e_legend(show = FALSE) |> 
      e_color(color = "#08acc6")
  })
}

shinyApp(ui, server)