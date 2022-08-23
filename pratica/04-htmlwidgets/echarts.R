# Fazer esse app como uma página de um dashboard feito usando Golem

library(shiny)
library(dplyr)
library(echarts4r)

pnud <- readr::read_rds(here::here("dados/pnud_min.rds"))

nossa_e_tooltip <- function (e, trigger = c("item", "axis"), ...) {
  if (missing(e)) {
    stop("must pass e", call. = FALSE)
  }
  tooltip <- list(trigger = trigger[1], ...)
  if (!e$x$tl) {
    e$x$opts$tooltip <- tooltip
  }
  else {
    e$x$opts$baseOption$tooltip <- tooltip
  }
  e
}

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
      width = 12,
      echarts4rOutput("grafico")
    )
  )
)

server <- function(input, output, session) {
  
  output$grafico <- renderEcharts4r({
    
    tooltip <- htmlwidgets::JS(
      "function(params) {
        return(params.value[0] + '<br><b>' + params.seriesName + '</b>: ' + params.value[1]);
      }"
    )
    
    pnud |> 
      filter(ano == input$ano) |>
      group_by(uf_sigla) |> 
      summarise(across(c(idhm, espvida, rdpc, gini), mean)) |> 
      mutate(across(where(is.numeric), round, digits = 2)) |> 
      arrange(across(one_of(input$variavel), desc)) |> 
      e_chart(x = uf_sigla) |> 
      e_bar_(serie = input$variavel) |> 
      e_legend(show = FALSE) |> 
      e_title(text = input$variavel, left = "center") |> 
      e_x_axis(
        name = "Estado",
        nameLocation = "center",
        nameGap = 35
      ) |> 
      e_y_axis(
        name = input$variavel,
        nameLocation = "center",
        nameGap = 35
      ) |> 
      e_color(color = "purple") |> 
      nossa_e_tooltip(
        formatter = tooltip
      )
      
  })
  
}

shinyApp(ui, server)