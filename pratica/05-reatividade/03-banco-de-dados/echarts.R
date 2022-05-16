# Fazer esse app como uma página de um dashboard feito usando Golem

library(shiny)
library(dplyr)
library(echarts4r)

nossa_e_tooltip <- function (e, trigger = c("item", "axis"), ...) 
{
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
        choices = c(1991, 200, 2010),
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
  
  con <- RSQLite::dbConnect(RSQLite::SQLite(), here::here("dados/pnud_min.sqlite"))
  pnud <- dplyr::tbl(con, "pnud")
  
  output$grafico <- renderEcharts4r({
    
    tooltip <- htmlwidgets::JS(
      "function(params) {
        return(params.value[0] + '<br><b>' + params.seriesName + '</b>: ' + params.value[1]);
      }"
    )
    
    ano_selecionado <- as.character(input$ano)
    variavel_selecionada <- input$variavel
    
    pnud |> 
      filter(ano == ano_selecionado) |>
      group_by(uf_sigla) |> 
      summarise(across(c(idhm, espvida, rdpc, gini), mean, na.rm = TRUE)) |> 
      mutate(across(c(idhm, espvida, rdpc, gini), round, digits = 2)) |> 
      collect() |> 
      arrange(across(any_of(variavel_selecionada), desc)) |>
      e_chart(x = uf_sigla) |> 
      e_bar_(serie = variavel_selecionada) |> 
      e_legend(show = FALSE) |> 
      e_title(text = variavel_selecionada, left = "center") |> 
      e_x_axis(
        name = "Estado",
        nameLocation = "center",
        nameGap = 35
      ) |> 
      e_y_axis(
        name = variavel_selecionada,
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