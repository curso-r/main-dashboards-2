library(shiny)
library(bs4Dash)
library(dplyr)
library(ggplot2)

pnud <- abjData::pnud_min

caixa_filtros_graficos <- function(id_ano, id_metrica) {
  fluidRow(
    box(
      title = "Filtros",
      width = 12,
      fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = id_ano,
            label = "Selecione o ano",
            choices = unique(sort(pnud$ano))
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId = id_metrica,
            label = "Selecione a métrica",
            choices = c("idhm", "rdpc", "gini")
          )
        )
      )
    )
  )
}

ui <- bs4DashPage(
  bs4DashNavbar(title = "HTMLWIDGETS"),
  bs4DashSidebar(
    bs4SidebarMenu(
      bs4SidebarMenuItem(
        "reactable",
        tabName = "reactable",
        icon = icon("table")
      ),
      bs4SidebarMenuItem(
        "plotly",
        tabName = "plotly",
        icon = icon("line-chart")
      ),
      bs4SidebarMenuItem(
        "echarts",
        tabName = "echarts",
        icon = icon("line-chart")
      ),
      bs4SidebarMenuItem(
        "highcharts",
        tabName = "highcharts",
        icon = icon("line-chart")
      ),
      bs4SidebarMenuItem(
        "leaflet",
        tabName = "leaflet",
        icon = icon("map-marked-alt")
      )
    )
  ),
  bs4DashBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css")
    ),
    bs4TabItems(
      bs4TabItem(
        tabName = "reactable",
        h1("Reactable"),
        fluidRow(
          box(
            title = "Filtros",
            width = 12,
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "ano",
                  label = "Selecione o ano",
                  choices = unique(sort(pnud$ano))
                )
              ),
              column(
                width = 4,
                selectInput(
                  inputId = "metrica",
                  label = "Selecione a métrica",
                  choices = c("idhm", "espvida", "rdpc", "gini")
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            reactable::reactableOutput("tabela_rt")
          )
        )
      ),
      bs4TabItem(
        tabName = "plotly",
        caixa_filtros_graficos(
          id_ano = "plotly_ano",
          id_metrica = "plotly_metrica"
        ),
        fluidRow(
          column(
            width = 6,
            plotly::plotlyOutput("grafico_plotly")
          ),
          column(
            width = 6,
            plotly::plotlyOutput("grafico_plotly2")
          )
        )
      ),
      bs4TabItem(
        tabName = "echarts",
        caixa_filtros_graficos(
          id_ano = "echarts_ano",
          id_metrica = "echarts_metrica"
        ),
        fluidRow(
          column(
            width = 6,
            offset = 3,
            echarts4r::echarts4rOutput("grafico_echarts")
          )
        )
      ),
      bs4TabItem(
        tabName = "highcharts",
        caixa_filtros_graficos(
          id_ano = "hc_ano",
          id_metrica = "hc_metrica"
        ),
        fluidRow(
          column(
            width = 6,
            offset = 3,
            highcharter::highchartOutput("grafico_hc")
          )
        )
      ),
      bs4TabItem(
        tabName = "leaflet",
        fluidRow(
          box(
            title = "Filtros",
            width = 12,
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = "leaflet_ano",
                  label = "Selecione o ano",
                  choices = unique(sort(pnud$ano))
                )
              ),
              column(
                width = 4,
                selectInput(
                  inputId = "leaflet_uf",
                  label = "Selecione o estado",
                  choices = unique(sort(pnud$uf_sigla))
                )
              ),
              column(
                width = 4,
                selectInput(
                  inputId = "leaflet_metrica",
                  label = "Selecione a métrica",
                  choices = c("idhm", "espvida", "rdpc", "gini")
                )
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 10,
            offset = 1,
            leaflet::leafletOutput("mapa_leaflet")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  # reactable ---------------------------------------------------------------
  
  output$tabela_rt <- reactable::renderReactable({
    
    pnud |>
      filter(ano == input$ano) |>
      arrange(across(input$metrica, desc)) |>
      slice(1:10) |>
      select(
        muni_nm,
        uf_sigla,
        input$metrica,
        idhm,
        espvida,
        rdpc,
        gini
      ) |>
      reactable::reactable(
        striped = TRUE,
        searchable = TRUE,
        filterable = TRUE,
        bordered = TRUE,
        highlight = TRUE,
        # defaultPageSize = 5,
        theme = reactable::reactableTheme(
          stripedColor = "grey"
        ),
        defaultColDef = reactable::colDef(
          align = "right"
        ),
        columns = list(
          muni_nm = reactable::colDef(
            align = "center"
          )
        )
      )
  })
  
  
  # plotly ------------------------------------------------------------------
  
  output$grafico_plotly <- plotly::renderPlotly({
    
    p <- pnud |>
      filter(ano == input$plotly_ano) |>
      rename(x = input$plotly_metrica) |>
      ggplot(aes(x = x, y = espvida)) +
      geom_point() +
      theme_minimal()
    
    plotly::ggplotly(p)
    
  })
  
  output$grafico_plotly2 <- plotly::renderPlotly({
    
    pnud |>
      filter(ano == input$plotly_ano) |>
      rename(x = input$plotly_metrica) |>
      plotly::plot_ly(
        x = ~x,
        y = ~espvida,
        type = "scatter"
      )
    
  })
  
  
  # echarts -----------------------------------------------------------------
  
  output$grafico_echarts <- echarts4r::renderEcharts4r({
    
    pnud |>
      filter(ano == input$echarts_ano) |>
      rename(x = input$echarts_metrica) |>
      echarts4r::e_chart(x = x) |>
      echarts4r::e_scatter(serie = espvida)
    # echarts4r::e_tooltip() |>
    # echarts4r::e_legend(show = FALSE)
  })
  
  
  # highcharts --------------------------------------------------------------
  
  
  output$grafico_hc <- highcharter::renderHighchart({
    
    pnud |>
      filter(ano == input$hc_ano) |>
      rename(x = input$hc_metrica) |>
      highcharter::hchart(
        type = "scatter",
        highcharter::hcaes(x = x, y = espvida)
      ) |>
      highcharter::hc_tooltip(borderColor = "orange")
    
    # highcharter::highchart() |> highcharter::hc_add_series() |> ...
    
  })
  
  
  # leaflet -----------------------------------------------------------------
  
  
  output$mapa_leaflet <- leaflet::renderLeaflet({
    
    pnud |>
      filter(ano == input$leaflet_ano, uf_sigla == input$leaflet_uf) |>
      arrange(across(input$leaflet_metrica, desc)) |>
      slice(1:10) |>
      leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addMarkers(
        lng = ~lon,
        lat = ~lat,
        label = ~muni_nm,
        popup = ~muni_nm
      )
    
  })
  
}

shinyApp(ui, server)