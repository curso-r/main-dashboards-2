# Refatorar para a tabela não depender do tipo de visualização

library(shiny)

ui <- fluidPage(
  h1("Módulos que retornam valores"),
  hr(),
  mod_filtro_ui("mod_filtro_1"),
  fluidPage(
    column(
      width = 6,
      echarts4r::echarts4rOutput("grafico")
    ),
    column(
      width = 6,
      reactable::reactableOutput("tabela")
    )
  )
)


server <- function(input, output, session) {
  
  ssp <- readr::read_rds(here::here("dados/ssp.rds"))
  
  valores <- mod_filtro_server("mod_filtro_1", ssp)
  
  output$grafico <- echarts4r::renderEcharts4r({
    
    req(valores())
    
    tab <- ssp |> 
      dplyr::filter(
        regiao_nome == valores()$regiao,
        municipio_nome == valores()$muni
      ) |> 
      dplyr::group_by(ano) |> 
      dplyr::summarise(
        roubo_veiculo = mean(roubo_veiculo, na.rm = TRUE)
      ) |> 
      dplyr::mutate(ano = as.character(ano))
    
    if (valores()$vis_escolhida == "barras") {
      tab  |> 
        echarts4r::e_chart(x = ano) |> 
        echarts4r::e_bar(serie = roubo_veiculo)
    } else {
      tab |> 
        echarts4r::e_chart(x = ano) |> 
        echarts4r::e_line(serie = roubo_veiculo)
    }
     
  })
  
  output$tabela <- reactable::renderReactable({
    
    req(valores())
    
    browser()
    
    tab <- ssp |> 
      dplyr::filter(
        regiao_nome == valores()$regiao,
        municipio_nome == valores()$muni
      ) |> 
      dplyr::group_by(delegacia_nome) |> 
      dplyr::summarise(
        roubo_veiculo = round(mean(roubo_veiculo, na.rm = TRUE), 1)
      ) |> 
      dplyr::arrange(desc(roubo_veiculo)) |> 
      reactable::reactable()
  })
}

shinyApp(ui, server)