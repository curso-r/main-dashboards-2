library(shiny)

ui <- fluidPage(
  h1("Editando banco de dados"),
  hr(),
  DT::dataTableOutput("tabela"),
  br(),
  actionButton("salvar", "Salvar alterações")
)

server <- function(input, output, session) {
  
  con <- RSQLite::dbConnect(RSQLite::SQLite(), "mtcars.sqlite")
  
  tabela_para_salvar <- reactiveVal(NULL)
  
  output$tabela <- DT::renderDataTable({
    
    tab_mtcars <- dplyr::tbl(con, "mtcars") |> 
      dplyr::collect()
    
    tabela_para_salvar(tab_mtcars)
    
    tab_mtcars |> 
      dplyr::collect() |> 
      DT::datatable(
        editable = TRUE
      )
  })
  
  proxy <- DT::dataTableProxy("tabela")
  
  observeEvent(input$tabela_cell_edit, {
    
    tab_atual <- tabela_para_salvar()
    
    tab_atualizada <- DT::editData(tab_atual, input$tabela_cell_edit)
    
    DT::replaceData(
      proxy,
      tab_atualizada,
      resetPaging = TRUE
    )
    
    tabela_para_salvar(tab_atualizada)
  })
  
  observeEvent(input$salvar, {
    RSQLite::dbWriteTable(
      con,
      name = "mtcars",
      value = tabela_para_salvar(),
      overwrite = TRUE
    )
    
    showModal(
      modalDialog(
        "Alterações salvas com sucesso!",
        title = "Aviso",
        easyClose = TRUE,
        footer = modalButton("Fechar")
      )
    )
  })
  
}

shinyApp(ui, server)