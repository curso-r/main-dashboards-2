library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Dados -------------------------------------------------------------------

df_pkmn <- readr::read_rds(here::here("dados/pkmn.rds")) %>% 
  filter(id_geracao != 0, !is.na(url_imagem))

df_cores <- df_pkmn %>% 
  distinct(tipo_1, cor_1)

geracoes_nomes <- c(paste("Geração", 1:6))
geracoes_valores <- c(1:6)

tipos <- df_pkmn %>%
  filter(!is.na(tipo_1)) %>% 
  distinct(tipo_1) %>% 
  arrange(tipo_1) %>% 
  purrr::flatten_chr()


# Funções -----------------------------------------------------------------

criar_value_box <- function(tab, var, titulo, icone, uni) {
  
  tab_filtrada <- tab() %>% 
    filter({{var}} == max({{var}}, na.rm = TRUE)) %>% 
    slice(1)
  
  valor <- dplyr::pull(tab_filtrada, {{var}})
  
  infoBox(
    value = paste(valor, uni),
    subtitle = tab_filtrada$pokemon,
    title = titulo,
    icon = icon(icone),
    color = "red",
    fill = TRUE
  )
  
}

pegar_imagem <- function(tab, var) {
  
  id <- tab() %>% 
    filter({{var}} == max({{var}}, na.rm = TRUE)) %>% 
    pull(id) |> 
    stringr::str_pad(width = 3, side = "left", pad = "0") |> 
    first()
  
  url <- glue::glue(
    "https://raw.githubusercontent.com/HybridShivam/Pokemon/master/assets/images/{id}.png"
  )
  
  img(
    src = url,
    width = 100,
    height = 100
  )
  
}

# Shiny ui ----------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Pokemon Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Visão geral", tabName = "visao_geral"),
      menuItem(text = "Por tipo", tabName = "por_tipo"),
      menuItem(text = "Comparando tipos", tabName = "comp_tipos"),
      tags$hr(),
      shiny::checkboxGroupInput(
        inputId = "geracoes",
        label = "Gerações",
        choices = purrr::set_names(geracoes_valores, geracoes_nomes),
        selected = 1
      ),
      actionButton(inputId = "botao_geracoes", label = "Incluir")
    )
  ),
  dashboardBody(
    tags$link(
      rel = "stylesheet", 
      type = "text/css",  
      href="https://use.fontawesome.com/releases/v5.3.1/css/all.css"
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(
        tabName = "visao_geral",
        h2("Visão geral"),
        fluidRow(
          box(
            title = "Quantidade por tipo",
            width = 6,
            plotOutput(outputId = "bar_plot", height = "550px")
          ),
          column(
            width = 6,
            fluidRow(
              infoBoxOutput(outputId = "vg_peso", width = 8),
              uiOutput(outputId = "vg_peso_imagem", height = "100px"),
              infoBoxOutput(outputId = "vg_altura", width = 8),
              uiOutput(outputId = "vg_altura_imagem", height = "100px"),
              infoBoxOutput(outputId = "vg_hp", width = 8),
              uiOutput(outputId = "vg_hp_imagem", height = "100px"),
              infoBoxOutput(outputId = "vg_ataque", width = 8),
              uiOutput(outputId = "vg_ataque_imagem", height = "100px"),
              infoBoxOutput(outputId = "vg_defesa", width = 8),
              uiOutput(outputId = "vg_defesa_imagem", height = "100px"),
              infoBoxOutput(outputId = "vg_velocidade", width = 8),
              uiOutput(outputId = "vg_velocidade_imagem", height = "100px")
            )
          )
        )
      ),
      tabItem(
        tabName = "por_tipo",
        h2("Visão por tipo"),
        fluidRow(
          box(
            width = 12,
            fluidRow(
              column(
                width = 3,
                selectInput(
                  inputId = "tipo_selecionado",
                  label = "Tipo",
                  choices = tipos,
                  selected = "grama"
                )
              ),
              column(
                width = 3,
                infoBoxOutput("n_apenas_primeiro_tipo", width = 12)
              ),
              column(
                width = 3,
                infoBoxOutput("n_primeiro_tipo", width = 12)
              ),
              column(
                width = 3,
                infoBoxOutput("n_segundo_tipo", width = 12)
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            fluidRow(
              infoBoxOutput(outputId = "por_tipo_peso", width = 8),
              uiOutput(outputId = "por_tipo_peso_imagem"),
              infoBoxOutput(outputId = "por_tipo_altura", width = 8),
              uiOutput(outputId = "por_tipo_altura_imagem"),
              infoBoxOutput(outputId = "por_tipo_hp", width = 8),
              uiOutput(outputId = "por_tipo_hp_imagem")
            )
          ),
          column(
            width = 6,
            fluidRow(
              infoBoxOutput(outputId = "por_tipo_ataque", width = 8),
              uiOutput(outputId = "por_tipo_ataque_imagem"),
              infoBoxOutput(outputId = "por_tipo_defesa", width = 8),
              uiOutput(outputId = "por_tipo_defesa_imagem"),
              infoBoxOutput(outputId = "por_tipo_velocidade", width = 8),
              uiOutput(outputId = "por_tipo_velocidade_imagem")
            )
          )
        ),
        fluidRow(
          box(
            title = "Distribuição dos status",
            width = 12,
            plotOutput(outputId = "boxplot_por_tipo", height = "550px")
          )
        )
      ),
      tabItem(
        tabName = "comp_tipos",
        h2("Comparanto tipos"),
        fluidRow(
          box(
            width = 12,
            fluidRow(
              column(
                width = 3,
                selectInput(
                  inputId = "tipo_selecionado_comp_1",
                  label = "Tipo 1",
                  choices = tipos,
                  selected = "água"
                )
              ),
              column(
                width = 3,
                selectInput(
                  inputId = "tipo_selecionado_comp_2",
                  label = "Tipo 2",
                  choices = tipos,
                  selected = "fogo"
                )
              )
            )
          )
        ),
        plotOutput("boxplot_comp_tipos")
      )
    )
  )
)


# Shiny server ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Base filtrada
  
  df_gen <- eventReactive(input$botao_geracoes, ignoreNULL = FALSE, {
    req(input$geracoes)
    df_pkmn %>% 
      filter(id_geracao %in% input$geracoes)
  })
  
  df_tipo <- reactive({
    
    df_ <- df_gen() %>% 
      filter(
        tipo_1 == input$tipo_selecionado | 
          tipo_2 == input$tipo_selecionado
      )
    
    if(nrow(df_) == 0) {
      req(NULL)
    }
    else {
      df_
    }
    
  })
  
  # Aba "Comparando tipos"
  
  output$boxplot_comp_tipos <- renderPlot({
    
    df_tipo_1 <- df_gen() %>% 
      filter(
        tipo_1 == input$tipo_selecionado_comp_1 | 
          tipo_2 == input$tipo_selecionado_comp_1
      ) %>% 
      mutate(tipo = input$tipo_selecionado_comp_1)
    
    df_tipo_2 <- df_gen() %>% 
      filter(
        tipo_1 == input$tipo_selecionado_comp_2 | 
          tipo_2 == input$tipo_selecionado_comp_2
      ) %>% 
      mutate(tipo = input$tipo_selecionado_comp_2)
    
    
    cores <- c(
      df_cores$cor_1[df_cores$tipo_1 == input$tipo_selecionado_comp_1],
      df_cores$cor_1[df_cores$tipo_1 == input$tipo_selecionado_comp_2]
    )
    
    cores <- purrr::set_names(
      cores, 
      c(input$tipo_selecionado_comp_1, input$tipo_selecionado_comp_2)
    )
    
    bind_rows(df_tipo_1, df_tipo_2) %>% 
      tidyr::pivot_longer(
        names_to = "stats",
        values_to = "valor",
        cols = c(
          hp,
          ataque,
          defesa,
          velocidade,
          ataque_especial,
          defesa_especial
        )
      ) %>% 
      ggplot(aes(x = stats, y = valor, fill = tipo)) +
      geom_boxplot() +
      scale_fill_manual(values = cores) +
      theme_minimal()
    
  })
  
  # Aba "Por tipo"
  
  output$n_apenas_primeiro_tipo <- renderInfoBox({
    
    n <- df_gen() %>% 
      filter(tipo_1 == input$tipo_selecionado, is.na(tipo_2)) %>% 
      nrow()
    
    infoBox(
      value = n,
      title = paste("Apenas", input$tipo_selecionado),
      icon = icon("hashtag"),
      color = "blue",
      fill = TRUE
    )
    
  })
  
  output$n_primeiro_tipo <- renderInfoBox({
    
    n <- df_gen() %>% 
      filter(tipo_1 == input$tipo_selecionado, !is.na(tipo_2)) %>% 
      nrow()
    
    infoBox(
      value = n,
      title = paste("Primeiro tipo"),
      icon = icon("hashtag"),
      color = "blue",
      fill = TRUE
    )
    
  })
  
  output$n_segundo_tipo <- renderInfoBox({
    
    n <- df_gen() %>% 
      filter(tipo_2 == input$tipo_selecionado) %>% 
      nrow()
    
    infoBox(
      value = n,
      title = paste("Segundo tipo"),
      icon = icon("hashtag"),
      color = "blue",
      fill = TRUE
    )
    
  })
  
  output$boxplot_por_tipo <- renderPlot({
    
    df_tipo() %>% 
      tidyr::pivot_longer(
        names_to = "stats", 
        values_to = "valor", 
        cols =  c(hp, ataque, defesa, velocidade, ataque_especial, defesa_especial)
      ) %>% 
      ggplot(aes(x = stats, y = valor)) + 
      geom_boxplot() + 
      theme_minimal()
    
  })
  
  # Peso
  output$por_tipo_peso <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_tipo, 
      var = peso, 
      uni = "Kg",
      titulo = "Mais pesado",
      icone = "weight-hanging"
    )
    
  })
  
  output$por_tipo_peso_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_tipo, 
      var = peso
    )
  })
  
  
  # Altura
  output$por_tipo_altura <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_tipo, 
      var = altura, 
      uni = "m",
      titulo = "Mais alto",
      icone = "up-long"
    )
    
  })
  
  output$por_tipo_altura_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_tipo, 
      var = altura
    )
  })
  
  
  
  # HP
  
  output$por_tipo_hp <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_tipo, 
      var = hp, 
      uni = "",
      titulo = "Maior HP",
      icone = "heart"
    )
    
  })
  
  output$por_tipo_hp_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_tipo, 
      var = hp
    )
  })
  
  # Ataque
  
  output$por_tipo_ataque <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_tipo, 
      var = ataque, 
      uni = "",
      titulo = "Maior ataque",
      icone = "hand-back-fist"
    )
    
  })
  
  output$por_tipo_ataque_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_tipo, 
      var = ataque
    )
  })
  
  
  
  # Defesa
  
  output$por_tipo_defesa <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_tipo, 
      var = defesa, 
      uni = "",
      titulo = "Maior defesa",
      icone = "shield"
    )
    
  })
  
  output$por_tipo_defesa_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_tipo, 
      var = defesa
    )
  })
  
  # Velocidade
  
  output$por_tipo_velocidade <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_tipo, 
      var = velocidade, 
      uni = "",
      titulo = "Mais rápido",
      icone = "gauge-simple-high"
    )
    
  })
  
  output$por_tipo_velocidade_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_tipo, 
      var = velocidade
    )
  })
  
  # Aba "Visão geral"
  
  output$bar_plot <- renderPlot({
    
    cores <- purrr::set_names(df_cores$cor_1, df_cores$tipo_1)
    
    df_gen() %>%
      tidyr::pivot_longer(
        names_to = "posicao",
        values_to = "tipo",
        cols = c(tipo_1, tipo_2)
      ) %>%
      filter(!is.na(tipo)) %>%
      count(tipo) %>% 
      mutate(tipo = forcats::fct_reorder(tipo, n)) %>% 
      ggplot(aes(x = tipo, y = n, fill = tipo)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = cores) +
      theme_minimal()
    
  })
  
  # Peso
  output$vg_peso <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_gen, 
      var = peso, 
      uni = "Kg",
      titulo = "Mais pesado",
      icone = "weight-hanging"
    )
    
  })
  
  output$vg_peso_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_gen, 
      var = peso
    )
  })
  
  
  # Altura
  output$vg_altura <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_gen, 
      var = altura, 
      uni = "m",
      titulo = "Mais alto",
      icone = "up-long"
    )
    
  })
  
  output$vg_altura_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_gen, 
      var = altura
    )
  })
  
  
  
  # HP
  
  output$vg_hp <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_gen, 
      var = hp, 
      uni = "",
      titulo = "Maior HP",
      icone = "heart"
    )
    
  })
  
  output$vg_hp_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_gen, 
      var = hp
    )
  })
  
  # Ataque
  
  output$vg_ataque <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_gen, 
      var = ataque, 
      uni = "",
      titulo = "Maior ataque",
      icone = "hand-back-fist"
    )
    
  })
  
  output$vg_ataque_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_gen, 
      var = ataque
    )
  })
  
  
  
  # Defesa
  
  output$vg_defesa <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_gen, 
      var = defesa, 
      uni = "",
      titulo = "Maior defesa",
      icone = "shield"
    )
    
  })
  
  output$vg_defesa_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_gen, 
      var = defesa
    )
  })
  
  # Velocidade
  
  output$vg_velocidade <- renderInfoBox({
    
    req(input$geracoes)
    
    criar_value_box(
      tab = df_gen, 
      var = velocidade, 
      uni = "",
      titulo = "Mais rápido",
      icone = "gauge-simple-high"
    )
    
  })
  
  output$vg_velocidade_imagem <- renderUI({
    req(input$geracoes)
    pegar_imagem(
      tab = df_gen, 
      var = velocidade
    )
  })
  
}


shinyApp(ui, server)