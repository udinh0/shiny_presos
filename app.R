library(shiny)
library(datasets)
library(DT)
library(readr)
library(tidyverse)
library(plotly)
library(leaflet)
library(geobr)
library(htmlwidgets)
library(shinycssloaders)
library(glue)
library(yaml)

dados <- readr::read_csv("dados.csv")
dados <- dados %>% filter(Estado != "Brasil")
config <- yaml::read_yaml("config.yaml")
dados[dados$Estado == "Amazonas",]$Estado = "Amazônas"
dados[dados$Estado == "Rio Grande do Norte",]$Estado = "Rio Grande Do Norte"
dados[dados$Estado == "Espírito Santo (4)",]$Estado ="Espírito Santo"
dados[dados$Estado == "Rio Grande do Sul",]$Estado ="Rio Grande Do Sul"
dados[dados$Estado == "Mato Grosso do Sul",]$Estado ="Mato Grosso Do Sul"
dados[dados$Estado == "Rio de Janeiro",]$Estado ="Rio De Janeiro"
dados <- dados %>% mutate(Valor = ifelse(is.na(Valor), 0, Valor))

ui <- function(request) {
  fluidPage(
    titlePanel("Painel Interativo de Análise do Sistema Prisional Brasileiro"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("sexo", label = "Sexo", choices = c("Ambos", "Feminino", "Masculino"), selected = "Ambos"),
        selectInput("estados", "Estado", choices = unique(dados$Estado), multiple = TRUE),
        sliderInput("ano", label = "Ano", min = 2018, max = 2023, value = c(2018, 2023), sep = ""),
        selectInput("tipo_grafico", label = "Tipo do gráfico", choices = c("Linhas", "Barras")),
        radioButtons("escala", "Escala", choices = c("Normal", "Logarítmica"), selected = "Logarítmica"),
        actionButton("reset_filters", "Resetar Filtros"),
        bookmarkButton()
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Tabela", DT::dataTableOutput("tabela"), downloadButton("download_tabela", "Baixar Tabela")),
          tabPanel("Gráficos", plotlyOutput("grafico_interativo"), downloadButton("download_grafico", "Baixar Gráfico")),
          tabPanel("Mapa", withSpinner(leafletOutput("mapa")), downloadButton("download_mapa", "Baixar Mapa"))
        )
      )
    )
  )
}

server <- function(input, output, session) {
  # Dados filtrados de acordo com os inputs
  filtered_data <- reactive({
    new_data <- dados
    
    if (input$sexo != "Ambos") {
      new_data <- new_data[new_data$Categoria == input$sexo, ]
    }
    if (length(input$estados) > 0) {
      new_data <- new_data[new_data$Estado %in% input$estados, ]
    }
    new_data <- new_data[new_data$Ano >= input$ano[1] & new_data$Ano <= input$ano[2], ]
    new_data
  })
  
  # Função para aplicar a transformação logarítmica
  transform_data <- function(data, escala) {
    if (escala == "Logarítmica") {
      data$Valor <- log1p(data$Valor) # Usando log1p para lidar com valores 0
    }
    data
  }
  
  # Tabela
  output$tabela <- DT::renderDataTable({
    data_to_display <- transform_data(filtered_data(), input$escala)
    DT::datatable(data_to_display)
  }, options = list(pageLength = config$numero_linhas))
  
  output$download_tabela <- downloadHandler(
    filename = function() { paste("tabela_filtrada", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Gráfico interativo
  output$grafico_interativo <- renderPlotly({
    dados_grafico <- filtered_data() %>%
      group_by(Estado, Ano) %>%
      summarise(Valor = sum(Valor, na.rm = TRUE), .groups = 'drop')
    
    # Aplica a transformação de escala
    dados_grafico <- transform_data(dados_grafico, input$escala)
    
    if (input$tipo_grafico == "Linhas") {
      plot_ly(dados_grafico, 
              x = ~Ano, 
              y = ~Valor, 
              color = ~Estado, 
              type = 'scatter', 
              mode = 'lines+markers',
              text = ~paste("Estado:", Estado, "<br>Ano:", Ano, "<br>Valor:", Valor),
              hoverinfo = 'text') %>%
        layout(
          title = list(
            text = glue("Valores por Estado ao Longo dos Anos ({input$ano[1]}-{input$ano[2]})"),
            x = 0.5,
            xanchor = "center"
          ),
          annotations = list(
            list(
              x = 0.5,
              y = 0.97,
              text = glue("Sexo: {input$sexo}"),
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              font = list(size = 14, color = "black")
            )
          ),
          xaxis = list(title = "Ano"),
          yaxis = list(title = "Valor Total", type = ifelse(input$escala == "Logarítmica", "log", "linear")),
          legend = list(title = list(text = "Estado"))
        )
    } else if (input$tipo_grafico == "Barras") {
      plot_ly(dados_grafico, 
              x = ~Ano, 
              y = ~Valor, 
              color = ~Estado, 
              type = 'bar',
              text = ~paste("Estado:", Estado, "<br>Ano:", Ano, "<br>Valor:", Valor),
              hoverinfo = 'text') %>%
        layout(
          title = list(
            text = glue("Valores por Estado ao Longo dos Anos ({input$ano[1]}-{input$ano[2]})"),
            x = 0.5,
            xanchor = "center"
          ),
          annotations = list(
            list(
              x = 0.5,
              y = 0.97,
              text = glue("Sexo: {input$sexo}"),
              showarrow = FALSE,
              xref = "paper",
              yref = "paper",
              xanchor = "center",
              yanchor = "bottom",
              font = list(size = 14, color = "black")
            )
          ),
          xaxis = list(title = "Ano"),
          yaxis = list(title = "Valor Total", type = ifelse(input$escala == "Logarítmica", "log", "linear")),
          legend = list(title = list(text = "Estado"))
        )
    }
  })
  
  output$download_grafico <- downloadHandler(
    filename = function() { paste("grafico_interativo", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      plotly_IMAGE(output$grafico_interativo(), format = "png", out_file = file)
    }
  )
  
  # Mapa interativo
  output$mapa <- renderLeaflet({
    mapa_estados <- geobr::read_state(year = 2020)
    dados_mapa <- filtered_data() %>%
      group_by(Estado) %>%
      summarise(Valor = sum(Valor, na.rm = TRUE))
    mapa_estados <- mapa_estados %>%
      left_join(dados_mapa, by = c("name_state" = "Estado"))
    
    # Aplica a transformação de escala
    mapa_estados <- transform_data(mapa_estados, input$escala)
    
    pal <- colorNumeric(
      palette = colorRampPalette(c("yellow", "orange", "red"))(100), # Cria uma paleta de transição
      domain = mapa_estados$Valor,  # Define o domínio das variáveis
      na.color = "transparent"            
    )    
    leaflet(mapa_estados) %>%
      addTiles() %>%
      addPolygons(fillColor = ~pal(Valor),
                  color = "black",
                  weight = 1,
                  opacity = 1,
                  fillOpacity = 0.7,
                  label = ~paste(name_state, "<br>Valor Total:", Valor),
                  highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = ~Valor, opacity = 0.7, title = "Valor Total", position = "bottomright")
  })
  
  output$download_mapa <- downloadHandler(
    filename = function() { paste("mapa_estados", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      saveWidget(output$mapa, file)
    }
  )
  
  # Função para resetar os filtros
  observeEvent(input$reset_filters, {
    updateRadioButtons(session, "sexo", selected = "Ambos")
    updateSelectInput(session, "estados", selected = character(0))
    updateSliderInput(session, "ano", value = c(2018, 2023))
    updateSelectInput(session, "tipo_grafico", selected = "Linhas")
    updateRadioButtons(session, "escala", selected = "Logarítmica")
  })
}

shinyApp(ui, server, enableBookmarking = "url")
