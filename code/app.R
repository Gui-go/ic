if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
# if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
# if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
# if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
# if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
# if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
# if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")



ui <- shiny::bootstrapPage(
  
  tags$head(
    tags$style(HTML("
      #sec_graphs {
      background-color: rgba(200,200,200,0.75);
      z-index: 10000;
      padding: 4px;
      border: 1px solid black;
      box-sizing: border-box;
      border-radius: 6px;
      }
      #controls {
      background-color: rgba(128,128,128,0.6);
      z-index: 10000;
      }"))),
  
  shiny::navbarPage(
    title = "Spillover explorer", 
    id = "nav", 
    selected = "mapa", 
    theme = shinythemes::shinytheme("flatly"),
    collapsible = TRUE,
    windowTitle = "Spillover explorer",
    
    tabPanel(
      title = "mapa",
      leafletOutput("mymap"),
      
      absolutePanel(
        id = "controls", #class = "panel panel-default",
        top = 75, left = 55, width = 250, fixed=TRUE,
        draggable = TRUE, height = "auto",
        shiny::selectInput(
          inputId = "input2", 
          label = "Selecione a região", 
          choices = c("Norte", "Nordeste", "Centro-oeste", "Sudeste", "Sul"), 
          selected = "Sul", 
          multiple = FALSE
        ),
        shiny::selectInput(
          inputId = "input8", 
          label = "Selecione a divisão", 
          choices = c("estadual", "Mesorregional", "Microrregional"), 
          selected = "Mesorregional", 
          multiple = FALSE
        ),
        shiny::selectInput(
          inputId = "input8", 
          label = "Selecione a frequência", 
          choices = c("Absoluta", "Relativa"), 
          selected = "Mesorregional", 
          multiple = FALSE
        ),
        shiny::selectInput(
          inputId = "input8", 
          label = "Selecione a estatística", 
          choices = c("Quantidade de habitantes", "Quantidade de trabalhadores", "Quantidade de trabalhadores no setor X1", "Proporção trabalhadores do setor X1"), 
          selected = "Proporção trabalhadores do setor X1", 
          multiple = FALSE
        ),
        shiny::sliderInput(
          inputId = "input9", 
          label = "Selecione o intervalo temporal", 
          min = 1970, 
          max = 2020, 
          value = c(2010, 2020), 
          step = 1, 
          ticks = FALSE
        )
      ),
      
      absolutePanel(
        id = "sec_graphs",
        # id = "controls",
        top = 75, right = 55, width = 400, fixed = TRUE,
        draggable = TRUE, height = "auto",
        tabsetPanel(
          tabPanel(h6("Distribuição")),
          tabPanel(h6("Correlação")),
          tabPanel(h6("Regressão")),
          tabPanel(h6("Série temporal")),
          tabPanel(h6("Rede"))
        ),
        shiny::selectInput(
          inputId = "input2", 
          label = "Selecione o indicador", 
          choices = c("Quantidade de trabalhadores", "Quantidade de empresas"), 
          selected = "Quantidade de trabalhadores", 
          multiple = FALSE
        ),
        shiny::selectInput(
          inputId = "input7", 
          label = "Selecione o gráfico", 
          choices = c("Gráfico de pontos", "Gráfico de linhas", "Boxplot", "Densidade"), 
          selected = "Quantidade de trabalhadores", 
          multiple = FALSE
        ),
        plotOutput(outputId = "plot1")
      )
      
    ),
    
    tabPanel(
      title = "Dados",
      h4("Os dados utilizados foram...")
    ),
    
    tabPanel(
      title = "Sobre",
      h3("Web app do projeto de iniciação ciêntifica"),
      h5("Universidade Federal de Santa Catarina - UFSC"),
      h6("Guilherme Viegas"),
      h6("Dominik Hartmann")
    )
    
  )
)

server = function(input, output, session){
  
  output$plot1 <- renderPlot({
    df <- data.frame(a=c(1, 3, 5, 8, 13), b=c(-1, -3, -5, -8, -13))
    ggplot(df)+
      geom_line(aes(x=a, y=b))+
      theme_minimal()
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = -48.521523, lat = -27.599015, popup = 'UFSC') %>%
      setView(lng = -48.521523, lat = -27.599015, zoom = 4)
  })
  
}

shinyApp(ui, server)


# Examples
# https://github.com/eparker12/nCoV_tracker/blob/master/app.R
# https://shiny.rstudio.com/gallery/crime-watch.html