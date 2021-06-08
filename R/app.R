rm(list = ls())
library(shiny)
library(dplyr)

source(file = "./tab_sobre.R")
source(file = "./tab_membros.R")

# https://davidgohel.github.io/ggiraph/articles/offcran/shiny.html
# https://shiny.rstudio.com/gallery/brazil-voter-profile.html

# Insert logs

ui <- shinyUI(
  shiny::fluidPage(
    navbarPage(
      title = "Complexity-Inequality", 
      id = "page_id", 
      selected = "app",
      
      tabPanel(
        title = "Home", 
        value = "home",
        column(width = 10, offset = 1,
          h3("The interdisciplinary research group on complexity, development and inequality at the Federal University of Santa Catarina makes use of new methods from network science, econometrics and economic complexity to understand the challenges of smart diversification, inequality and inclusive growth in Brazil and across the world."),
          br(),
          h3("Our team [Link to Team Members] views economies as complex evolving systems and explores the boundaries of economics with other disciplines, such as data science, geography, and innovation studies to getting a better understanding on dynamic socioeconomic development processes and challenges in the digital age."),
          br(),
          h3("We collaborate with leading researchers from several countries and regions of Brazil on topics such as structural change and labor market dynamics, inequality and poverty dynamics, and economic catch-up and leapfrogging ahead approaches [Link to Publications]. The complexity explorer application allows to map and download data on education, complexity, inequality and social efficiency values across Brazil and the world [Link to App]."),
          br(),
          h3("Bem-vindo to our group’s webpage, projects and applications.")
        )
      ),
      
      tabPanel(
        title = "App", 
        value = "app",
        shiny::fluidPage(
          shiny::column(
            width = 2,
            shiny::fluidRow(
              shiny::br(),
              shiny::uiOutput(outputId = "input_ui_1"),
              shiny::br(),
              shiny::uiOutput(outputId = "input_ui_2"),
              shiny::br(),
              shiny::uiOutput(outputId = "input_ui_3"),
              shiny::br(),
              shiny::uiOutput(outputId = "input_ui_4"),
              shiny::br(),
              shiny::actionButton(
                inputId = "goButton", width = "100%", strong("Processar"), #width = "320px",
                style="display: inline-block; align-items: center;justify-content: center;float:center;padding-bottom:13px;border-radius: 30px;"
              )
            )
          ),
          shiny::column(width = 1),
          shiny::column(
            width = 8,
            shiny::fluidRow(
              tabsetPanel(
                tabPanel(title = "geom_sf", shiny::plotOutput(outputId = "plot1")),
                # tabPanel(title = "geom_sf_interactive", ggiraph::girafeOutput(outputId = "plot2")),
                tabPanel(title = "density plot", shiny::plotOutput(outputId = "plot3")),
                tabPanel(title = "Tabela", shiny::tableOutput(outputId = "table1"))
              )
            ),
            shinydashboard::box(
              title = "Detalhes", 
              background = "light-blue", 
              solidHeader = F, 
              shiny::textOutput("info1"),
              h5("Detailed content, such as max, min, mean, median, sd, M1, ...")
            )
          ),
          shiny::column(width = 1),
        )
      ),

      tabPanel(
        title = "Publicações", 
        value = "publications",
        br(),
        h5("Aqui será uma vitrine para as principais publicações dos integrantes do grupo relacionadas à complexidade economica e desigualdade")
      ),
      
      tabPanel(
        title = "Metodologia", 
        value = "met",
        br(),
        h4("Here we are gonna explain the methodology, methods and tools used in the app and by the group")
      ),
      
      tabPanel(
        title = "Membros",
        value = "members",
        br(),
        h3("Aqui iremos apresentar os integrantes do grupo"),
        uiOutput(outputId = "authors_test")
      ),
      
      # membros,
      
      tabPanel(
        title = "Mais", 
        value = "more",
        h5("Aqui iremos inserir qualquer adicional que não nas outras abas")
      ),
      
      sobre
    )
  )
)




server <- function(input, output){
  
  options(stringsAsFactors = F)
  
  mongo_credentials <- config::get(file = "../conf/globalresources.yml")
  
  # option_estados <- readr::read_csv("./data/options/option_estados.csv")
  option_loc <- readr::read_csv("../data/options/option_loc.csv")
  option_stats <- readr::read_csv("../data/options/option_stats.csv")
  
  # Global variables
  ## Tratar e salvar prontas localmente :::::::::::::::::;
  ### insert into a list
  #### Check GEOjson in mongoDB performance
  ##### Dowload series button with zero value to empty regions
  shp_uf <- sf::st_read("../data/shp/shp_uf/")
  shp_meso <- sf::st_read("../data/shp/shp_meso/")
  shp_rgint <- sf::st_read("../data/shp/shp_rgint/")
  shp_micro <- sf::st_read("../data/shp/shp_micro/")
  shp_rgime <- sf::st_read("../data/shp/shp_rgime/")
  
  output$input_ui_1 <- shiny::renderUI({
    choices1 <- as.list(option_loc %>% select(sg_rg) %>% unique() %>% pull())
    names(choices1) <- option_loc %>% select(nm_rg) %>% unique() %>% pull()
    shinyWidgets::pickerInput(
      inputId = "input_server_1",
      label = "Selecione a região",
      choices = choices1,
      multiple = F,
      selected = choices1[1]
    )
  })
  
  output$input_ui_2 <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_server_2",
      label = "Selecione a divisão territorial",
      choices = list(
        "Estadual" = "uf",
        "Mesorregional" = "meso", 
        "Microrregional" = "micro", 
        "Intermediária" = "rgint", 
        "Imediata" = "rgime"
      ),
      multiple = FALSE,
      selected = "uf"
    )
  })
  
  output$input_ui_3 <- shiny::renderUI({
    choices3 <- as.list(option_stats %>% select(cd_tema) %>% unique() %>% pull())
    names(choices3) <- option_stats %>% select(nm_tema) %>% unique() %>% pull()
    shinyWidgets::pickerInput(
      inputId = "input_server_3",
      label = "Selecione o tema da análise",
      choices = choices3,
      multiple = FALSE,
      selected = "eci"
    )
  })
  
  output$input_ui_4 <- shiny::renderUI({
    choices4 <- as.list(option_stats %>% filter(cd_tema==input$input_server_3) %>% select(cd_stat) %>% pull())
    names(choices4) <- option_stats %>% filter(cd_tema==input$input_server_3) %>% select(nm_stat) %>% pull()
    shinyWidgets::pickerInput(
      inputId = "input_server_4",
      label = "Selecione a estatística",
      choices = choices4,
      multiple = FALSE,
      selected = choices4[1]
    )
  })
  
  reac_shp <- eventReactive(input$goButton, {
    switch(input$input_server_2,
           "uf" = {shp_df <- shp_uf},
           "meso" = {shp_df <- shp_meso},
           "rgint" = {shp_df <- shp_rgint},
           "micro" = {shp_df <- shp_micro},
           "rgime" = {shp_df <- shp_rgime},
           stop("Nope")
    )
    if(input$input_server_1!="BR"){
      shp_df <- shp_df %>%
        dplyr::filter(sg_rg==input$input_server_1)
    } else {shp_df}
    shp_df
  })

  reac_query <- shiny::eventReactive(input$goButton, {
    colec = paste0("colec_", input$input_server_2)
    mongo_set <- mongolite::mongo(db = "db1", collection = colec, url = mongo_credentials$mongoURL, verbose = TRUE)
    df <- mongo_set$find(paste0('{"product" : ', paste0('"', input$input_server_4, '"'), '}'))
    if(input$input_server_1!="BR"){ # melhorar com vars() depois
      df <- df %>%
        dplyr::filter(sg_rg==input$input_server_1)
    } else {df}
    df
  })

  react_df <- shiny::eventReactive(input$goButton, {
    df_shp <- dplyr::full_join(
      reac_query(),
      reac_shp()
    ) %>% sf::st_sf()
  })

  output$plot1 <- shiny::renderPlot({
    ggplot2::ggplot(react_df())+
      # ggplot2::geom_sf(ggplot2::aes(0), color="black", size=.13)+
      ggplot2::geom_sf(ggplot2::aes(fill=value), color="black", size=.2)+
      ggplot2::scale_fill_gradient(low="white", high="blue")+
      ggplot2::labs(title = "", caption = "", y = "Latitude", x = "Longitude")+
      ggplot2::theme_void()
  })

  output$plot2 <- ggiraph::renderGirafe({ # CSS Loader
    dfr1 <- react_df()
    rownames(dfr1) <- dfr1$cd_meso
    gg2 <- ggplot2::ggplot(dfr1,
      ggplot2::aes(
        fill=dfr1$value,
        tooltip=dfr1$cd_meso,
        data_id=dfr1$cd_meso
      )
    )+
      ggiraph::geom_sf_interactive(color="black", size=.2)+
      ggplot2::scale_fill_gradient(low="white", high="blue")+
      ggplot2::labs(title = "", caption = "", y = "Latitude", x = "Longitude")+
      ggplot2::theme_void()
    ggiraph::girafe(ggobj = gg2)
  })

  output$plot3 <- shiny::renderPlot({
    dfr1 <- react_df()
    ggplot2::ggplot(dfr1)+
      ggplot2::geom_density(aes(value))+
      ggplot2::theme_void()
  })

  output$info1 <- shiny::renderText({
    vl <- reac_query()$value
    paste0(
      "Média: ", round(mean(vl), 3), "; ",
      "Mediana: ", round(median(vl), 3), "; ",
      "Desvio Padrão: ", round(sd(vl), 3), "; ",
      "Variância: ", round(var(vl), 3), "; ",
      "Máximo: ", round(max(vl), 3), "; ",
      "Mínimo: ", round(min(vl), 3)
    )
  })

  output$table1 <- shiny::renderTable({
    reac_query()
  })
  
  output$authors_test <- renderUI({
    df <- data.frame("a"=c("gui", "ori"), "b"=c("viegas", "ortiz"))
    num=2
    lapply(1:num, function(i) {
      div(h4(df[i, 1]), h5(df[i, 2]))
    })
  })
  
  
}

shinyApp(ui = ui, server = server)


