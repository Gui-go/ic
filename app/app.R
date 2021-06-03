# rm(list = ls())
library(shiny)
library(dplyr)

source(file = "tab_sobre.R")

# https://davidgohel.github.io/ggiraph/articles/offcran/shiny.html
# https://shiny.rstudio.com/gallery/brazil-voter-profile.html

# Insert logs

ui <- shinyUI(
  shiny::fluidPage(
    navbarPage(
      title = "Complexity-Inequality", 
      id = "np", 
      selected = "main",
      
      tabPanel(
        title = "Home", 
        value = "home"
      ),
      
      tabPanel(
        title = "Main", 
        value = "main",
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
              h5("Detailed content, such as max, min, mean, median, sd, M1, ...")
            )
          ),
          shiny::column(width = 1),
        )
      ),
      
      tabPanel(
        title = "Artigos Interativos", 
        value = "art_int"
      ),
      
      tabPanel(
        title = "Recomendações", 
        value = "recom"
      ),
      
      tabPanel(
        title = "Metodologia", 
        value = "met"
      ),
      
      tabPanel(
        title = "Mais", 
        value = "more"
      ),
      
      sobre
    )
  )
)




server <- function(input, output){
  
  options(stringsAsFactors = F)
  
  mongo_credentials <- config::get(file = "../conf/globalresources.yml")
  
  # option_estados <- readr::read_csv("data/option_estados.csv")
  option_loc <- readr::read_csv("../data/option_loc.csv")
  option_stats <- readr::read_csv("../data/option_stats.csv")
  
  # Global variables
  ## Tratar e salvar prontas localmente :::::::::::::::::;
  ### insert into a list
  #### Check GEOjson in mongoDB performance
  ##### Dowload series button with zero value to empty regions
  shp_ufs <- sf::st_read("../data/shp/shp_ufs/")
  shp_meso <- sf::st_read("../data/shp/shp_meso/")
  
  # shp_meso <- sf::st_read("data/shp/BR_Mesorregioes_2020/") %>%
  #   janitor::clean_names() %>%
  #   sf::st_set_crs(4326) %>%
  #   dplyr::mutate(cd_meso = as.character(cd_meso))
  # 
  # shp_micro <- sf::st_read("data/shp/BR_Microrregioes_2020/") %>%
  #   janitor::clean_names() %>%
  #   sf::st_set_crs(4326) %>%
  #   dplyr::mutate(cd_micro = as.character(cd_micro))
  
  # shp_int
  # shp_ime
  
  
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
        "Intermediária" = "int", 
        "Imediata" = "ime"
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
           "uf" = {shp_df <- shp_ufs},
           "meso" = {shp_df <- shp_meso},
           "micro" = {shp_df <- shp_micro},
           stop("Nope")
    )
    if(input$input_server_1!="BR"){
      shp_df <- shp_df %>%
        dplyr::filter(sg_rg==input$input_server_1)
    } else {shp_df}
    shp_df
  })
  
  reac_query <- shiny::eventReactive(input$goButton, {
    colec = paste0("colec_", input$input_server_2, "_exp_eci")
    mongo_set <- mongolite::mongo(db = "db1", collection = colec, url = mongo_credentials$mongoURL, verbose = TRUE)
    df <- mongo_set$find(paste0('{"product" : ', paste0('"', input$input_server_4, '"'), '}'))
    if(input$input_server_1!="BR"){ # melhorar com vars() depois
      df <- df %>%
        dplyr::filter(sg_rg==input$input_server_1)
    } else {df}
    df
  })
  
  react_df <- shiny::eventReactive(input$goButton, {
    df_shp <- dplyr::inner_join(
      reac_query(), 
      reac_shp()
    ) %>% sf::st_sf()
  })
  
  output$plot1 <- shiny::renderPlot({
    print("react_df()")
    dplyr::glimpse(react_df())
    ggplot2::ggplot(react_df())+
      ggplot2::geom_sf(ggplot2::aes(fill=value), color="black", size=.2)+
      ggplot2::scale_fill_gradient(low="white", high="blue")+
      ggplot2::labs(title = "", caption = "", y = "Latitude", x = "Longitude")+
      ggplot2::theme_void()
  })
  
  output$plot2 <- ggiraph::renderGirafe({
    dfr1 <- react_df()
    rownames(dfr1) <- dfr1$cd_uf
    gg2 <- ggplot2::ggplot(
      dfr1, 
      ggplot2::aes(
        fill=dfr1$value, 
        tooltip=dfr1$cd_uf, 
        data_id=dfr1$cd_uf
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
    ggplot(dfr1)+
      geom_density(aes(value))+
      theme_void()
  })
  
  output$table1 <- shiny::renderTable({
    reac_query()
  })
  
  
}

shinyApp(ui = ui, server = server)


