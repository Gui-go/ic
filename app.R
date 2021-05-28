library(shiny)

ui <- shiny::fluidPage(
  shiny::column(
    width = 3,
    shiny::fluidRow(
      shiny::uiOutput(outputId = "input_ui_1"),
      shiny::uiOutput(outputId = "input_ui_2"),
      shiny::uiOutput(outputId = "input_ui_3"),
      shiny::uiOutput(outputId = "input_ui_4"),
      shiny::actionButton(
        inputId = "goButton", width = "100%", strong("Processar"), #width = "320px",
        style="display: inline-block; align-items: center;justify-content: center;float:center;padding-bottom:13px;border-radius: 30px;"
      )
    )
  ),
  shiny::column(
    width = 9,
    shiny::fluidRow(
      shiny::plotOutput(outputId = "plot1")
    )
  )
)

server <- function(input, output){
  
  library(dplyr)
  
  mongo_credentials <- config::get(file = "conf/globalresources.yml")
  
  option_estados <- readr::read_csv("data/option_estados.csv")
  option_stats <- readr::read_csv("data/option_stats.csv")
  
  output$input_ui_1 <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_server_1",
      label = "Selecione os estados",
      choices = list(
        "Centro-Oeste" = list("Distrito Federal"=53, "Goiás"=52, "Mato Grosso"=51, "Mato Grosso do Sul"=50),
        "Nordeste" = list("Alagoas"=27, "Bahia"=29, "Ceará"=23, "Maranhão"=21, "Paraíba"=25, "Pernambuco"=26, "Piauí"=22, "Rio Grande do Norte"=24, "Sergipe"=28),
        "Norte" = list("Acre"=12, "Amapá"=16, "Amazonas"=13, "Pará"=15, "Rondônia"=11, "Roraima"=14, "Tocantins"=17),
        "Sudeste" = list("Espírito Santo"=32, "Minas Gerais"=31, "Rio de Janeiro"=33, "São Paulo"=35),
        "Sul" = list("Paraná"=41, "Rio Grande do Sul"=43, "Santa Catarina"=42)
      ),
      multiple = T,
      selected = option_estados$cd_uf
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
      # choices = list("ECI"="eci", "Educação"="edu", "Exportações"="exp", "Trabalho"="trab"),
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
  
  # Global variables
  ## Tratar e salvar prontas localmente :::::::::::::::::;
  shp_ufs <- sf::st_read("data/shp/BR_UF_2020/") %>%
    janitor::clean_names() %>%
    sf::st_set_crs(4326) %>%
    dplyr::mutate(cd_uf = as.character(cd_uf))
  
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
  
  reac_shp <- eventReactive(input$goButton, {
    switch(input$input_server_2,
           "uf" = {shp <- shp_ufs},
           "meso" = {shp <- shp_meso},
           "micro" = {shp <- shp_micro},
           stop("Nope")
    )
    shp_df <- shp %>% 
      dplyr::filter(cd_uf%in%input$input_server_1)
  })

  reac_query <- eventReactive(input$goButton, {
    mcolec = paste0("db1_", input$input_server_2) # mcolec é referente a escala
    qq <- paste0('{"product" : ', paste0('"', input$input_server_4, '"'), '}')
    mongo_set <- mongolite::mongo(db = "db1", collection = "colec_uf_exp_eci", url = mongo_credentials$mongoURL, verbose = TRUE)
    df <- mongo_set$find(qq)
  })
  
  react_df <- eventReactive(input$goButton, {
    df_shp <- dplyr::full_join(reac_query(), reac_shp()) %>% 
      sf::st_sf()
  })
  
  output$plot1 <- shiny::renderPlot({
    ggplot2::ggplot(react_df())+
      ggplot2::geom_sf(ggplot2::aes(fill=value), color="black", size=.2)+
      ggplot2::scale_fill_gradient(low="white", high="blue")+
      ggplot2::labs(title = "", caption = "", y = "Latitude", x = "Longitude")+
      ggplot2::theme_void()
  })
  
  
}

shinyApp(ui = ui, server = server)


