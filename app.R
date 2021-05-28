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
  
  mongo_credentials <- config::get(file = "conf/globalresources.yml")
  
  option_estados <- read_csv("data/option_estados.csv")
  
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
        "Estado" = "uf",
        "Mesorregião" = "meso", 
        "Intermediária" = "int", 
        "Microrregião" = "micro", 
        "Imediata" = "ime"
      ),
      multiple = FALSE,
      selected = "uf"
    )
  })
  
  output$input_ui_3 <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_server_3",
      label = "Selecione o tema da análise",
      choices = c("ECI", "Educação", "Exportações", "Trabalho"),
      multiple = FALSE,
      selected = "ECI"
    )
  })
  
  output$input_ui_4 <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_server_4",
      label = "Selecione a estatística",
      choices = c("Opções encadeadas de acordo com as opções acima", "opcao2"),
      multiple = FALSE,
      selected = "opcao2"
    )
  })
  
  # Global variables
  ## Tratar e salvar prontas localmente :::::::::::::::::;
  shp_ufs <- sf::st_read("data/shp/BR_UF_2020/") %>%
    janitor::clean_names() %>%
    sf::st_set_crs(4326) %>%
    dplyr::mutate(cd_uf = as.character(cd_uf))
  
  shp_meso <- sf::st_read("data/shp/BR_Mesorregioes_2020/") %>%
    janitor::clean_names() %>%
    sf::st_set_crs(4326) %>%
    dplyr::mutate(cd_meso = as.character(cd_meso))
  
  shp_micro <- sf::st_read("data/shp/BR_Microrregioes_2020/") %>%
    janitor::clean_names() %>%
    sf::st_set_crs(4326) %>%
    dplyr::mutate(cd_micro = as.character(cd_micro))
  
  # shp_meso
  # shp_int
  # shp_micro
  # shp_ime
  
  
  reac_shp <- eventReactive(input$goButton, {
    switch(input$input_server_2,
           "uf" = {shp <- shp_ufs},
           "meso" = {shp <- shp_meso},
           "micro" = {shp <- shp_micro},
           stop("Nope")
    )
    # shp <- shp_ufs
    shp_df <- shp %>% 
      dplyr::filter(cd_uf%in%input$input_server_1)
  })
  
  
  # reac_query pra definir query
  reac_query <- eventReactive(input$goButton, {
    nm_db="db1"; nm_collec = "br_uf_raweci"
    mongo_set <- mongo(db = nm_db, collection = nm_collec, url = mongo_credentials$mongoURL, verbose = TRUE)
    df <- mongo_set$find()
  })
  
  react_df <- eventReactive(input$goButton, {
    df_shp <- dplyr::left_join(reac_query(), reac_shp()) %>% sf::st_sf()
  })
  
  output$plot1 <- shiny::renderPlot({
    # plot(react_df()["eci"])
    ggplot(react_df())+
      geom_sf(aes(fill=eci), color="black", size=.2)+
      scale_fill_gradient(low="white", high="blue")+
      annotate(x = -44.878609, y = -28.916854, geom = "text", label=paste0("M1=", "0.666 (dinamizar loc)"))+
      labs(title = "Gráfico teste", caption = "eci.app.br", y = "Latitude", x = "Longitude")
  })
  
  
}

shinyApp(ui = ui, server = server)


