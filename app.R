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
  
  output$input_ui_1 <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_server_1",
      label = "Selecione a região do Brasil",
      choices = c("Brasil", "Centro-oeste", "Nordeste", "Norte", "Sudeste", "Sul"),
      # choices = list(
        # "Centro-oeste", "Nordeste", "Norte", "Sudeste", "Sul"
      #   "Brasil" = "Brasil", 
      #   "Centro-Oeste" = "Centro-oeste", 
      #   "Nordeste" = "Nordeste", 
      #   "Norte" = "Norte", 
      #   "Sudeste" = "Sudeste", 
      #   "Sul" = "Sul"
      # ),
      multiple = FALSE,
      selected = "Brasil"
    )
  })
  
  output$input_ui_2 <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_server_2",
      label = "Selecione a divisão territorial",
      choices = list(
        "Estado" = "uf"
        "Macrorregião" = "macro", 
        "Intermediária" = "int", 
        "Microrregião" = "micro", 
        "Imediata" = "ime"
      ),
      multiple = FALSE,
      selected = "micro"
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
  shp_ufs <- sf::st_read("data/BR_UF_2020/") %>%
    janitor::clean_names() %>%
    sf::st_set_crs(4326) %>%
    dplyr::mutate(cd_uf = as.character(cd_uf))
  
  # shp_meso
  # shp_int
  # shp_micro
  # shp_ime
  
  # reac_query pra definir query
  reac_query <- eventReactive(input$goButton, {
    nm_db="db1"; nm_collec = "br_uf_raweci"
    mongo_set <- mongo(db = nm_db, collection = nm_collec, url = mongo_credentials$mongoURL, verbose = TRUE)
    df <- mongo_set$find()
  })
  
  reac_shp <- eventReactive(input$goButton, {
    # switch(input$input_server_2,
    #   "uf" = {shp <- shp_ufs},
    #   "macro" = {shp <- shp_ufs}
    # )
    shp <- shp_ufs
    shp_df <- shp %>% 
      dplyr::filter(if(input$input_server_1 != "Brasil") (nm_regiao%in%input$input_server_1) else T)
  })
  
  react_df <- eventReactive(input$goButton, {
    df_shp <- dplyr::left_join(reac_query(), reac_shp()) %>% sf::st_sf()
  })
  
  output$plot1 <- shiny::renderPlot({
    # plot(react_df()["eci"])
    ggplot(react_df())+
      geom_sf(aes(fill=eci), color="black", size=.2)+
      scale_fill_gradient(low="white", high="blue")+
      annotate(x = -44.878609, y = -28.916854, geom = "text", label=paste0("M1=", "0.666"))+
      labs(title = "Gráfico teste", caption = "eci.app.br", y = "Latitude", x = "Longitude")
  })
  
  
}

shinyApp(ui = ui, server = server)


