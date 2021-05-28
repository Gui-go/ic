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
  
  output$input_ui_1 <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_server_1",
      label = "Selecione a região do Brasil",
      choices = sort(c("Brasil", "Norte", "Nordeste", "Centro-Oeste", "Sul", "Sudeste")),
      multiple = FALSE,
      selected = "Sul"
    )
  })
  
  output$input_ui_2 <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId = "input_server_2",
      label = "Selecione a divisão territorial",
      choices = sort(c("Macrorregião", "Intermediária", "Microrregião", "Imediata")),
      multiple = FALSE,
      selected = "Microrregião"
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
  
  # reac_shp
  reac_shp <- eventReactive(input$goButton, { # event reative mesmo:::::::::::::::::::;;
    shp_ufs <- sf::st_read("data/BR_UF_2020/") %>%
      janitor::clean_names() %>% 
      sf::st_set_crs(4326) %>% 
      dplyr::mutate(cd_uf = as.character(cd_uf))
    return(shp_ufs)
  })
  
  # reac_query pra definir query
  reac_query <- eventReactive(input$goButton, {
    # source(file = "code/functions/fct_findmongodb.R")
    nm_db="db1"; nm_collec = "br_uf_raweci"
    mongo_credentials <- config::get(file = "conf/globalresources.yml")
    mongo_set <- mongo(db = nm_db, collection = nm_collec, url = mongo_credentials$mongoURL, verbose = TRUE)
    df <- dfmongo_set$find()
  })
  
  
  
  react_df <- eventReactive(input$goButton, {
    dff_shp <- dplyr::left_join(reac_query(), reac_shp()) %>% sf::st_sf()
    return(dff_shp)
  })
  
  output$plot1 <- shiny::renderPlot({
    print(str(reac_query()))
    # plot(react_df()["eci"])
  })
  

}

shinyApp(ui = ui, server = server)


