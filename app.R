library(shiny)
library(tidyverse)
library(htmltools)
library(shinyjs)
library(rlang)
library(stringi)

inittime <- reactiveVal(10)
started <- reactiveVal(0)
rights <- reactiveVal(0)

respostas <- tibble(
  Bahia = c("Atlético-BA","Bahia","Bahia de Feira",
            "Barcelona-BA","Doce Mel","Jacuipense",
            "Juazeirense","UNIRB","Vitória",
            "Vitória da Conquista"),
  Pernambuco = c("Afogados","Caruaru City","Íbis",
                 "Náutico","Retrô","Salgueiro",
                 "Santa Cruz","Sete de Setembro","Sport",
                 "Vera Cruz")
  
)

ligas <- tibble(
  nomes = colnames(respostas)
)

alternativas_df <- tibble(
  Bahia = c("Atlético-BA","^bahia$","^bahia de feira$",
            "Barcelona-BA","Doce Mel","Jacuipense",
            "Juazeirense","UNIRB","Vitória",
            "Vitória da Conquista"),
  Pernambuco = c("Afogados","Caruaru City","Íbis",
                 "Náutico","Retrô","Salgueiro",
                 "Santa Cruz","Sete de Setembro","Sport",
                 "Vera Cruz")
  
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$style(HTML(
      "
      .lacuna {
      border: black 1px solid;
      width: 300px;
      height: 30px;
      }
      "
      
    ))
  ),
  
  fluidRow(
    column(
      width = 4,
      textInput(
        inputId = "chute",
        label = NULL,
        width = "80%",
        placeholder = "digite seu chute aqui"
      ),
    ),
    
    column(
      width = 3,
      offset = 1,
      uiOutput("acertos")
    ),
    
    column(
      width = 3,
      offset = 1,
      uiOutput("tempo")
    )
  ),
  
  fluidRow(
    ligas %>% 
      apply(1, list) %>% 
      map(function(liga) {
        column(
          width = 2,
          uiOutput(paste0("TABLE",liga))
        )
      })
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  alternativas <- reactiveVal({alternativas_df})
  shoot <- reactive({
    text <- tolower(input$chute)
    stringi::stri_trans_general(str = text, id = "Latin-ASCII")
    })
  
  ligas %>% 
    apply(1, list) %>% 
    purrr::map(function(liga) {
      
      output[[paste0("TABLE",liga)]] = renderUI({
        
        x = respostas %>%
          dplyr::pull(as.character(liga)) %>%
          as.matrix() %>%
          apply(1, list)
        
        map2(x, seq_along(x),
             ~tags$tr(
               tags$td(
                 class = "lacuna",
                 hidden(span(id = paste0(liga,.y), .x[[1]]))
               )
             )
        ) %>%
          tags$table() %>%
          tagInsertChildren(
            tags$tr(tags$th(as.character(liga))),
            after = 0
          )
        
      })
      
    })
  
  observe({
    # isolate({
      
      ligas %>% 
        apply(1, list) %>% 
        purrr::map(function(liga) {
          
          x = alternativas() %>%
            dplyr::pull(as.character(liga)) %>%
            as.matrix() %>%
            apply(1, list)  
          
          map2(x, seq_along(x),
               ~observeEvent(req(shoot()),{
                 shoot = shoot()
                 opcoes = ifelse(is.na(.x), "STRINGDENAOCLUBE", tolower(as.character(.x)))
                 id = paste0(liga,.y)
                 if (str_detect(shoot, opcoes)) {
                   print(.x)
                   show(id = id, anim = TRUE, time = 0.25)
                   var = rlang::sym(as.character(liga))
                   newvalue <- alternativas()
                   newvalue <- newvalue %>% 
                     dplyr::mutate(!!var := ifelse(rownames(newvalue) == .y, NA, !!var))
                   alternativas(newvalue)
                   
                   newvalue <- rights()
                   newvalue <- newvalue+1
                   rights(newvalue)
                 }
               })
          )
        })
    # })
  })
  
  output$tempo <- renderUI({
    span(inittime())
  })
  output$acertos <- renderUI({
    span(rights())
  })
  observeEvent(req(shoot()), {
    started(1)
  })
  
  observe({
    invalidateLater(1000, session)
    isolate(
      if (started() == 1) {
        newvalue <- inittime()-1
        inittime(newvalue)
      }
    )
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
