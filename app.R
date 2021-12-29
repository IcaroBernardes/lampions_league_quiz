# 0. Gestão dos pacotes
library(shiny)
library(htmltools)
library(shinyjs)
library(tidyverse)
library(rlang)
library(stringi)
library(lubridate)
library(glue)

# 1. Definição inicial dos dados
## Define os nomes dos estados com acentuação
estados <- c("Bahia","Pernambuco","Ceará",
             "Alagoas","Sergipe","Paraíba",
             "Rio Grande do Norte","Maranhão",
             "Piauí")

## Define as respostas a serem exibidas na tela
respostas <- tibble(
  BA = c("Atlético-BA","Bahia","Bahia de Feira",
         "Barcelona-BA","Doce Mel","Jacuipense",
         "Juazeirense","UNIRB","Vitória",
         "Vitória da Conquista"),
  Pernambuco = c("Afogados","Caruaru City","Íbis",
                 "Náutico","Retrô","Salgueiro",
                 "Santa Cruz","Sete de Setembro","Sport",
                 "Vera Cruz"),
  Ceara = c("Atlético-CE","Caucaia","Ceará",
            "Crato","Ferroviário","Fortaleza",
            "Icasa","Iguatu","Maracanã",
            "Pacajus"),
  Alagoas = c("ASA","CRB","Cruzeiro de Arapiraca",
              "CSA","CSE","Desportivo Aliança",
              "Jaciobá","Murici",NA,
              NA),
  Sergipe = c("América-SE","Atlético Gloriense","Boca Júnior",
              "Confiança","Falcon","Freipaulistano",
              "Itabaiana","Lagarto","Maruinense",
              "Sergipe"),
  Paraiba = c("Atlético-PB","Auto Esporte","Botafogo-PB",
              "Campinense","CSP","Nacional de Patos",
              "São Paulo Crystal","Sport-PB","Sousa",
              "Treze"),
  RN = c("ABC","América-RN","Assu",
         "Força e Luz","Globo FC","Potiguar de Mossoró",
         "Potyguar","Santa Cruz de Natal",NA,
         NA),
  Maranhao = c("Cordino","IAPE","Juventude",
               "Moto Club","Pinheiro","Sampaio Correia",
               "São José","Tuntum",NA,
               NA),
  Piaui = c("4 de Julho","Altos","Corisabbá",
            "Flamengo-PI","Fluminense-PI","Oeirense",
            "Parnahyba","River-PI",NA,
            NA)
)

## Obtém os nomes das ligas e lhes devolve o acento
colnames(respostas) <- estados
ligas <- tibble(
  nomes = colnames(respostas)
)

## Define as múltiplas formas pelas quais um clube pode ser identificado
alternativas_df <- tibble(
  BA = c("^atletico-ba$|^atletico ba$|^atletico de alagoinhas$|^alagoinhas atletico clube$|^alagoinhas ac$",
         "^bahia$|^esporte clube bahia$|^ec bahia$",
         "^bahia de feira$|^associacao desportiva bahia de feira$",
         "^barcelona-ba$|^barcelona ba$|^barcelona futebol clube$|^barcelona de ilheus$|^barcelona fc$",
         "^doce mel$|^doce mel esporte clube$|^doce mel ec$",
         "^jacuipense$|^esporte clube jacuipense$|^ec jacuipense$",
         "^juazeirense$|^sociedade desportiva juazeirense$",
         "^unirb$|^unirb futebol clube$|^unirb fc$",
         "^vitoria$|^esporte clube vitoria$|^ec vitoria$",
         "^vitoria da conquista$|^esporte clube primeiro passo vitoria da conquista$|^primeiro passo vitoria da conquista$|^ec vitoria da conquista$"),
  Pernambuco = c("^afogados$|^afogados da ingazeira futebol clube$|^afogados da ingazeira$|^afogados fc$",
                 "^caruaru city$|^caruaru city sport club$|^caruaru$|^caruaru city sc$",
                 "^ibis$|^ibis sport club$|^ibis sc$",
                 "^nautico$|^clube nautico capibaribe$|^nautico capibaribe$",
                 "^retro$|^retro futebol clube brasil$|^retro fc$",
                 "^salgueiro$|^salgueiro atletico clube$|^salgueiro ac$",
                 "^santa cruz$|^santa cruz futebol clube$|^santa cruz fc$",
                 "^sete de setembro$|^sete de setembro esporte clube$|^sete de setembro ec$|^7 de setembro$|^7 de setembro esporte clube$|^7 de setembro ec$",
                 "^sport$|^sport club do recife$|^sc recife$",
                 "^vera cruz$|^vera cruz futebol clube$|^vera cruz fc$"),
  Ceara = c("^atletico-ce$|^atletico ce$|^futebol clube atletico cearense$|^fc atletico cearence$|^uniclinic$",
            "^caucaia$|^caucaia esporte clube$|^caucaia ec$",
            "^ceara$|^ceara sporting club$|^ceara sc$",
            "^crato$|^crato esporte clube$|^crato ec$",
            "^ferroviario$|^ferroviario atletico clube$|^ferroviario ac$",
            "^fortaleza$|^fortaleza esporte clube$|^fortaleza ec$",
            "^icasa$|^associacao desportiva recreativa cultural icasa$",
            "^iguatu$|^associacao desportiva iguatu$",
            "^maracana$|^maracana esporte clube$|^maracana ec$",
            "^pacajus$|^pacajus esporte clube$|^pacajus ec$"),
  Alagoas = c("^asa$|^agremiacao sportiva arapiraquense$|^asa de arapiraca$",
              "^crb$|^clube de regatas brasil$",
              "^cruzeiro de arapiraca$|^esporte clube cruzeiro arapiraca$|^ec cruzeiro arapiraca$",
              "^csa$|^centro sportivo alagoano$",
              "^cse$|^clube cociedade esportiva$",
              "^desportivo alianca$|^desportivo alianca pilarense$|^desportiva alianca$|^desportiva alianca pilarense$",
              "^jacioba$|^jacyoba$|^jacyoba atletico clube$|^jacioba atletico clube$|^jacyoba ac$|^jacioba ac$",
              "^murici$|^murici futebol clube$|^murici fc$",
              NA,
              NA),
  Sergipe = c("^america-se$|^america se$|^america futebol clube$|^america fc$",
              "^atletico gloriense$|^associacao desportiva atletico gloriense$",
              "^boca junior$|^sociedade boca junior futebol clube$|^sociedade boca junior fc$",
              "^confianca$|^associacao desportiva confianca$",
              "^falcon$|^falcon futebol clube$|^falcon fc$",
              "^associacao desportiva freipaulistano$|^freipaulistano$",
              "^associacao olimpica de itabaiana$|^itabaiana$",
              "^lagarto$|^lagarto fc$|^lagarto futebol clube$",
              "^maruinense$|^centro sportivo maruinense$",
              "^sergipe$|^club sportivo sergipe$"),
  Paraiba = c("^atletico-pb$|^atletico pb$|^atletico cajazeirense de desportos$|^atletico de cajazeiras$",
              "^auto esporte$|^auto esporte clube$",
              "^botafogo-pb$|^botafogo pb$|^botafogo futebol clube$|^botafogo fc$|^botafogo da paraiba$",
              "^campinense$|^campinense clube$",
              "^csp$|^centro sportivo paraibano$",
              "^nacional de patos$|^nacional atletico clube$",
              "^sao paulo crystal$|^sao paulo crystal futebol clube$|^sao paulo crystal fc$",
              "^sport-pb$|^sport pb$|^sport club lagoa seca$",
              "^sousa$|^sousa esporte clube$|^sousa ec$",
              "^treze$|^treze futebol clube$|^treze fc$"),
  RN = c("^abc$|^abc futebol clube$|^abc fc$",
         "^america-rn$|^america rn$|^america de natal$|^america futebol clube$|^america fc$",
         "^assu$|^associacao sportiva sociedade unida$",
         "^forca e luz$|^centro esportivo forca e luz$",
         "^globo$|^globo futebol clube$|^globo fc$",
         "^potiguar de mossoro$|^potiguar$|^associacao cultural e desportiva potiguar$",
         "^potyguar$|^associacao cultural e desportiva potyguar seridoense$",
         "^santa cruz de natal$|^santa cruz futebol clube$|^santa cruz fc$",
         NA,
         NA),
  Maranhao = c("^cordino$|^cordino esporte clube$|^cordino ec$",
               "^iape$|^instituto de administracao de projetos educacionais futebol clube$|^instituto de administracao de projetos educacionais fc$",
               "^juventude$|^sociedade esportiva juventude$",
               "^moto club$|^moto club de sao luis$",
               "^pinheiro$|^pinheiro atletico clube$|^pinheiro ac$",
               "^sampaio correia$|^sampaio correa$|^sampaio correa futebol clube$|^sampaio correa fc$",
               "^sao jose$|^sao jose de ribamar esporte clube$|^sao jose de ribamar ec$",
               "^tuntum$|^tuntum esporte clube$|^tuntum ec$",
               NA,
               NA),
  Piaui = c("^4 de julho$|^quatro de julho$|^4 de julho esporte clube$|^quatro de julho esporte clube$|^4 de julho ec$|^quatro de julho ec$",
            "^altos$|^associacao atletica de altos$",
            "^corisabba$|^associacao atletica corisabba$",
            "^flamengo-pi$|^flamengo pi$|^esporte clube flamengo$|^ec flamengo$",
            "^fluminense-pi$|^fluminense pi$|^fluminense esporte clube$|^fluminense ec$",
            "^oeirense$|^associacao atletica oeirense$",
            "^parnahyba$|^parnahyba sport club$",
            "^river-pi$|^river pi$|^river atletico clube$|^river ac$",
            NA,
            NA)
)
colnames(alternativas_df) <- estados

# 2. UI
ui <- fixedPage(
  
  ## Ativa o uso de JS através do pacote shinyjs
  useShinyjs(),
  
  ## Cria link com arquivo css externo
  includeCSS("www/style.css"),
  
  ## Importa as fontes do Google usadas no app
  tags$head(
    tags$style(
      "@import url('https://fonts.googleapis.com/css2?family=Montserrat:wght@400;600&display=swap');",
      "@import url('https://fonts.googleapis.com/css2?family=Merriweather:wght@300;700&display=swap');"
    )
  ),
  
  ## Insere o título
  fixedRow(
    column(
      width = 12,
      id = "title",
      "Lampions League Quiz"
    )
  ),
  
  ## Insere o subtítulo
  fixedRow(
    column(
      width = 12,
      id = "subtitle",
      "Teste seus conhecimentos sobre as principais ligas do Nordeste!"
    )
  ),
  
  ## Insere as instruções do app
  fixedRow(
    column(
      width = 12,
      id = "instructions",
      span("Digite na caixa abaixo os seus chutes. Se o seu chute for correto, a caixa ficará limpa para receber novas respostas. Você tem 18 minutos para dar seus palpites!"),
      br(),
      span("Esse Quiz é inspirado em um similar "),
      a("Desafio do GE", target = "_blank", href = "https://interativos.globoesporte.globo.com/futebol/futebol-internacional/chuta-ai/chuta-ai-acerte-todos-os-98-participantes-das-top-5-ligas-da-europa"),
      span("e é feito em Shiny (R). "),
      a("Clique aqui", target = "_blank", href = "https://github.com/IcaroBernardes/lampions_league_quiz"),
      span("para saber mais sobre essa web application.")
    )
  ),
  
  ## Insere os botões de compartilhamento
  fixedRow(
    column(
      width = 12,
      id = "social-start",
      class = "social",
      span("Desafie outros: "),
      a(
        img(src = "twitter.png", height = "17.2px"),
        href = "https://twitter.com/intent/tweet?url=https%3A%2F%2Ficarob.shinyapps.io%2Flampions_quiz&text=Quantos+times+do+Nordeste+você+consegue+adivinhar%3F ",
        onclick = "javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
      ),
      a(
        img(src = "whatsapp.png", height = "17.2px"),
        href = "https://api.whatsapp.com/send/?phone&text=Quantos+times+do+Nordeste+você+consegue+adivinhar%3F+Entre+aqui+e+se+desafie%3A+https%3A%2F%2Ficarob.shinyapps.io%2Flampions_quiz&app_absent=0",
        onclick = "javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
      )
    )
  ),
  
  ## Insere a mensagem final de compartilhamento
  fixedRow(
    column(
      width = 12,
      shinyjs::hidden(uiOutput("share"))
    )
  ),
  
  ## Insere logo da competição e fonte da imagem
  fixedRow(
    column(
      offset = 4,
      width = 4,
      img(src = "logo.png", width = "40%")
    )
  ),
  fixedRow(
    column(
      width = 12,
      class = "fonte",
      "Imagem: Felipe Santos / Ceará SC / Lance!"
    )
  ),
  
  ## Insere a barra de chute/status
  fixedRow(
    class = "barra",
    column(
      width = 4,
      textInput(
        inputId = "chute",
        label = NULL,
        width = "100%",
        placeholder = "digite seu chute aqui"
      ),
    ),
    
    column(
      width = 4,
      uiOutput("acertos")
    ),
    
    column(
      width = 4,
      uiOutput("tempo")
    )
  ),
  
  ## Insere as tabelas
  fixedRow(
    ligas %>% 
      apply(1, list) %>% 
      map(function(liga) {
        column(
          width = 4,
          uiOutput(paste0("TABLE", stringi::stri_trans_general(str = liga, id = "Latin-ASCII")))
        )
      })
  )
  
)

# 3. Server
server <- function(input, output, session) {
  
  ## Define algumas variáveis
  inittime <- reactiveVal(18*60) ### Define o tempo total para responder o quiz (segundos)
  started <- reactiveVal(0) ### Indica se o usuario começou a responder o quiz
  ended <- reactiveVal(0) ### Indica se o usuário terminou de responder o quiz
  rights <- reactiveVal(0) ### Indica a qtd. de acertos
  
  ## Cria um objeto para armazenar as diferentes formas de  
  ## identificar um clube as quais ainda não foram adivinhadas
  alternativas <- reactiveVal({alternativas_df})
  
  ## Toma o chute do usuário e põe em minúsculas e retira acentos
  shoot <- reactive({
    text <- tolower(input$chute)
    stringi::stri_trans_general(str = text, id = "Latin-ASCII")
  })
  
  ## Cria a estrutura das tabelas onde são postas as respostas
  ### Converte a tibble que tem os nomes das ligas em lista e opera nela
  ligas %>% 
    apply(1, list) %>% 
    purrr::map(function(liga) {
      
      ### Cria um output (tabela) para cada liga
      output[[paste0("TABLE",stringi::stri_trans_general(str = liga, id = "Latin-ASCII"))]] = renderUI({
        
        ### Toma as respostas de uma dada liga e converte em lista
        x = respostas %>%
          dplyr::pull(as.character(liga)) %>%
          as.matrix() %>%
          apply(1, list)
        
        ### Cria linhas de uma dada tabela (tr). Essas linhas têm 
        ### apenas uma célula (td). Cada célula contém o nome de
        ### um clube em um span inicialmente invisível
        map2(x, seq_along(x),
             ~if (!is.na(.x)) {
               tags$tr(
                 tags$td(
                   class = "lacuna",
                   id = paste0("LAC",stringi::stri_trans_general(str = liga, id = "Latin-ASCII"),.y),
                   hidden(span(id = paste0(stringi::stri_trans_general(str = liga, id = "Latin-ASCII"),.y), .x[[1]]))
                 )
               )
             }
        ) %>%
          #### Toma o conjunto de linhas e põe dentro de uma tabela (table)
          tags$table() %>%
          #### Insere uma linha acima de todas as outras dentro da tabela.
          #### Essa linha contém o título da coluna (th)
          tagInsertChildren(
            tags$tr(tags$th(class = "header", as.character(liga))),
            after = 0
          )
        
      })
      
    })
  
  ## Cria uma estrutra que verifica se o usuario acertou algum clube e
  ## retira aqueles que foram identificados da lista de verificação
  observe({
    
    ligas %>% 
      apply(1, list) %>% 
      purrr::map(function(liga) {
        
        ### Toma as diferentes formas de resposta
        ### de uma dada liga e converte em lista
        x = alternativas() %>%
          dplyr::pull(as.character(liga)) %>%
          as.matrix() %>%
          apply(1, list)  
        
        map2(x, seq_along(x),
             function(x,y){
               ### Toma o chute do usuário sem acento e em minúsculas
               shoot = shoot()
               ### Define a string com as diferentes formas de identificar
               ### um dado clube e cria uma string falsa caso o clube 
               ### tenha sido adivinhado ou esteja como NA
               opcoes = ifelse(is.na(x), "STRINGdeAUSENCIA", tolower(as.character(x)))
               ### Obtém a id da linha de um dado clube na tabela de uma dada liga
               id = paste0(stringi::stri_trans_general(str = liga, id = "Latin-ASCII"),y)
               ### Verifica se o chute corresponde a alguma das opções de resposta
               if (str_detect(shoot, opcoes)) {
                 #### Exibe o nome do clube adivinhado
                 shinyjs::show(id = id, anim = TRUE, time = 0.25)
                 #### Muda a aparência da linha do clube adivinhado
                 shinyjs::addClass(id = paste0("LAC",id), class = "answer")
                 #### Toma o nome da liga e converte em symbol
                 var = rlang::sym(as.character(liga))
                 #### Retira as opções de resposta do clube adivinhado
                 newvalue <- alternativas()
                 newvalue <- newvalue %>% 
                   dplyr::mutate(!!var := ifelse(rownames(newvalue) == y, NA, !!var))
                 alternativas(newvalue)
                 #### Aumenta a contagem de clubes adivinhados
                 newvalue <- rights()
                 newvalue <- newvalue+1
                 rights(newvalue)
                 #### Reinicia o campo de inserção dos chutes
                 updateTextInput(
                   session = session,
                   inputId = "chute",
                   value = ""
                 )
               }
             }
        )
      })
    
  }) %>% 
    ### Condiciona a verificação do chute a alterações
    ### no texto digitado pelo usuário como chute
    bindEvent(shoot(), ignoreInit = TRUE)
  
  ## Pega o tempo e converte em período legível para o usuário
  output$tempo <- renderUI({
    div(
      span(class = "cima", "TEMPO", br()), 
      span(class = "baixo", lubridate::seconds_to_period(inittime()))
    )
  })
  
  ## Pega a qtd. de acertos e exibe para o usuário
  output$acertos <- renderUI({
    div(
      span(class = "cima", "ACERTOS", br()), 
      span(class = "baixo", paste0(rights(),"/82"))
    )
  })
  
  ## Ativa o cronômetro assim que o usuário digita
  ## o primeiro caractere (ativa a "flag" started) e
  ## confirma que o quiz
  observeEvent(req(shoot()), {
    if (ended() == 0){
      started(1)
    }
  })
  
  ## Diminui um segundo do tempo a partir do
  ## momento que a "flag" started é ativada
  observe({
    invalidateLater(1000, session)
    isolate(
      if (started() == 1) {
        newvalue <- inittime()-1
        inittime(newvalue)
      }
    )
  })
  
  ## Verifica se o tempo acabou,
  ## para o cronômetro e ativa a "flag" ended
  observe({
    if (inittime() == 0) {
      started(0)
      ended(1)
    } 
  })
  
  ## Verifica se o usuário acertou todos os clubes,
  ## para o cronômetro e ativa a "flag" ended
  observe({
    if (rights() == 82) {
      started(0)
      ended(1)
    } 
  })
  
  ## Verifica se o quiz acabou ("flag" ended ativa), esconde o
  ## campo de inserção dos chutes, elementos do topo da página e
  ## exibe todos os nomes dos clubes e uma mensagem final
  observe({
    if (ended() == 1) {
      
      shinyjs::hide(id = "chute")
      shinyjs::hide(id = "subtitle")
      shinyjs::hide(id = "instructions")
      shinyjs::hide(id = "social-start")
      shinyjs::show(id = "share")
      
      ligas %>% 
        apply(1, list) %>% 
        purrr::map(function(liga) {
          
          ### Toma as diferentes formas de resposta
          ### de uma dada liga e converte em lista
          x = alternativas() %>%
            dplyr::pull(as.character(liga)) %>%
            as.matrix() %>%
            apply(1, list)  
          
          map2(x, seq_along(x),
               function(x,y){
                 ### Define a string com as diferentes formas de identificar um dado clube 
                 opcoes = as.character(x)
                 ### Obtém a id da linha de um dado clube na tabela de uma dada liga
                 id = paste0(stringi::stri_trans_general(str = liga, id = "Latin-ASCII"),y)
                 ### Verifica se um dado clube ainda não foi adivinhado
                 if (!is.na(opcoes)) {
                   #### Exibe o nome do clube não-adivinhado
                   shinyjs::show(id = id, anim = TRUE, time = 0.25)
                   #### Muda a aparência da linha do clube não-adivinhado
                   shinyjs::addClass(id = paste0("LAC",id), class = "wrong")
                 }
               }
          )
        })
      
    }
  })
  
  ## Cria a mensagem final de compartilhamento
  output$share <- renderUI({
    
    
    corretos <- rights()
    tempo <- 18*60 - inittime()
    text_show <- glue::glue("Você acertou {corretos} clubes em {tempo} segundos!")
    text_twitter <- glue::glue("https://twitter.com/intent/tweet?url=https%3A%2F%2Ficarob.shinyapps.io%2Flampions_quiz&text=Eu+joguei+esse+Quiz+e+acertei+{corretos}+times+em+{tempo}+segundos. E+você%3F+Quantos+times+do+Nordeste+você+consegue+adivinhar%3F ")
    text_whatsapp <- glue::glue("https://api.whatsapp.com/send/?phone&text=Eu+joguei+esse+Quiz+e+acertei+{corretos}+times+em+{tempo}+segundos. E+você%3F+Quantos+times+do+Nordeste+você+consegue+adivinhar%3F+Entre+aqui+e+se+desafie%3A+https%3A%2F%2Ficarob.shinyapps.io%2Flampions_quiz&app_absent=0")
    
    div(
      class = "social",
      span(text_show, id = "message"), br(),
      span("Mostre seu desempenho a outros: "),
      a(
        img(src = "twitter.png", height = "17.2px"),
        href = text_twitter,
        onclick = "javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
      ),
      a(
        img(src = "whatsapp.png", height = "17.2px"),
        href = text_whatsapp,
        onclick = "javascript:window.open(this.href, '', 'menubar=no,toolbar=no,resizable=yes,scrollbars=yes,height=300,width=600');return false;"
      )
    )
    
  })
  
}

# 4. Roda o app
shinyApp(ui, server)