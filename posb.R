##### Banco Capes - Pos Banca ###########

library(shiny)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(fresh)
library(shinyjs)
library(readxl)
library(DT)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(stringr)
library(SnowballC)
library(plotly)
library(openxlsx)


mytheme <- create_theme(
  
  adminlte_color(
    light_blue = "#3F5B72",
    
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#3F5B72", #lateral
    dark_hover_bg = "#CE0707", # detaque da lateral
    dark_color = "#FFFFFF" # fontes da lateral
  ),
  
  adminlte_global(
    content_bg = "#FFFFFF", #fundo do MainPainel
    box_bg = "#FCFCFC", # Detalhes da box
    info_box_bg = "#FFFFFF" # Detalhes das box
    
  )
)

area <- read_excel("area.xlsx")

area <- area$area



# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(disable = TRUE,
                  title = " ",
                  titleWidth = 500,
                  tags$li(
                    class = "dropdown",
                    tags$a(
                      href = "https://www.emorio.org/",
                      style = "color: white;",  
                      "Nosso Site"
                    )
                  )
  ),
  
  dashboardSidebar(
    tags$style(".sidebar-toggle { display: none; }"),
    collapsed = TRUE
  ),
  dashboardBody(
    use_theme(mytheme),
    useShinyjs(),
    h2(HTML("<strong> Pós-Banca</strong> | Teses e Dissertações")),
    br(),
    fluidPage(
      fluidRow(
        column(
          width = 4,
          height = 180,
          HTML(paste("Iniciativa voltada para facilitar a busca de trabalhos produzidos",
                     "nos programas de pós-graduação do Brasil.",
                     "<br>",
                     "Aqui você vai descobrir quem defendeu, quem orientou e em que áreas e programas foram produzidos",
                     "trabalhos acadêmicos sobre temas do seu interesse.",
                     "<br>",
                     "<br>",
                     "Clique em <strong>Baixar</strong> para guardar os resultados da sua consulta."
                     
                     
          ))
          
          
        ),
        column(
          width = 4,
          height = 180,
          br(),
          textInput("Tema", "Tema", "Digite"),
          verbatimTextOutput("Tema"),
          selectInput("area", "Grande Área", choices = area, selected = "CIÊNCIAS HUMANAS" ),
          
          textOutput("resultado")),
        column(
          width = 4,
          height = 180,
          br(),
          radioButtons('fonte', 'Fonte', list('Palavras-chave' = 'palavras', 'Resumos' = 'resumos' ), 
                       selected = 'palavras', inline = TRUE),
          br(),
          br(),
          actionButton('pesq', 'Pesquisar',
                       style = "color: #fff; background-color: #3F5B72; border-color: 
                         #fff;padding: 3px 10px 4px 14px;margin: 10px 10px 10px 10px; "),
          downloadButton('baixar',
                         'Baixar',
                         style = "color: #fff; background-color:#3F5B72; border-color: 
                         #fff;padding: 3px 10px 4px 14px;margin: 10px 10px 10px 10px; "),
          HTML(paste())
        )
        ),
      fluidRow(
        HTML(paste( '<hr style="border-color: #DCDCDC;">')),
      ),
      conditionalPanel(
        condition = "input.pesq > 0",
      fluidRow(
        column(
          width = 4,
          height = 180,
          br(),
          valueBoxOutput("Box",  width = 12)),
        column(
          width = 4,
          height = 180,
          br(),
          valueBoxOutput("BoxD",  width = 12),
        
      ),
      column(
        width = 4,
        height = 180,
        br(),
        valueBoxOutput("BoxE",  width = 12),
        
      )),
      
      fluidRow(
        
        column(
          
          width = 6.0,
          height = 90,
          box(
            width = 12,
            height = 600,
            title = "Trabalhos",
            status = "primary",
            checkboxGroupInput("Trabalhos", "", choices = c("Tese", "Dissertação", "Outros"),
                               selected = c("Tese", "Dissertação", "Outros"), inline = TRUE),
            DTOutput("tabela_output")
            
          )),
        column(
          width = 6.0,  
          height = 600, 
          solidHeader = FALSE,
          box(
            collapsible = TRUE,
            title = "Programas",
            width = 12,
            height = 600,
            
            status = "primary",
            
            plotOutput("plot", height = "500px"))
        )
      ),
      fluidRow(
        column(
          
          width = 6.0,
          height = 90,
          box(
            width = 12,
            height = 600,
            title = "Autores",
            status = "primary",
            checkboxGroupInput("Trabalhosb", "", choices = c("Tese", "Dissertação", "Outros"),
                               selected = c("Tese", "Dissertação", "Outros"), inline = TRUE),
            DTOutput("tabela_outputb")
            
          )),
        column(
          width = 6.0,  
          height = 600, 
          solidHeader = FALSE,
          box(
            collapsible = TRUE,
            title = "Área de Conhecimento",
            width = 12,
            height = 600,
            
            status = "primary",
            
            plotOutput("plot1", height = "500px"))
          
        )
        
        
      ),
      
      fluidRow(
        column(
          
          width = 6.0,
          height = 90,
          box(
            width = 12,
            height = 600,
            title = "Orientadores",
            status = "primary",
            checkboxGroupInput("Trabalhosc", "", choices = c("Tese", "Dissertação", "Outros"),
                               selected = c("Tese", "Dissertação", "Outros"), inline = TRUE),
            DTOutput("tabela_outputc")
            
          )),
        
        column(
          width = 6.0,  
          height = 600, 
          solidHeader = FALSE,
          box(
            collapsible = TRUE,
            title = "Instituições",
            width = 12,
            height = 600,
            
            status = "primary",
            
            plotOutput("plot2", height = "500px"))
        )
      ),
      fluidRow( # retirar caso o funcionamento esteja lento
        column(
          width = 6.0,  
          height = 400, 
          solidHeader = FALSE,
          box(
            collapsible = TRUE,
            title = "Resumo",
            width = 12,
            height = 500,
            
            plotOutput("nuvem")
          )
        ),
        column(
          width = 6.0,  
          height = 400, 
          solidHeader = FALSE,
          box(
            collapsible = TRUE,
            title = "Palavras-Chave",
            width = 12,
            height = 500,
            
            plotOutput("nuvemB")
          )
        )
      )
      ),
      fluidRow(
        h6(HTML("*** <strong>Fase de teste</strong>: disponível apenas para trabalhos defendidos","<br>",
                           "em 2021. ",
                   "<br>",
                   "<br>",
                   "Acesse base completa em:",
                   "<br>",
                   "<a href='", "https://osf.io/qva6u/", "' target='_blank'>", "Banco de Teses e Dissertações da Capes", "</a>"))
      )
      
    )
  
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  dfatual <- read_excel("btest.xlsx")
  dfatualx <- read_excel("brest.xlsx")
  
 
  observeEvent(input$pesq ,{
    if(input$Tema == "CIÊNCIAS AGRÁRIAS"||input$Tema == "CIÊNCIAS EXATAS E DA TERRA"||
       input$Tema == "ENGENHARIAS"){
      dfatual <- dfatualx
    }
    if(input$fonte=="palavras"){
      base <<-  subset(dfatual,NM_GRANDE_AREA_CONHECIMENTO==input$area)
      base <<- base[grep(input$Tema,base$DS_PALAVRA_CHAVE, ignore.case = TRUE), ]
      
    }else{
      base <<-  subset(dfatual,NM_GRANDE_AREA_CONHECIMENTO==input$area)
      base <<- base[grep(input$Tema,base$DS_RESUMO, ignore.case = TRUE), ]
      
    }
    
    tabela <- reactive({
      
      btrab <- base %>% 
        dplyr::select(PRODUCAO, NM_PRODUCAO, NM_AREA_CONHECIMENTO, AN_BASE)
      subset(btrab, PRODUCAO %in% input$Trabalhos)
      
      
    })
    
    output$tabela_output <- renderDT({
      datatable(tabela(), options = list(
        scrollY = "400px",  
        scrollX = "100px",
        paging = FALSE, searching = FALSE))
    })
    
    tabelab <- reactive({
    
      btrab <- base %>% 
        dplyr::select(PRODUCAO, NM_DISCENTE, NM_PRODUCAO)
      subset(btrab, PRODUCAO %in% input$Trabalhosb)
      
      
    })
    
    output$tabela_outputb <- renderDT({
      datatable(tabelab(), options = list(
        scrollY = "400px",  
        scrollX = "100px",
        paging = FALSE, searching = FALSE))
    })
    
    tabelac <- reactive({
      
      btrab <- base %>% select(NM_ORIENTADOR, NM_DISCENTE, PRODUCAO)
      subset(btrab, PRODUCAO %in% input$Trabalhosc)
      
      
    })
    
    output$tabela_outputc <- renderDT({
      datatable(tabelac(), options = list(
        scrollY = "400px",  
        scrollX = "100px",
        paging = FALSE, searching = FALSE))
    })
    
    btotal <- summarise(base, valor=sum(n()))
    
    output$Box <- renderValueBox({
      valueBox(
        btotal$valor, "Trabalho", 
        color = "light-blue", 
      )
    })
    
    bprog <- base %>% 
      group_by(NM_PROGRAMA) %>% 
      summarise(valor=n()) %>% 
      mutate(total=sum(n_distinct(NM_PROGRAMA)))
    
    output$BoxD <- renderValueBox({
      valueBox(
        ifelse(is.na(bprog[1, 3]), 0, bprog[1, 3]), "Programas", 
        color = "teal"
      )
    })
    
    barea <- base %>% 
      group_by(NM_AREA_CONHECIMENTO) %>% 
      summarise(valor=n()) %>% 
      mutate(total=sum(n_distinct(NM_AREA_CONHECIMENTO)))
    
    output$BoxE <- renderValueBox({
      valueBox(
        ifelse(is.na(barea[1, 3]), 0, barea[1, 3]), "Área de Conhecimento", 
        color = "teal"
      )
    })
    
    output$plot1 <- renderPlot({ 
      
      
      barea <- base %>% 
        group_by(NM_AREA_CONHECIMENTO) %>% 
        summarise(valor=n()) 
      
      
      ggplot(barea %>% top_n(10, valor), aes(x = reorder(NM_AREA_CONHECIMENTO, valor), y = valor)) +
        geom_bar(stat = "identity", width = 0.8, color="#39CCCC", fill= "#39CCCC") +xlab(" ")+
        ylab(" ")+geom_text(aes(label = valor), hjust = -0.3, size = 4, color = "#3F5B72") +
        theme_minimal()+coord_flip()+theme(axis.text.y = element_text(size = 9))
      
      
      
    })
    
    output$plot <- renderPlot({ 
     
      bporg <- base %>% 
        group_by(NM_PROGRAMA) %>% 
        summarise(valor=n()) 
      
      
      
      ggplot(bprog %>% top_n(10, valor), aes(x = reorder(NM_PROGRAMA, valor), y = valor)) +
        geom_bar(stat = "identity", width = 0.8, color="#39CCCC", fill= "#39CCCC") +xlab(" ")+
        ylab(" ")+geom_text(aes(label = valor), hjust = -0.3, size = 4, color = "#3F5B72") +
        theme_minimal()+coord_flip()+theme(axis.text.y = element_text(size = 9))
      
      
      
    })
    
    
    output$plot2 <- renderPlot({ 
    
      
      bent <<- base %>% 
        group_by(SG_ENTIDADE_ENSINO) %>% 
        summarise(valor=n()) 
      
      
      ggplot(bent%>% top_n(10, valor), aes(x = reorder(SG_ENTIDADE_ENSINO, valor), y = valor)) +
        geom_bar(stat = "identity", width = 0.8, color="#39CCCC", fill= "#39CCCC") +xlab(" ")+
        ylab(" ")+geom_text(aes(label = valor), hjust = -0.3, size = 4, color = "#3F5B72") +
        theme_minimal()+coord_flip()+theme(axis.text.y = element_text(size = 9))
      
      
      
    })
    # retirar caso o funcionamento esteja lento
    output$nuvem <- renderPlot({
      
     
      aux <- base %>% 
        select(DS_RESUMO)
      
      
      aux <- str_split(aux, fixed(';'))
      
      stop_words_custom <- c("COM", "QUE","SER" ,"SUA","SUAS","SEUS", "SEU","SOBRE", "PARA", "POR", 
                             "DAS", "DOS", "DE", "PELO", "PELA", "PELAS", "ENTRE", "FOI",
                             "TAMBÈM", "AINDA", "NOS", "NAS", "NÃO", "COMO","MAIS","ALÈM",
                             "UMA", "UMAS", "AOS", "FORAM", "SÃO", "ASSIM", "TEM", "FORMA",
                             "BEM", "MAIOR", toupper(input$Tema))
      auxCorpus <- Corpus(VectorSource(aux))
      auxCorpus <- tm_map(auxCorpus, PlainTextDocument)
      auxCorpus <- tm_map(auxCorpus, removePunctuation) 
      auxCorpus <- tm_map(auxCorpus, removeWords, c(stopwords('pt'), stop_words_custom)) 
      auxCorpus <- tm_map(auxCorpus, stemDocument)
      wordcloud(auxCorpus,max.words=80,colors=c("#3F5B72","#39CCCC"), scale = c(3, 1))
      
    })
    
    output$nuvemB <- renderPlot({
      
     
      aux <- base %>% 
        select(DS_PALAVRA_CHAVE)
      
      
      aux <- str_split(aux, fixed(';'))
      
      stop_words_custom <- c("COM", "QUE","SER" ,"SUA","SUAS","SEUS", "SEU","SOBRE", "PARA", "POR", 
                             "DAS", "DOS", "DE", "PELO", "PELA", "PELAS", "ENTRE", "FOI",
                             "TAMBÈM", "AINDA", "NOS", "NAS", "NÃO", "COMO","MAIS","ALÈM",
                             "UMA", "UMAS", "AOS", "FORAM", "SÃO", "ASSIM", "TEM", "FORMA",
                             "BEM", "MAIOR", toupper(input$Tema))
      
      auxCorpus <- Corpus(VectorSource(aux))
      auxCorpus <- tm_map(auxCorpus, PlainTextDocument)
      auxCorpus <- tm_map(auxCorpus, removePunctuation) 
      auxCorpus <- tm_map(auxCorpus, removeWords, c(stopwords('pt'), stop_words_custom)) 
      auxCorpus <- tm_map(auxCorpus, stemDocument)
      wordcloud(auxCorpus,max.words=80,colors=c("#3F5B72","#39CCCC"), scale = c(3, 1))
      
    })
    
    
    output$baixar <- downloadHandler(
      
      filename = function() {
        "base.xlsx"
      },
      content = function(file) {
        write.xlsx(base, file)
      }
    )
    
  })
  
  

    
}

# Run the application 
shinyApp(ui = ui, server = server)
