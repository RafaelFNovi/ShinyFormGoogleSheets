library(shinycssloaders)
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(ggthemes)
library(hrbrthemes)
library(shinyWidgets)
library(shinyfullscreen)
library(readxl)
library(googlesheets4)
library(googledrive)

fields <- c("Data", "Lote","Galpao" ,"Linhagem","QTD","Peso")

options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "dashboard.n****@gmail.com")
gs4_auth(token = drive_token())
#C*****b********01

ui<-shinyUI(
  navbarPage(title = "Uniformidade",id = "inTabset",
             theme = "style/style.css",
             fluid = TRUE, 
             collapsible = TRUE,
             
             tabPanel("Ovo",
                      
                      fluidRow(
                        column(2,
                               dateInput('Data',
                                         label = 'Data',
                                         value = Sys.Date(),
                                         width = "220px"
                               )
                               
                        ),
                        
                        column(2,#offset = 1,
                               selectInput(inputId = "Lote",
                                           label = "Lote",
                                           choices = c("1", "2","3","4","5","6","7","8","9","10","11","12"),
                                           selected = "50 Free",
                                           width = "220px"
                               )
                               
                        ),
                        column(2,#offset = 1,
                               selectInput(inputId = "Galpao",
                                           label = "Galpao",
                                           choices = c("1A","1B","2A","2B","3A","3A","4A","4B"),
                                           selected = "50 Free",
                                           width = "220px"
                               )
                               
                        ),
                        column(2,#offset = 1,
                               selectInput(inputId = "Linhagem",
                                           label = "Linhagem",
                                           choices = c("COBB", "ROSS"),
                                           selected = "50 Free",
                                           width = "220px")
                        ),
                        column(1,#offset = 1,
                               selectInput(inputId = "QTD",
                                           label = "Quantidade",
                                           choices = c("1","2","3","4","5","6","7","8","9","10","11","12",
                                                       "13","14","15","16","17","18","19","20"),
                                           selected = "50 Free",
                                           width = "220px")
                        ),
                        column(2,#offset = 1,
                               sliderInput("Peso",
                                           "Peso",
                                           min = 35,
                                           max = 100,
                                           value = 70)
                        ),
                        
                        
                        br(),
                        br(),
                        column(1,#offset = 1,
                               actionButton("submit", "Submit")
                        )
                      ),
                      hr(),
                      mainPanel(
                        
                        
                        fluidRow(
                          column(6,
                                 
                                 shinycssloaders::withSpinner(
                                   DT::dataTableOutput("responses", width = 300))
                          ),
                          column(2,
                                 dateInput('Data1',
                                           label = 'Data',
                                           value = Sys.Date(),
                                           width = "220px"
                                 )),
                          column(2,
                                 selectInput("inSelect", "Select input",
                                             c("Todos"))
                                 
                          ),
                          
                          column(6,#offset = 0,
                                 plotlyOutput(outputId = "plot1", width = 1100,height=600)
                          )
                          
                        )    
                        
                      )),
             
             # tab panel 3 -
             tabPanel("Base de Dados",
                      
                      fluidPage(
                        #titlePanel(""),
                        htmlOutput("googleSheet"))
                      
             )
  ))

server = function(input, output, session) {
  
  ####
  # link your Sheet's URL string here
  googleSheet_embed_link <- "https://docs.google.com/spreadsheets/d/1RwcmkFx2gGMvXDLazlWp7hXHBl1cC4czCjx4SMjgdnI/edit#gid=0"

    output$googleSheet <- renderUI({
      tags$iframe(id = "googleSheet",
                  src = googleSheet_embed_link,
                  width = 1024,
                  height = 768,
                  frameborder = 0,
                  marginheight = 0)
    })

  ###### captura de dados
  
  outputDir <- "responses"
  
  
  saveData <- function(data) {
    # The data must be a dataframe rather than a named vector
    data <- data %>% as.list() %>% data.frame() 
    
    data$DataInput<-Sys.Date()
    
    # Add the data as a new row
    data$Data<- as.numeric(data$Data)
    data$Data<- as.Date(data$Data, origin = "1970-01-01")
    
    sheet_append("1RwcmkFx2gGMvXDLazlWp7hXHBl1cC4czCjx4SMjgdnI", data)
    
  }
  
  loadData <- function() {
    # Read the data
    
    read_sheet("1RwcmkFx2gGMvXDLazlWp7hXHBl1cC4czCjx4SMjgdnI")
    
    
  }
  
  
  ##########################################################################################
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })
  
  Base<- reactive({
    input$submit
    Dados <- read_sheet("1RwcmkFx2gGMvXDLazlWp7hXHBl1cC4czCjx4SMjgdnI")
    Base <- as.data.frame(Dados)
    
    Base$Data<- as.Date(Base$Data)
    Base$DataInput<-as.Date(Base$DataInput)
    Base
    
  }
  )
  # Historico<- reactive({
  #   Historico <- read_excel("Historico.xlsx")
  #   Historico$Data<-as.Date(Historico$Data)
  #   Historico$Peso<-round(Historico$Peso,0)
  #   Historico
  # })
  #####################################
  dataset<- reactive({
    
    dataset <- Base() #%>%
    #bind_rows(Historico())
    dataset<-arrange(dataset, Peso, Lote,Galpao) #ordena
    dataset$Lote<-as.character(dataset$Lote)
    datafiltro<-input$Data1
    dataset<-filter(dataset, Data == datafiltro)
    dataset$Lote<-as.factor(dataset$Lote)
    dataset$Peso<-as.numeric(dataset$Peso)
    dataset$Galpao<-as.character(dataset$Galpao)
    dataset$QTD<-as.character(dataset$QTD)
    dataset$QTD<-as.numeric(dataset$QTD)
    dataset <- as.data.frame(lapply(dataset,rep, dataset$QTD))
    dataset
  })
  
  observe({
    input$Data1
    y <- input$inSelect
    
    # Can use character(0) to remove all choices
    if (is.null(y))
      y <- "Todos"
    if(y %in% dataset()$Lote==FALSE){
      y<-"Todos"
    }
    updateSelectInput(session, "inSelect",
                      label = paste("Selececione o Lote"),
                      choices =  list(`All Lotes` = list("Todos"),
                                      `Lotes` = levels(dataset()$Lote)),
                      selected =y #tail(dataset(),1)
    )
  })
  dataset1<-reactive({
    lotefiltro<-input$inSelect
    dataset1<-dataset()
    
    if (is.null(lotefiltro))
      lotefiltro <- "Todos"
    if(lotefiltro %in% dataset()$Lote==FALSE){
      lotefiltro<-"Todos"
    }
    
    dataset1$Lote<-as.character(dataset1$Lote)
    
    if(lotefiltro!="Todos"){
      dataset1<-filter(dataset1, Lote == lotefiltro)
    }
    if(length(dataset()$Peso)==0){
      Peso<-0
      dataset1<-as.data.frame(Peso)
    }
    
    n<-length(dataset1$Peso)
    index<-as.vector(NULL)
    
    
    for (i in 1:n) {
      if(i==1){
        index[i]<-1
      }
      else if(dataset1$Peso[i]==dataset1$Peso[i-1]){
        index[i]<-index[i-1]+1
      }
      else{
        index[i]<-1
      }
    }
    dataset1$index<-index 
    
    # <- as.numeric(input$Data)
    # <- as.Date(input$Data, origin = "1970-01-01")
    
    dataset1
  })
  
  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    Base()
  })
  output$plot1 <- renderPlotly({
    input$submit
    if(length(dataset1())>2){
      p1<-ggplot(dataset1(),aes(x=index, y=Peso,colour=Galpao))+
        labs(x="", y="Pesado                            Medio                            Leve", title = "")+
        geom_point(size=6,shape=15)+
        geom_text(aes(label=Peso),color="black",size=3)+
        scale_y_continuous(trans = "reverse")
      p1
      ggplotly(p1)
    }else{
      p1<-ggplot(NULL)
      ggplotly(p1)
    }
    
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
######



