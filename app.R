library(shiny)
library(tidyverse)
library(readxl)
library(shinythemes)
library(gghighlight)
library(plotly)
library(beeswarm)
library(ggrepel)

ui <- fluidPage(titlePanel("EDA with Shiny", title=div(img(height = 100, width = 120, src="logo.png"))),
                theme = shinytheme("cosmo"),
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                # title
                headerPanel(h1("Exploratory data analysis with Shiny")),
                sidebarPanel
                (
                    helpText(""),  
                    fileInput('files', "Upload Wyscout-file (.xslx format)", accept = c('.xlsx')),
                    tags$hr(),
                    helpText("Compare a player"),
                    uiOutput("spiller_sammenlign"),
                    helpText(""),
                    uiOutput("sammenlign"),
                    tags$hr(),
                    helpText("Steady & interactive graphs"),
                    uiOutput("xplotly"),
                    uiOutput("yplotly"),
                    uiOutput("sizely"),
                    uiOutput("title"),
                    tags$hr(),
                    helpText("Table"),
                    #helpText("Vælg variabler til tabel"),
                    uiOutput("nytabel"),
                    width = 3
                ),
                mainPanel(
                    tabsetPanel(id = "dataset",
                                tabPanel("Compare player", plotOutput("compplot")),
                                tabPanel("Steady graph", plotOutput("plot")),
                                tabPanel("Interactive graph", plotlyOutput("plotly")),
                                tabPanel("Table", DT::dataTableOutput("rendered_file"))
                    )),
)

server <- function(input,output,session) {
    
    df <- reactive({
        req(input$files)
        
        inFile <- input$files
        if (is.null(inFile)) return(NULL)
        
        file.rename(inFile$datapath,
                    paste(inFile$datapath, "", sep=""))
        read_excel(paste(inFile$datapath,'', sep =""))
   
            })
    
    output$nyfil <- renderUI({
        selectizeInput(inputId = "select_var", 
                       label = "", 
                       choices = names(df()),
                       selected = "Player", TRUE,
                       multiple = TRUE)
        
    })
    
    output$spiller_sammenlign <- renderUI({
        selectizeInput(inputId = "select_player",
                       label = "Spiller",
                       choices = df()$Player, options = list(
                           placeholder = 'Choose player',
                           onInitialize = I('function() { this.setValue(""); }')),
                       multiple = FALSE)
    })
    
    output$sammenlign <- renderUI({
        selectizeInput(inputId = "select_var_comp",
                       label = "Variable",
                       choices = (names(df())),options = list(
                           placeholder = 'Choose variable',
                           onInitialize = I('function() { this.setValue(""); }')),
                       multiple = FALSE)
        
    })
    
    output$xplotly <- renderUI({
        selectizeInput(inputId = "xplotly",
                       label = "X axis",
                       choices = (names(df())), options = list(
                           placeholder = 'Choose variable on X axis',
                           onInitialize = I('function() { this.setValue(""); }')),
                       multiple = FALSE)
        
    })
    
    output$yplotly <- renderUI({
        selectizeInput(inputId = "yplotly",
                       label = "Y axis",
                       choices = (names(df())), options = list(
                           placeholder = 'Choose variable on Y axis',
                           onInitialize = I('function() { this.setValue(""); }')),
                       multiple = FALSE)
    })
    
    output$sizely <- renderUI({
        selectizeInput(inputId = "sizely",
                       label = "Size of point",
                       choices = (names(df())), options = list(
                           placeholder = "Choose a numeric variable",
                           onInitialize = I('function() { this.setValue(""); }')),
                       multiple = FALSE)
        
    })
    
    output$title <- renderUI({
        textInput("caption", "Your name in the caption")
        
    })
    
    output$nytabel <- renderUI({
        selectizeInput(inputId = "select_tab", 
                       label = "", 
                       choices = names(df()),
                       options = list(
                           placeholder = "Choose columns to be displayed in table"
                       ),
                       multiple = TRUE)
        
    })
    
    
    df_sel <- reactive({
        req(input$select_var)
        df_sel <- df() %>% select(input$select_var)
        
    })
    
    df_tab <- reactive({
        req(input$select_tab)
        df_tab <- df() %>%
            select(input$select_tab)
        
    })
    
    df_comp <- reactive({
        req(input$select_var_comp)
        
        df_comp <- df() %>%
            select(input$select_var_comp)
        
    })
    
    df_player <- reactive({
        req(input$select_player)
        
        df_player <- df() %>%
            select(input$select_player)
        
    })
    
    output$compplot <- renderPlot({
        
        d1 <- ggplot(df(), aes_(as.name(input$select_var_comp))) +
            geom_dotplot(color = "lightgrey", alpha = 0.7, stackdir = "centerwhole") +
            labs(title = as.name(input$select_player)) +
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.text.x = element_text(size = 12),
                  panel.grid.major = element_blank(),
                  plot.title = element_text(size = 20, hjust = 0.5))
        
        d2 <- d1 + gghighlight(Player == as.name(input$select_player))
        
        d2
        
    })
    output$plot <- renderPlot({
        
        ggplot(df(), aes_string(x = as.name(input$xplotly), y = as.name(input$yplotly), size = as.name(input$sizely))) +
        geom_point(alpha = 0.5) +
        #geom_text_repel(label = df()$Player) +
        labs(caption = paste0("Created by ", input$caption," with courtesy of data_scimon")) 
      
    })
    
    output$plotly <- renderPlotly({
        
        plot <- plot_ly(df(), x = ~get(input$xplotly), y = ~get(input$yplotly), size = ~get(input$sizely), text = ~paste("", Player), 
                        mode = "markers", type = "scatter") %>%
            layout(title = "",
                   xaxis = list(title = input$xplotly),
                   yaxis = list(title = input$yplotly))
        
        
        plot
        
    })
    
    
    output$rendered_file <- DT::renderDataTable({
        df_tab()
        
    })
    
}

shinyApp(ui, server)
