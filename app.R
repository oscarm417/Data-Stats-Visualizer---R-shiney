#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
    #Navigation Bar
    navbarPage(
        "William Sonoma Data Lab",
        #TAB 1
        tabPanel("Manage Data",
                 sidebarLayout(
                     sidebarPanel(
                         fileInput("file1",
                                    "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                         checkboxInput("header","Header",TRUE),
                         radioButtons("disp","Display",
                                      choices = c(Head = "head",
                                                  All = "all"),
                                      selected = "head")
                     ),
                     mainPanel(
                         h1("Data Preview"),
                         tableOutput("contents")
                     )
                 ),
         ),
        #TAB 2
        tabPanel("Summary",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("pTypes",
                                     "Select plot",
                                     choices = c("Bar Plot",
                                                 "Histogram",
                                                 "Box Plot",
                                                 "Pie Chart"),
                                     selected = "Histogram"),
                                     
                         selectInput("varSelect",
                                     "Select Data",
                                     choices = NULL,
                                     selected = NULL),
                         
                         selectInput("groupSelect",
                                     "Select Groupby",
                                     choices = NULL,
                                     selected = NULL),
                         h1(" "),
                         sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 30),
                         radioButtons("extension", "Save As:",
                                      choices = c("png", "pdf", "svg"), inline = TRUE),
                         downloadButton("download", "Save Plot")
                 
                     ),
                     mainPanel(
                         h1("Summary Plots"),
                         plotOutput("selectedPlot")                        
                     )
                 )),
        #TAB3
        tabPanel("Relationships",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("varSelectR1",
                                     "Select X",
                                     choices = NULL,
                                     selected = NULL),
                         
                         
                         selectInput("varSelectR2",
                                     "Select Y",
                                     choices = NULL,
                                     selected = NULL),
                         
                         radioButtons("extension2",
                                      "Save As:",
                                      choices = c("png",
                                                  "pdf", 
                                                  "svg"),
                                      inline = TRUE),

                         downloadButton("download2", "Save Plot")

                     ),
                     mainPanel(
                         h1("Data Relationships"),
                         plotOutput("selectedPlot2") 
                         
                     )
                 )),
        tabPanel("Time Visualizations",
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("timeSelect",
                                     "Select Variable",
                                     choices = NULL,
                                     selected = NULL),
                         selectInput("timeGroupby",
                                     "Select Group By",
                                     choices = NULL,
                                     selected =NULL),
                         selectInput("timeIndex",
                                     "Select Time Interval",
                                     choices = NULL,
                                     selected = NULL),
                         radioButtons("extension3",
                                      "Save As:",
                                      choices = c("png",
                                                  "pdf", 
                                                  "svg"),
                                      inline = TRUE),
                         downloadButton("download3", "Save Plot")
                     ),
                     mainPanel(
                         h1("Time Series Analysis"),
                         plotOutput("selectedPlot3")
                     )
                 ))
    ),

    # Sidebar with a slider input for number of bins 
    #sidebarLayout(

        # Show a plot of the generated distribution
        #mainPanel(
         #  plotOutput("distPlot")
   #    # )
   # )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    df_upload <- reactive({
        require(input$file1)
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        df <- read.csv(inFile$datapath, header = input$header)
        return(na.omit(df))
    })
    observeEvent(df_upload(),{
        updateSelectInput(session,"varSelect",choices = colnames(df_upload()))
        updateSelectInput(session,"groupSelect",choices =colnames(df_upload()))
        updateSelectInput(session,"varSelectR1",choices = colnames(df_upload()))
        updateSelectInput(session,"varSelectR2",choices = colnames(df_upload()))
        updateSelectInput(session,"timeSelect",choices = colnames(df_upload()))
        updateSelectInput(session,"timeGroupby",choices = colnames(df_upload()))
        updateSelectInput(session,"timeIndex",choices = colnames(df_upload()))
    })
    
    output$contents <- renderTable({
        
        df<- df_upload()
        
        if(input$disp =="head"){
            return(head(df))
        }else{
            return(df)
        }
        
    })
    
    barPlot <- reactive({
        ggplot(data = df_upload(),aes_string(x=input$varSelect)) + geom_bar(alpha = .5,stat='count')
        
    })
    
    histPlot <- reactive({
        ggplot(data = df_upload(),aes_string(x=input$varSelect, fill = input$groupSelect, color = input$groupSelect)) + geom_histogram(bins = input$bins,alpha = .5)
        
    })
    
    boxPlot <- reactive({
        ggplot(data = df_upload(),aes_string(x=input$varSelect, fill = input$groupSelect, color = input$varSelect)) + geom_boxplot()
        
        
    })
    
    piePlot <- reactive({
        ggplot(data=df_upload(), aes_string(x="1", y=input$varSelect, fill = input$groupSelect)) +
            geom_col(stat="identity", width=1) +
            coord_polar("y", start=0)
        
    })
    
    scatterPlot <- reactive({
        ggplot(data = df_upload(), aes_string(x= input$varSelectR1, y=input$varSelectR2)) +
           geom_point(shape=1) +    
           geom_smooth(method=lm) 
    })
    

    graphInput <- reactive({
        switch(input$pTypes,
               "Bar Plot" = barPlot(),
               "Histogram" = histPlot(),
               "Box Plot" = boxPlot(),
               "Pie Chart" = piePlot()
               )
    })
    
    timeseriesPlot <-reactive({
        x<- input$timeIndex
        y<- input$timeSelect
        g<- input$Groupby

        
        ggplot(data = df_upload(), aes_string(x = input$timeIndex , y= input$timeSelect, color = input$timeGroupby))+
            geom_point(aes(group = input$timeGroupby))
    })
    

    
    output$selectedPlot <- renderPlot({
        graphInput()
    })
    
    output$selectedPlot2<-renderPlot({
        scatterPlot()
    })
    
    output$selectedPlot3<-renderPlot({
        timeseriesPlot()
    })
    

    output$download <- downloadHandler(
        filename = function() {
            paste("histogram", input$extension, sep = ".")
        },
        content = function(file){
            ggsave(file, graphInput(), device = input$extension)
        }
    )
    
    output$download2 <- downloadHandler(
        filename = function() {
            paste("scatter", input$extension2, sep = ".")
        },
        content = function(file){
            ggsave(file, scatterPlot(), device = input$extension2)
        }
    )
    
    output$download3<- downloadHandler(
        filename = function() {
            paste("series", input$extension3, sep = ".")
        },
        content = function(file){
            ggsave(file, timeseriesPlot(), device = input$extension3)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
