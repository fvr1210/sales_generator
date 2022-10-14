#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(data.table)
library(prophet)
library(recipes)
library(tidyverse)
library(lubridate)

# source


# eval(parse("scripts/create_holiday.R", encoding="UTF-8"))

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Sales Generator", titleWidth = 320 # extend width because of the longer title
                    ),
                    # sidebare ---------
                    dashboardSidebar(disable = TRUE),
                    #* Sales Generator ----
    dashboardBody(tabItem(tabName = "generator",
                          
            fluidRow(    
                tags$h4("Sales from to"),
                dateInput("sales_from", "Start date:", value = Sys.Date()-720, format = "dd/mm/yyyy"),
                dateInput("sales_to", "End date:", value  = Sys.Date(), format = "dd/mm/yyyy")
                ),             
            
                                
                                 
            fluidRow(  
                h3("Master Data"), 
                #  EXT_PROD_ID
                column(5, textInput(inputId = "EXT_PROD_ID", label = "Provide EXT_PROD_ID for which data is generated", value = "", width = NULL, placeholder = NULL)),
                    verbatimTextOutput("EXT_PROD_ID"),
                #  EXT_LOC_ID
                column(5, textInput(inputId = "EXT_LOC_ID", label = "Provide EXT_PROD_ID for which data is generated", value = "", width = NULL, placeholder = NULL)),
                verbatimTextOutput("EXT_LOC_ID"),
                #  LOC_TCD
                column(5,textInput(inputId = "LOC_TCD", label = "Provide LOC_TCD for which data is generated", value = "", width = NULL, placeholder = NULL)),
                verbatimTextOutput("LOC_TCD"),
                #  SALES_UOM
                column(5, textInput(inputId = "SALES_UOM", label = "Provide SALES_UOM for which data is generated", value = "", width = NULL, placeholder = NULL)),
                verbatimTextOutput("SALES_UOM"),
                #  SALES_UOM
                column(5, textInput(inputId = "LOC_CURRENCY", label = "Provide LOC_CURRENCY for which data is generated", value = "", width = NULL, placeholder = NULL)),
                verbatimTextOutput("LOC_CURRENCY")),
                
           
                
            fluidRow(    
            h3("Variablen weight"),
                tags$h4("Week Day"),
                
                    column(3, numericInput(inputId="Monday", label = 'Weight Monday:',
                                          min=0, value=1)),
                    column(3, numericInput(inputId="Tuesday", label = 'Weight Tuesday:',
                                          min=0,  value=1)),
                    column(3, numericInput(inputId="Wendnesday", label = 'Weight Wednesday:',
                                          min=0, value=1)),
                    column(3, numericInput(inputId="Thursday", label = 'Weight Thursday:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="Friday", label = 'Weight Friday:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="Saturday", label = 'Weight Saturday:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="Sunday", label = 'Weight Sunday:',
                                   min=0, value=1))),
            
            fluidRow( 
                tags$h4("Month"),
            
                    column(3, numericInput(inputId="January", label = 'Weight January:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="February", label = 'Weight February:',
                                   min=0,  value=1)),
                    column(3, numericInput(inputId="March", label = 'Weight March:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="April", label = 'Weight April:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="Mai", label = 'Weight Mai:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="June", label = 'Weight June:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="July", label = 'Weight July:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="August", label = 'Weight August:',
                                    min=0, value=1)),
                    column(3, numericInput(inputId="September", label = 'Weight September:',
                                    min=0,  value=1)),
                    column(3, numericInput(inputId="October", label = 'Weight October:',
                                    min=0, value=1)),
                    column(3, numericInput(inputId="November", label = 'Weight November:',
                                    min=0, value=1)),
                    column(3, numericInput(inputId="Dezember", label = 'Weight Dezember:',
                                     min=0, value=1))
                ),
            
            fluidRow(    
                tags$h4("Year"),
                
                column(3, numericInput(inputId="2020", label = 'Weight 2020:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="2021", label = 'Weight 2021:',
                                       min=0,  value=1)),
                column(3, numericInput(inputId="2022", label = 'Weight 2022:',
                                       min=0, value=1))),
            
            fluidRow( 
                tags$h4("Holiday"),
                
                column(3, numericInput(inputId="Christi.Himmelfahrt", label = 'Weight Himmelfahrt:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="Erster.Mai", label = 'Weight Erster Mai:',
                                       min=0,  value=1)),
                column(3, numericInput(inputId="Erster.Weihnachtstag", label = 'Weight Weihnachtstag:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="Karfreitag", label = 'Weight Karfreitag:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="Neujahr", label = 'Weight Neujahr:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="Ostermontag", label = 'Weight Ostermontag:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="Pfingstmontag", label = 'Weight Pfingstmontag:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="Tag.der.Deutschen.Einheit", label = 'Weight Tag der Deutschen Einheit:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="Zweiter.Weihnachtstag", label = 'Weight Weihnachtstag:',
                                       min=0,  value=1))
            )

    
                            
                
                
                    )))


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    

    
    # Date
    dates <- reactive({c(input$sales_to, input$sales_from)})
    
    df1 <- reactive({
    obs_days <- as_date(ymd(dates()[2]):ymd(dates()[1]))
    # 
    df1 <- data.frame(obs_days)  %>% 
        mutate(wday = weekdays(obs_days)) %>% 
        mutate(month = months(obs_days)) %>% 
        mutate(year = year(obs_days)) 
    
    
     

    }
    )
    
    observe(print(df1()))
    # 

    
    
    # Master Data
    output$EXT_PROD_ID <- renderText({ input$EXT_PROD_ID })
    output$EXT_LOC_ID <- renderText({ input$EXT_LOC_ID })
    output$LOC_TCD <- renderText({ input$LOC_TCD })
    output$SALES_UOM <- renderText({ input$SALES_UOM })
    output$LOC_CURRENCY <- renderText({ input$LOC_CURRENCY })
    
    
    
    
    
    
    

    # 
}



# Run the application 
shinyApp(ui = ui, server = server)
