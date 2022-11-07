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
library(mltools)
library(tidyverse)
library(lubridate)
library(DT)
library(shinyjs)
library(tsintermittent)


# source

eval(parse("scripts/create_holiday.R", encoding="UTF-8"))
eval(parse("scripts/closed_days.R", encoding="UTF-8"))

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Sales Generator", titleWidth = 320 # extend width because of the longer title
                    ),
                    # sidebare ---------
                    dashboardSidebar(disable = TRUE,
                                     tagList(                       # Aligne the checkboxes left; code from https://stackoverflow.com/questions/29738975/how-to-align-a-group-of-checkboxgroupinput-in-r-shiny
                                       tags$head(
                                         tags$style(
                                           HTML(                   # Change position of different elements 
                                                        ".row {    
                    margin-left: 15px;
                    margin-right: 15px;
                    }",
                                                        
                                                        ".shiny-input-container{ 
                    margin-left: 0px;
                    margin-right: 0px;
                    }",
                                             
                                             
                                             ".shiny-bound-output{ 
                    margin-left: 0px;
                    margin-right: 0px;
                    }",
                                           ))))
                                     
                                     ),
                    #* Sales Generator ----
                    
                  
                    
                    
    dashboardBody(tabItem(tabName = "generator",
                          
           fluidRow(
               
               fileInput("target_upload", "Choose CSV File",
                         accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
               radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=";",inline=TRUE),
               ),


           # 
                          
            fluidRow(    
                tags$h4("Sales from to"),
                dateInput("sales_from", "Start date:", value = Sys.Date()-729, format = "dd/mm/yyyy"),
                dateInput("sales_to", "End date:", value  = Sys.Date(), format = "dd/mm/yyyy")
                ),             
            
                                
                                 
            fluidRow(  
                h3("Master Data"), 
                #  EXT_PROD_ID
                column(2, textInput(inputId = "EXT_PROD_ID", label = "EXT_PROD_ID", value = "", width = NULL, placeholder = NULL)),
                    verbatimTextOutput("EXT_PROD_ID"),
                #  EXT_LOC_ID
                column(2, textInput(inputId = "EXT_LOC_ID", label = "EXT_LOC_ID", value = "", width = NULL, placeholder = NULL)),
                verbatimTextOutput("EXT_LOC_ID"),
                #  LOC_TCD
                column(2,textInput(inputId = "LOC_TCD", label = "LOC_TCD", value = "1040", width = NULL, placeholder = NULL)),
                verbatimTextOutput("LOC_TCD"),
                #  SALES_UOM
                column(2, textInput(inputId = "SALES_UOM", label = "SALES_UOM", value = "ST", width = NULL, placeholder = NULL)),
                verbatimTextOutput("SALES_UOM"),
                #  SALES_UOM
                column(2, numericInput(inputId = "PRICE", label = "Price", value = "1.95")),
                verbatimTextOutput("PRICE"),
                #  SALES_UOM
                column(2, textInput(inputId = "LOC_CURRENCY", label = "LOC_CURRENCY", value = "EUR", width = NULL, placeholder = NULL)),
                verbatimTextOutput("LOC_CURRENCY")),
                
           
                
            fluidRow(    
            h3("Variablen weight"),
                tags$h4("Week Day"),
                
                    column(3, numericInput(inputId="wday_Montag", label = 'Weight Monday:', 1)),
                    column(3, numericInput(inputId="wday_Dienstag", label = 'Weight Tuesday:', 1)),
                    column(3, numericInput(inputId="wday_Mittwoch", label = 'Weight Wednesday:', 1)),
                    column(3, numericInput(inputId="wday_Donnerstag", label = 'Weight Thursday:', 1)),
                    column(3, numericInput(inputId="wday_Freitag", label = 'Weight Friday:', 1)),
                    column(3, numericInput(inputId="wday_Samstag", label = 'Weight Saturday:', 1)),
                    column(3, numericInput(inputId="wday_Sonntag", label = 'Weight Sunday:', 1))),
            
            fluidRow( 
                tags$h4("Month"),
            
                    column(3, numericInput(inputId="month_Januar", label = 'Weight January:', value=1)),
                    column(3, numericInput(inputId="month_Februar", label = 'Weight February:', value=1)),
                    column(3, numericInput(inputId="month_März", label = 'Weight March:', value=1)),
                    column(3, numericInput(inputId="month_April", label = 'Weight April:', value=1)),
                    column(3, numericInput(inputId="month_Mai", label = 'Weight Mai:', value=1)),
                    column(3, numericInput(inputId="month_Juni", label = 'Weight June:', value=1)),
                    column(3, numericInput(inputId="month_Juli", label = 'Weight July:', value=1)),
                    column(3, numericInput(inputId="month_August", label = 'Weight August:', value=1)),
                    column(3, numericInput(inputId="month_September", label = 'Weight September:', value=1)),
                    column(3, numericInput(inputId="month_Oktober", label = 'Weight October:', value=1)),
                    column(3, numericInput(inputId="month_November", label = 'Weight November:', value=1)),
                    column(3, numericInput(inputId="month_Dezember", label = 'Weight Dezember:', value=1))
                ),
            
            fluidRow(    
                tags$h4("Year"),
                
                column(2, uiOutput("year")),
                
                column(3, numericInput(inputId="year_2020", label = 'Weight 2020:', value=1)),
                column(3, numericInput(inputId="year_2021", label = 'Weight 2021:', value=1)),
                column(3, numericInput(inputId="year_2022", label = 'Weight 2022:', value=1)),
                column(3, numericInput(inputId="year_2023", label = 'Weight 2023:', value=1)),
                ),
          
               
            
            fluidRow( 
                tags$h4("Holiday"),
                
                column(3, numericInput(inputId="holiday_Christi.Himmelfahrt", label = 'Weight Himmelfahrt:',
                                     value=1)),
                column(3, numericInput(inputId="holiday_Erster.Mai", label = 'Weight Erster Mai:',
                                      value=1)),
                column(3, numericInput(inputId="holiday_Erster.Weihnachtstag", label = 'Weight Erster Weihnachtstag:',
                                     value=1)),
                column(3, numericInput(inputId="holiday_Karfreitag", label = 'Weight Karfreitag:',
                                     value=1)),
                column(3, numericInput(inputId="holiday_Neujahr", label = 'Weight Neujahr:',
                                     value=1)),
                column(3, numericInput(inputId="holiday_Ostermontag", label = 'Weight Ostermontag:',
                                     value=1)),
                column(3, numericInput(inputId="holiday_Pfingstmontag", label = 'Weight Pfingstmontag:',
                                     value=1)),
                column(3, numericInput(inputId="holiday_Tag.der.Deutschen.Einheit", label = 'Weight Tag der Deutschen Einheit:',
                                     value=1)),
                column(3, numericInput(inputId="holiday_Zweiter.Weihnachtstag", label = 'Weight Zweiter Weihnachtstag:',
                                      value=1))
            ), 
            # 
            fluidRow(
              tags$h4("DIFs (do not change the DIF Name)"),
                 numericInput(inputId = "n_DIF",label = "Number of DIFs", 2),
                 # place to hold dynamic inputs
                 column(2, uiOutput("DIF_NAME")),
                 column(2, uiOutput("DIF_START")),
                 column(2, uiOutput("DIF_END")),
                 column(2, uiOutput("DIF_WEIGHT")),
                
            ),
           
           
           fluidRow(
             tags$h4("Offers (do not change the Offer Name)"),
             numericInput(inputId = "n_OFR", label = "Number of Offers", value = 2),
           # place to hold dynamic inputs
           column(2, uiOutput("OFR_NAME")),
           column(2, uiOutput("OFR_START")),
           column(2, uiOutput("OFR_END")),
           column(2, uiOutput("OFR_SALES")),
           column(2, uiOutput("OFR_PRICE")), 
           column(2, uiOutput("OFR_OFR_ID"))
           ),
           
           
           
            # 
            
            fluidRow(
                tags$h4("Sales Data"),
                
                
                radioButtons("sales_dist", "Sales type:",
                             c("Normal" = "Normal",
                               "Intermittent" = "Intermittent")),
                
                column(3, uiOutput("sales_selection"))),
# 
#                 column(3, numericInput(inputId="lambda", label = 'Expected average sales value',
#                                        min = 1, value=5)),
            
        fluidRow(
            actionButton("generate", "Generate Sales data", style="color: #fff; background-color: #d73925; border-color: #c34113;
                                border-radius: 10px; 
                             border-width: 2px")),

            fluidRow(
                # br(),
                # dataTableOutput("wei_stand_dis_table"),
                br(),
                # dataTableOutput("sales_table"),
                plotOutput("sales_plot")
            ),

            # Button
            downloadButton("downloadData", "Download"),
           
           textOutput("inputValues")
            
                            
                
                
                    )))



        
        

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    df_products_upload <- reactive({
        inFile <- input$target_upload
        if (is.null(inFile))
            return(NULL)
        df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
        return(df)
    })
    


    # 
     observeEvent(input$target_upload, {
         if(!is.null(df_products_upload()))
             df_w <- df_products_upload() 

             updateNumericInput(session, "wday_Montag",  value = if_else(!is.null(df_w$Montag_w), df_w$Montag_w, 1), min = 1)
             updateNumericInput(session, "wday_Dienstag",  value = if_else(!is.null(df_w$Dienstag_w), df_w$Dienstag_w, 1), min = 1)
             updateNumericInput(session, "wday_Mittwoch",  value = if_else(!is.null(df_w$Mittwoch_w), df_w$Mittwoch_w, 1), min = 1)
             updateNumericInput(session, "wday_Donnerstag",  value = if_else(!is.null(df_w$Donnerstag_w), df_w$Donnerstag_w, 1), min = 1)
             updateNumericInput(session, "wday_Freitag",  value = if_else(!is.null(df_w$Freitag_w), df_w$Freitag_w, 1), min = 1)
             updateNumericInput(session, "wday_Samstag",  value = if_else(!is.null(df_w$Samstag_w), df_w$Samstag_w, 1), min = 1)
             updateNumericInput(session, "wday_Sonntag",  value = if_else(!is.null(df_w$Sonntag_w), df_w$Sonntag_w, 1), min = 1)
             
             updateNumericInput(session, "month_Januar",  value = if_else(!is.null(df_w$Januar_w), df_w$Januar_w, 1), min = 1)
             updateNumericInput(session, "month_Februar",  value = if_else(!is.null(df_w$Februar_w), df_w$Februar_w, 1), min = 1)
             updateNumericInput(session, "month_März",  value = if_else(!is.null(df_w$März_w), df_w$März_w, 1), min = 1)
             updateNumericInput(session, "month_April",  value = if_else(!is.null(df_w$April_w), df_w$April_w, 1), min = 1)
             updateNumericInput(session, "month_Mai",  value = if_else(!is.null(df_w$Mai_w), df_w$Mai_w, 1), min = 1)
             updateNumericInput(session, "month_Juni",  value = if_else(!is.null(df_w$Juni_w), df_w$Juni_w, 1), min = 1)
             updateNumericInput(session, "month_Juli",  value = if_else(!is.null(df_w$Juli_w), df_w$Juli_w, 1), min = 1)
             updateNumericInput(session, "month_August",  value = if_else(!is.null(df_w$August_w), df_w$August_w, 1), min = 1)
             updateNumericInput(session, "month_September",  value = if_else(!is.null(df_w$September_w), df_w$September_w, 1), min = 1)
             updateNumericInput(session, "month_Oktober",  value = if_else(!is.null(df_w$Oktober_w), df_w$Oktober_w, 1), min = 1)
             updateNumericInput(session, "month_November",  value = if_else(!is.null(df_w$November_w), df_w$November_w, 1), min = 1)
             updateNumericInput(session, "month_Dezember",  value = if_else(!is.null(df_w$Dezember_w), df_w$Dezember_w, 1), min = 1)
              
             updateNumericInput(session, "year_2020",  value = if_else(!is.null(df_w$w2020), df_w$w2020, 1), min = 1)
             updateNumericInput(session, "year_2021",  value = if_else(!is.null(df_w$w2021), df_w$w2021, 1), min = 1)
             updateNumericInput(session, "year_2022",  value = if_else(!is.null(df_w$w2022), df_w$w2022, 1), min = 1)

             updateNumericInput(session, "holiday_Christi Himmelfahrt",  value = if_else(!is.null(df_w$christi_w), df_w$christi_w, 1), min = 1)
             updateNumericInput(session, "holiday_Erster Mai",  value = if_else(!is.null(df_w$erstermai_w), df_w$erstermai_w, 1), min = 1)
             updateNumericInput(session, "holiday_Erster Weihnachtstag",  value = if_else(!is.null(df_w$erster_wt), df_w$erster_wt, 1), min = 1)
             updateNumericInput(session, "holiday_Karfreitag",  value = if_else(!is.null(df_w$karfrei_w), df_w$karfrei_w, 1), min = 1)
             updateNumericInput(session, "holiday_Neujahr",  value = if_else(!is.null(df_w$nj_w), df_w$nj_w, 1), min = 1)
             updateNumericInput(session, "holiday_Ostermontag",  value = if_else(!is.null(df_w$osterm_w), df_w$osterm_w, 1), min = 1)
             updateNumericInput(session, "holiday_Pfingstmontag",  value = if_else(!is.null(df_w$pfingstm_w), df_w$pfingstm_w, 1), min = 1)
             updateNumericInput(session, "holiday_Tag.der Deutschen.Einheit",  value = if_else(!is.null(df_w$tde_w), df_w$tde_w, 1), min = 1)
             updateNumericInput(session, "holiday_Zweiter Weihnachtstag",  value = if_else(!is.null(df_w$zweiter_wt), df_w$zweiter_wt, 1), min = 1)

             updateNumericInput(session, "n_OFR",  value = if_else(!is.null(df_w$n_ofr), df_w$n_ofr, 1), min = 0)
             })
     
     
     
     # dynamic year input ----
     # observe({
     #   
     #   dates <- c(input$sales_to, input$sales_from)
     #   
     #   
     #   obs_days <- as_date(ymd(dates[2]):ymd(dates[1]))
     #   #
     #   
     #   df_d <- data.frame(obs_days) %>% 
     #           as_tibble() %>% 
     #           mutate(year = year(obs_days)) %>% 
     #           distinct(year) %>% 
     #           arrange(year)
     #   
     #   
     #   output$year = renderUI({
     #     input_list <- lapply(df_d$year[1]:df_d$year[nrow(df_d)], function(i) {
     #       # for each dynamically generated input, give a different name
     #       Years <- paste("year_X", i, sep = "")
     #       numericInput(Years, paste("Year ", i, sep = ""), 1)
     #     })
     #     do.call(tagList, input_list)
     #   })
     #     })
     
     
     # df_year <- eventReactive(input$generate,{
     #   
     #   dates <- c(input$sales_to, input$sales_from)
     #   
     #   
     #   obs_days <- as_date(ymd(dates[2]):ymd(dates[1]))
     #   
     #   df_d <- data.frame(obs_days) %>% 
     #     as_tibble() %>% 
     #     mutate(year = year(obs_days)) %>% 
     #     distinct(year) %>% 
     #     arrange(year)
     #   
     #   
     #   
     #   df_year <-  data.frame(
     #     
     #     year_weight =  paste(lapply(df_d$year[1]:df_d$year[nrow(df_d)], function(i) {
     #       inputName <- paste("Weight Year", i, sep = " ")
     #       input[[inputName]]}))
     #   )
     #   
     #   
     #   cbind(df_d, df_year)
       
           # 
           # })
       
     # observe(print(df_year()))
 
     
     
     # dynamic DIF input ----
     observeEvent(input$n_DIF, {
       
    
       
       output$DIF_START = renderUI({
         input_list <- lapply(1:input$n_DIF, function(i) {
           # for each dynamically generated input, give a different name
           DIF_Start <- paste("DIF_start", i, sep = "_")
           dateInput(DIF_Start, "Start of DIF:", value = Sys.Date()-729, format = "dd/mm/yyyy")
         })
         do.call(tagList, input_list)
       })
       
       output$DIF_END = renderUI({
         input_list <- lapply(1:input$n_DIF, function(i) {
           # for each dynamically generated input, give a different name
           DIF_End <- paste("DIF_end", i, sep = "_")
           dateInput(DIF_End, "End of DIF:", value = Sys.Date(), format = "dd/mm/yyyy")
         })
         do.call(tagList, input_list)
       })
       
       output$DIF_NAME = renderUI({
         input_list <- lapply(1:input$n_DIF, function(i) {
           # for each dynamically generated input, give a different name
           DIF_Name <- paste("DIF_name", i, sep = "_")
           textInput(DIF_Name, "DIF Name", value = paste("DIF", i, sep = "_"))
         })
         do.call(tagList, input_list)
       })
       
       output$DIF_WEIGHT = renderUI({
         input_list <- lapply(1:input$n_DIF, function(i) {
           # for each dynamically generated input, give a different name
           DIF_Weight <- paste("DIF_weight", i, sep = "_")
           numericInput(DIF_Weight, "DIF Weight", 1)
         })
         do.call(tagList, input_list)
       })
       
       
       
     })
     
     
     # dynamic OFR input ----
     
     
     
     observeEvent(input$n_OFR, {
       
       
       
       output$OFR_START = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_Start <- paste("OFR_start", i, sep = "_")
           dateInput(OFR_Start, "Start of Offer:", value = Sys.Date()-729, format = "dd/mm/yyyy")
         })
         do.call(tagList, input_list)
       })
       
       output$OFR_END = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_End <- paste("OFR_end", i, sep = "_")
           dateInput(OFR_End, "End of Offer:", value = Sys.Date(), format = "dd/mm/yyyy")
         })
         do.call(tagList, input_list)
       })
       
       output$OFR_NAME = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_Name <- paste("OFR_name", i, sep = "_")
           textInput(OFR_Name, "Offer Name", value = paste("OFR", i, sep = "_"))
         })
         do.call(tagList, input_list)
       })
       
       output$OFR_SALES = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_Sales <- paste("OFR_sales", i, sep = "_")
           numericInput(OFR_Sales, "Expected Offer Sales", 0)
         })
         do.call(tagList, input_list)
       })
       
       
       output$OFR_PRICE = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_Price <- paste("OFR_price", i, sep = "_")
           numericInput(OFR_Price, "OFR Price Reduction (in %)", 0)
         })
         do.call(tagList, input_list)
       })
       
       output$OFR_OFR_ID = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_Ofr_Id <- paste("OFR_ofr_id", i, sep = "_")
           textInput(OFR_Ofr_Id, "Offer id", value = '')
         })
         do.call(tagList, input_list)
       })
       
       
     })
     
     
     observeEvent(input$target_upload, {
       
       
       if(!is.null(df_products_upload()))
         df_w <- df_products_upload() 
       
       
       
       output$OFR_START = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_Start <- paste("OFR_start", i, sep = "_")
           dateInput(OFR_Start, "Start of Offer:", value = Sys.Date()-729, format = "dd/mm/yyyy")
         })
         do.call(tagList, input_list)
       })
       
       output$OFR_END = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_End <- paste("OFR_end", i, sep = "_")
           dateInput(OFR_End, "End of Offer:", value = Sys.Date(), format = "dd/mm/yyyy")
         })
         do.call(tagList, input_list)
       })
       
       output$OFR_NAME = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_Name <- paste("OFR_name", i, sep = "_")
           textInput(OFR_Name, "Offer Name", value = paste("OFR", i, sep = "_"))
         })
         do.call(tagList, input_list)
       })
       
       output$OFR_SALES = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_Sales <- paste("OFR_sales", i, sep = "_")
           numericInput(OFR_Sales, "Expected Offer Sales", 0)
         })
         do.call(tagList, input_list)
       })
       
       
       output$OFR_PRICE = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_Price <- paste("OFR_price", i, sep = "_")
           numericInput(OFR_Price, "OFR Price Reduction (in %)", 0)
         })
         do.call(tagList, input_list)
       })
       
       output$OFR_OFR_ID = renderUI({
         input_list <- lapply(1:input$n_OFR, function(i) {
           # for each dynamically generated input, give a different name
           OFR_Ofr_Id <- paste("OFR_ofr_id", i, sep = "_")
           textInput(OFR_Ofr_Id, "Offer id", value = '')
         })
         do.call(tagList, input_list)
       })
       
       
     })
     
     
     
     # dynamic sales input ----
     observeEvent(input$sales_dist, {
       
       
       
       output$sales_selection = renderUI({
         switch(input$sales_dist,
                Normal =  numericInput(inputId="lambda", label = 'Expected average sales value', min = 1, value=5),
                Intermittent = list(
                  numericInput(inputId="intervall", label = "Average intermittent demand interval", min = 1, value=5), 
                  numericInput(inputId="mean_demand", label = 'Average Mean demand of non-zero demands (must be greater than 1)', min = 1.1, value=5)))
  
       })
       
       
     })




    # Create dataset and Weights
    
    # observe({print(names(input))})
    
    
    df_full <- eventReactive(input$generate,{
      
      
    # DIF dataset  ----
      df_DIF <-  data.frame(
        
        DIF_name =  paste(lapply(1:input$n_DIF, function(i) {
          inputName <- paste("DIF_name", i, sep = "_")
          input[[inputName]]})),
        
        
        DIF_start =  paste(lapply(1:input$n_DIF, function(i) {
          inputName <- paste("DIF_start", i, sep = "_")
          start <- input[[inputName]]
          start <- format(start, "%Y-%m-%d")
          return(start)
        })),
        
        DIF_end =  paste(lapply(1:input$n_DIF, function(i) {
          inputName <- paste("DIF_end", i, sep = "_")
          end <- input[[inputName]]
          end <- format(end, "%Y-%m-%d")
          return(end)
        })),
        
        DIF_weight =  paste(lapply(1:input$n_DIF, function(i) {
          inputName <- paste("DIF_weight", i, sep = "_")
          input[[inputName]]}))
        
      )
      
      df_DIF <- df_DIF %>% 
        as_tibble() %>% mutate(DIF_start = ymd(DIF_start),
                               DIF_end = ymd(DIF_end),
                               DIF_weight = as.double(DIF_weight))
                               # DIF_price = as.double(DIF_price_r))
      
      
      ### DIF weights ----
      dates <- c(input$sales_to, input$sales_from)
      
      
      obs_days <- as_date(ymd(dates[2]):ymd(dates[1]))
      #
      
      df_d <- data.frame(obs_days) %>% as_tibble()
      
      dif <- data.frame()
      listofdfs <- list()
      
      for(i in 1:nrow(df_DIF)) {
        dif <- df_d %>%
          mutate(DIF = if_else(df_DIF$DIF_start[i] <= df_d$obs_days & df_DIF$DIF_end[i] >= df_d$obs_days, 1, 0) ) %>%
          mutate(DIF = case_when(
                                DIF == 1 ~ log(DIF*df_DIF$DIF_weight[i]), 
                                DIF == 0 ~ NA_real_,
                                DIF == 1 & df_DIF$DIF_weight[i] == 0 ~ -99,
                                TRUE ~ DIF
            )) %>% 
          rename(!!paste("DIF", i, sep = "_") := "DIF")
        listofdfs[[i]] <- dif
      }
      
      df_dif <- reduce(listofdfs, full_join, by = "obs_days")
      
      
      
      
      
      
      
      
  


 # input weights ----
        dates <- c(input$sales_to, input$sales_from)


        obs_days <- as_date(ymd(dates[2]):ymd(dates[1]))
        #
        df_d <- data.frame(obs_days)  %>%
            mutate(wday = weekdays(obs_days)) %>%
            mutate(month = months(obs_days)) %>%
            mutate(year = year(obs_days))

        holiday <- make_holidays_df(c(df_d$year), "DE")

        df_dh <- left_join(df_d, holiday, by = c("obs_days" = "ds"))


        # df1 <- df_dh %>%
        #     mutate(year = as.factor(year)) %>%
        #     recipe( ~ .) %>%
        #     step_dummy(wday, month, year, holiday, one_hot = T ) %>%
        #     prep() %>%
        #     bake(df_dh) %>%
        #     replace(is.na(.), 0) %>%
        #     select(where(~ any(. != 0)))
        # df_3 <- df %>% 
        #   as_tibble %>% 
        #   mutate(wday = as.factor(wday), month = as.factor(month), year = as.factor(year))
        # 
        # df_3 <- one_hot(as.data.table(df_3))
        # 
        # df_3 <- df_3 %>% as_tibble %>%
        #   replace(is.na(.), 0) %>%
        #   select(where(~ any(. != 0)))
        
        df1 <- df_dh %>% 
          as_tibble %>% 
          mutate(wday = as.factor(wday), month = as.factor(month), year = as.factor(year))
          
        df1 <- one_hot(as.data.table(df1))
        
        df1 <- df1 %>% as_tibble %>% 
          replace(is.na(.), 0) %>%
          select(where(~ any(. != 0))) %>% 
          rename_all(~str_replace_all(., "\\s+", "."))



#
#
    input_df <- NULL
    df_row <- NULL

            for(i in 1:length(names(input))){
                df_row <- as.data.frame(cbind(names(input)[i], input[[names(input)[i]]]))
                input_df <- as_tibble(dplyr::bind_rows(input_df, df_row)) %>%
                    select(c(V1, V2))
            }
            names(input_df) <- c("input_id", "input_val")
            input_df

            weight_inputs <- input_df %>%
                filter(grepl('wday_', names(input)) | grepl('holiday_', names(input)) | grepl('year_', names(input)) | grepl('month_', names(input)) ) %>%
                pivot_wider(names_from=c(input_id), values_from=input_val) %>%
                slice(rep(1, each = nrow(df1)))

            df2 <- df1 %>% select(obs_days) %>%
                cbind(.,weight_inputs)

            df2 <- as_tibble(df2)  %>%
                mutate(across(where(is.character), as.double))



            df1_dt <- as.data.table(df1)

            df2_dt <- as.data.table(df2)

            nm1 <- names(df1)[-1]

            setDT(df1_dt)[df2_dt,  (nm1) := Map(`*`, mget(nm1),
                                                mget(paste0("i.", nm1))) , on = "obs_days"]



            log_data <- log(df1_dt[,2:ncol(df1_dt)])

            #

            
#
#
#
            df3 <- df_dh %>%
              cbind(log_data) %>%
              as_tibble() %>%
              mutate(year = as.factor(year)) %>%
              mutate_if(is.numeric, ~ replace(., is.infinite(.), NA)) %>%
              left_join(df_dif) %>% 
              mutate(sum_log = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>%
              mutate(sum = exp(sum_log)) %>% 
              mutate(holiday = if_else(is.na(holiday), "", as.character(holiday))) %>% 
              week_days_null("Montag") %>%
              week_days_null("Dienstag") %>%  
              week_days_null("Mittwoch") %>%  
              week_days_null("Donnerstag") %>%  
              week_days_null("Freitag") %>%  
              week_days_null("Samstag") %>%  
              week_days_null("Sonntag") %>%  
              holidays_null("Christi Himmelfahrt") %>% 
              holidays_null("Erster Mai") %>% 
              holidays_null("Erster Weihnachtstag") %>% 
              holidays_null("Karfreitag") %>% 
              holidays_null("Neujahr") %>% 
              holidays_null("Ostermontag") %>% 
              holidays_null("Pfingstmontag") %>% 
              holidays_null("Tag der Deutschen Einheit") %>% 
              holidays_null("Zweiter Weihnachtstag") 
            
       
         
            
# 
      # generate sales ----
            lambda <- input$lambda

            intervall <- input$intervall
            mean_demand <- input$mean_demand

            df4 <- df3 %>%
                select(obs_days, sum) %>%
                mutate(sales =

                         switch(input$sales_dist,
                                Normal =  rpois(nrow(df3), lambda = lambda),
                                Intermittent = simID(n=1, obs=nrow(df3), idi= intervall, cv2= 0.5, level=mean_demand))

                         ) %>%
                mutate(ss_sales = round(sales*sum), 0) %>%
                mutate(ss_sales =

                         switch(input$sales_dist,
                                Normal =  ss_sales,
                                Intermittent = ss_sales$ts.1)

                ) %>%
                as_tibble
        
    })
    
    observe({print(df_full())})
    # 
    
    
    # OFR dataset  ----
  df_OFR <-   eventReactive(input$generate,{
    
    df_OFR <-  data.frame(
      
      OFR_name =  paste(lapply(1:input$n_OFR, function(i) {
        inputName <- paste("OFR_name", i, sep = "_")
        input[[inputName]]})),
      
      
      OFR_start =  paste(lapply(1:input$n_OFR, function(i) {
        inputName <- paste("OFR_start", i, sep = "_")
        start <- input[[inputName]]
        start <- format(start, "%Y-%m-%d")
        return(start)
      })),
      
      OFR_end =  paste(lapply(1:input$n_OFR, function(i) {
        inputName <- paste("OFR_end", i, sep = "_")
        end <- input[[inputName]]
        end <- format(end, "%Y-%m-%d")
        return(end)
      })),
      
      OFR_sales =  paste(lapply(1:input$n_OFR, function(i) {
        inputName <- paste("OFR_sales", i, sep = "_")
        input[[inputName]]})),
      
      OFR_price =  paste(lapply(1:input$n_OFR, function(i) {
        inputName <- paste("OFR_price", i, sep = "_")
        input[[inputName]]})),
      
      
      OFR_ofr_id =  paste(lapply(1:input$n_OFR, function(i) {
        inputName <- paste("OFR_ofr_id", i, sep = "_")
        input[[inputName]]}))
    )
    
    df_OFR <- df_OFR %>%
      as_tibble() %>% mutate(OFR_start = ymd(OFR_start),
                             OFR_end = ymd(OFR_end),
                             OFR_sales = as.double(OFR_sales),
                             OFR_price = as.double(OFR_price))
    

    
    ## OFR sales ----
    
    ofr <- data.frame()
    listofdfs_ofr <- list()
    
    for(i in 1:nrow(df_OFR)) {
      
      obs_days <- as_date(ymd(df_OFR$OFR_start[i]):ymd(df_OFR$OFR_end[i]))
      #
      
      ofr <- data.frame(obs_days) %>%
        as_tibble()
      
      
      
      lambda <- df_OFR$OFR_sales[i]
      
      
      
      
      ofr <- ofr %>%
        mutate(OFR_ID = df_OFR$OFR_ofr_id[i] ) %>%
        mutate(sales= rpois(nrow(ofr), lambda = lambda)) %>%
        mutate(OFR_price = round(input$PRICE*(1-df_OFR$OFR_price[i]/100), 2))
      listofdfs_ofr[[i]] <- ofr
    }
    
    df_ofrs <- bind_rows(listofdfs_ofr)
    
    # 
    df_ofrs <- df_ofrs %>% filter(sales > 0)


    
    })
# 
    
    
  
    observe({print(df_OFR())})
    



     # 
     # 
     output$sales_plot <- renderPlot(df_full() %>% ggplot() +
                                         geom_line(aes(obs_days, ss_sales)) +
                                         geom_point(df_OFR(), mapping = aes(obs_days, sales), colour = "blue") +
                                         theme_bw())

     #
     #
     #
      df_table <- reactive({

        df_s <-   df_full() %>%
          mutate(EXT_PROD_ID = input$EXT_PROD_ID,
                 EXT_LOC_ID = input$EXT_LOC_ID,
                 LOC_TCD = input$LOC_TCD,
                 SALES_UOM = input$SALES_UOM,
                 LOC_CURRENCY = input$LOC_CURRENCY,
                 GRANULARITY = "D",
                 OFR_ID = "",
                 TIMESTAMP = format(obs_days, "%Y%m%d%H%M%S"),
                 PRC_TCD = "",
                 RETURNED_SALES = "",
                 UNIT_SALES = ss_sales,
                 ZPROMO_FLAG = "",
                 FUNCTION = "I",
                 GROSS_SALES_AMNT = ss_sales * input$PRICE,
                 NET_SALES_AMNT = ss_sales * input$PRICE,
                 CURRENCY_ISO = input$LOC_CURRENCY,
                 UOM_ISO = input$SALES_UOM
          ) %>%
          select(c(EXT_LOC_ID, EXT_PROD_ID, LOC_TCD, GRANULARITY, OFR_ID, TIMESTAMP,
                   PRC_TCD, RETURNED_SALES, UNIT_SALES, SALES_UOM, LOC_CURRENCY, ZPROMO_FLAG, GROSS_SALES_AMNT, NET_SALES_AMNT,
                   CURRENCY_ISO, UOM_ISO, FUNCTION))

        df_os <-     df_OFR() %>%
          mutate(EXT_PROD_ID = input$EXT_PROD_ID,
                 EXT_LOC_ID = input$EXT_LOC_ID,
                 LOC_TCD = input$LOC_TCD,
                 SALES_UOM = input$SALES_UOM,
                 LOC_CURRENCY = input$LOC_CURRENCY,
                 GRANULARITY = "1",
                 TIMESTAMP = format(obs_days, "%Y%m%d%H%M%S"),
                 PRC_TCD = "",
                 RETURNED_SALES = "",
                 UNIT_SALES = sales,
                 ZPROMO_FLAG = "",
                 FUNCTION = "",
                 GROSS_SALES_AMNT = sales * OFR_price,
                 NET_SALES_AMNT = sales * OFR_price,
                 CURRENCY_ISO = input$LOC_CURRENCY,
                 UOM_ISO = input$SALES_UOM
          ) %>%
          select(c(EXT_LOC_ID, EXT_PROD_ID, LOC_TCD, GRANULARITY, OFR_ID, TIMESTAMP,
                   PRC_TCD, RETURNED_SALES, UNIT_SALES, SALES_UOM, LOC_CURRENCY, ZPROMO_FLAG, GROSS_SALES_AMNT, NET_SALES_AMNT,
                   CURRENCY_ISO, UOM_ISO, FUNCTION))

      df <-   rbind(df_s, df_os)


      })

      observe({print(df_table())})
     #
     output$downloadData <- downloadHandler(
         filename = function() {
             paste(input$EXT_PROD_ID, input$EXT_LOC_ID, ".csv", sep = "_")
         },
         content = function(file) {
             write_delim(df_table(), file, delim = ";")
         })


    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
