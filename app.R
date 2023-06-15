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


Sys.setlocale("LC_ALL","C")

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
                
                    column(3, numericInput(inputId="wday_Monday", label = 'Weight Monday:', 1)),
                    column(3, numericInput(inputId="wday_Tuesday", label = 'Weight Tuesday:', 1)),
                    column(3, numericInput(inputId="wday_Wednesday", label = 'Weight Wednesday:', 1)),
                    column(3, numericInput(inputId="wday_Thursday", label = 'Weight Thursday:', 1)),
                    column(3, numericInput(inputId="wday_Friday", label = 'Weight Friday:', 1)),
                    column(3, numericInput(inputId="wday_Saturday", label = 'Weight Saturday:', 1)),
                    column(3, numericInput(inputId="wday_Sunday", label = 'Weight Sunday:', 1))),
            
            fluidRow( 
                tags$h4("Month"),
            
                    column(3, numericInput(inputId="month_January", label = 'Weight January:', value=1)),
                    column(3, numericInput(inputId="month_February", label = 'Weight February:', value=1)),
                    column(3, numericInput(inputId="month_March", label = 'Weight March:', value=1)),
                    column(3, numericInput(inputId="month_April", label = 'Weight April:', value=1)),
                    column(3, numericInput(inputId="month_May", label = 'Weight May:', value=1)),
                    column(3, numericInput(inputId="month_June", label = 'Weight June:', value=1)),
                    column(3, numericInput(inputId="month_July", label = 'Weight July:', value=1)),
                    column(3, numericInput(inputId="month_August", label = 'Weight August:', value=1)),
                    column(3, numericInput(inputId="month_September", label = 'Weight September:', value=1)),
                    column(3, numericInput(inputId="month_October", label = 'Weight October:', value=1)),
                    column(3, numericInput(inputId="month_November", label = 'Weight November:', value=1)),
                    column(3, numericInput(inputId="month_December", label = 'Weight December:', value=1))
                ),
            
            fluidRow(    
                tags$h4("Year"),
                
                column(3, uiOutput("year")),
                
                # column(3, numericInput(inputId="year_2019", label = 'Weight 2019:', value=1)),
                # column(3, numericInput(inputId="year_2020", label = 'Weight 2020:', value=1)),
                # column(3, numericInput(inputId="year_2021", label = 'Weight 2021:', value=1)),
                # column(3, numericInput(inputId="year_2022", label = 'Weight 2022:', value=1)),
                # column(3, numericInput(inputId="year_2023", label = 'Weight 2023:', value=1)),
                ),
          
               
            
            fluidRow( 
                tags$h4("Holiday"),
                
                column(3, numericInput(inputId="holiday_Neujahr", label = 'Weight Neujahr:',
                                       value=0)),
                column(3, numericInput(inputId="holiday_Heilige.Drei.Konige", label = 'Weight Heilige Drei Konige:',
                                       value=0)),
                column(3, numericInput(inputId="holiday_Karfreitag", label = 'Weight Karfreitag:',
                                       value=0)),
                column(3, numericInput(inputId="holiday_Ostern", label = 'Weight Ostern:',
                                       value=0)),
                column(3, numericInput(inputId="holiday_Ostermontag", label = 'Weight Ostermontag:',
                                       value=0)),
                column(3, numericInput(inputId="holiday_Erster.Mai", label = 'Weight Erster Mai:',
                                       value=0)),
                column(3, numericInput(inputId="holiday_Christi.Himmelfahrt", label = 'Weight Himmelfahrt:',
                                     value=0)),
                column(3, numericInput(inputId="holiday_Pfingsten", label = 'Weight Pfingsten:',
                                       value=0)),
                column(3, numericInput(inputId="holiday_Pfingstmontag", label = 'Weight Pfingstmontag:',
                                       value=0)),
                column(3, numericInput(inputId="holiday_Fronleichnam", label = 'Weight Fronleichnam:',
                                       value=0)),
                column(3, numericInput(inputId="holiday_Tag.der.Deutschen.Einheit", label = 'Weight Tag der Deutschen Einheit:',
                                       value=0)),
                column(3, numericInput(inputId="holiday_Allerheiligen", label = 'Weight Allerheiligen:',
                                       value=0)),
                column(3, numericInput(inputId="holiday_Erster.Weihnachtstag", label = 'Weight Erster Weihnachtstag:',
                                     value=0)),
                column(3, numericInput(inputId="holiday_Zweiter.Weihnachtstag", label = 'Weight Zweiter Weihnachtstag:',
                                      value=0))

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

            fluidRow(
              tags$h4("For how many locations do you need the sales data (additional location name will be EXT_LOC_ID + 1, + 2 etc.)"),
              numericInput(inputId = "n_LOC_ID", label = "Number of Locations", value = 1),
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
         
             updateTextInput(session, "EXT_PROD_ID",  value = if_else(!is.null(df_w$PROD_ID_w), df_w$PROD_ID_w, ""))

             updateNumericInput(session, "wday_Monday",  value = if_else(!is.null(df_w$Monday_w), df_w$Monday_w, NULL), min = 1)
             updateNumericInput(session, "wday_Tuesday",  value = if_else(!is.null(df_w$Tuesday_w), df_w$Tuesday_w, NULL), min = 1)
             updateNumericInput(session, "wday_Wednesday",  value = if_else(!is.null(df_w$Wednesday_w), df_w$Wednesday_w, NULL), min = 1)
             updateNumericInput(session, "wday_Thursday",  value = if_else(!is.null(df_w$Thursday_w), df_w$Thursday_w, NULL), min = 1)
             updateNumericInput(session, "wday_Friday",  value = if_else(!is.null(df_w$Friday_w), df_w$Friday_w, NULL), min = 1)
             updateNumericInput(session, "wday_Saturday",  value = if_else(!is.null(df_w$Saturday_w), df_w$Saturday_w, NULL), min = 1)
             updateNumericInput(session, "wday_Sunday",  value = if_else(!is.null(df_w$Sunday_w), df_w$Sunday_w, NULL), min = 1)

             updateNumericInput(session, "month_January",  value = if_else(!is.null(df_w$January_w), df_w$January_w, NULL), min = 1)
             updateNumericInput(session, "month_February",  value = if_else(!is.null(df_w$February_w), df_w$February_w, NULL), min = 1)
             updateNumericInput(session, "month_March",  value = if_else(!is.null(df_w$March_w), df_w$March_w, NULL), min = 1)
             updateNumericInput(session, "month_April",  value = if_else(!is.null(df_w$April_w), df_w$April_w, NULL), min = 1)
             updateNumericInput(session, "month_May",  value = if_else(!is.null(df_w$May_w), df_w$May_w, NULL), min = 1)
             updateNumericInput(session, "month_June",  value = if_else(!is.null(df_w$June_w), df_w$June_w, NULL), min = 1)
             updateNumericInput(session, "month_July",  value = if_else(!is.null(df_w$July_w), df_w$July_w, NULL), min = 1)
             updateNumericInput(session, "month_August",  value = if_else(!is.null(df_w$August_w), df_w$August_w, NULL), min = 1)
             updateNumericInput(session, "month_September",  value = if_else(!is.null(df_w$September_w), df_w$September_w, NULL), min = 1)
             updateNumericInput(session, "month_October",  value = if_else(!is.null(df_w$October_w), df_w$October_w, NULL), min = 1)
             updateNumericInput(session, "month_November",  value = if_else(!is.null(df_w$November_w), df_w$November_w, NULL), min = 1)
             updateNumericInput(session, "month_December",  value = if_else(!is.null(df_w$December_w), df_w$December_w, NULL), min = 1)

             updateNumericInput(session, "year_2019",  value = if_else(!is.null(df_w$w2019), df_w$w2019, NULL), min = 1)
             updateNumericInput(session, "year_2020",  value = if_else(!is.null(df_w$w2020), df_w$w2020, NULL), min = 1)
             updateNumericInput(session, "year_2021",  value = if_else(!is.null(df_w$w2021), df_w$w2021, NULL), min = 1)
             updateNumericInput(session, "year_2022",  value = if_else(!is.null(df_w$w2022), df_w$w2022, NULL), min = 1)
             updateNumericInput(session, "year_2023",  value = if_else(!is.null(df_w$w2023), df_w$w2023, NULL), min = 1)

             updateNumericInput(session, "holiday_Christi.Himmelfahrt",  value = if_else(!is.null(df_w$christi_w), df_w$christi_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Erster.Mai",  value = if_else(!is.null(df_w$erstermai_w), df_w$erstermai_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Erster.Weihnachtstag",  value = if_else(!is.null(df_w$erster_w), df_w$erster_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Karfreitag",  value = if_else(!is.null(df_w$karfrei_w), df_w$karfrei_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Neujahr",  value = if_else(!is.null(df_w$nj_w), df_w$nj_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Ostermontag",  value = if_else(!is.null(df_w$osterm_w), df_w$osterm_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Pfingstmontag",  value = if_else(!is.null(df_w$pfingstm_w), df_w$pfingstm_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Tag.der.Deutschen.Einheit",  value = if_else(!is.null(df_w$tde_w), df_w$tde_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Zweiter.Weihnachtstag",  value = if_else(!is.null(df_w$zweiter_w), df_w$zweiter_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Ostern",  value = if_else(!is.null(df_w$ostern_w), df_w$ostern_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Fronleichnam",  value = if_else(!is.null(df_w$fron_w), df_w$fron_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Allerheiligen",  value = if_else(!is.null(df_w$aller_w), df_w$aller_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Heilige.Drei.Konige",  value = if_else(!is.null(df_w$hdk_w), df_w$hdk_w, NULL), min = 0)
             updateNumericInput(session, "holiday_Pfingsten",  value = if_else(!is.null(df_w$pfingsten_w), df_w$pfingsten_w, NULL), min = 0)

             updateNumericInput(session, "n_OFR",  value = if_else(!is.null(df_w$n_ofr), df_w$n_ofr, NULL), min = 0)
             })
     
     
     
     # dynamic year input ----

     
     observe({

       
       sales_from <- input$sales_from
       sales_to <- input$sales_to
       
       selected_years <- seq(year(sales_from), year(sales_to))
       # 
       output$year = renderUI({

         input_list <- lapply(selected_years, function(i) {
           # for each dynamically generated input, give a different name
           Year <- paste("i.year", i, sep = "_")
           numericInput(Year, paste("Weight Year", i), value = 1)
         })
         do.call(tagList, input_list)
       })
       
       
       

     })
           
 

     # observe({print(start_year))})
     # 
     
     
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

        holiday_de <- make_holidays_df(c(df_d$year), "DE")
        holiday_ch <- make_holidays_df(c(df_d$year), "CH")
        holiday_at <- make_holidays_df(c(df_d$year), "AT")
        
        
        holiday <- holiday_de %>% 
          full_join(., holiday_at, by = c("ds" = "ds")) %>% 
          full_join(., holiday_ch, by = c("ds" = "ds")) %>% 
          rename("holiday.z" = "holiday") %>% 
          mutate(holiday = case_when(
            is.na(holiday.x) & !is.na(holiday.y) ~ holiday.y, 
            is.na(holiday.x) & is.na(holiday.y) ~ holiday.z,
            TRUE ~ holiday.x
          )) %>% 
          select(ds, holiday) %>% 
          filter(holiday %in% c(
            "Christi Himmelfahrt",
              "Erster Mai",
              "Erster Weihnachtstag",
              "Karfreitag",
              "Neujahr",
              "Ostermontag",
              "Pfingstmontag",
              "Tag der Deutschen Einheit",
              "Zweiter Weihnachtstag",
              "Fronleichnam", 
              "Ostern",
              "Allerheiligen",
              "Heilige Drei Konige",
              "Pfingsten"
          ))

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
            
            df1 <- as_tibble(df1)  %>%
              mutate(across(where(is.integer), as.double))

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
              week_days_null("Monday") %>%
              week_days_null("Tuesday") %>%  
              week_days_null("Wednesday") %>%  
              week_days_null("Thursday") %>%  
              week_days_null("Friday") %>%  
              week_days_null("Saturday") %>%  
              week_days_null("Sunday") %>%  
              holidays_null("Christi Himmelfahrt") %>% 
              holidays_null("Erster Mai") %>% 
              holidays_null("Erster Weihnachtstag") %>% 
              holidays_null("Karfreitag") %>% 
              holidays_null("Neujahr") %>% 
              holidays_null("Ostermontag") %>% 
              holidays_null("Pfingstmontag") %>% 
              holidays_null("Tag der Deutschen Einheit") %>% 
              holidays_null("Zweiter Weihnachtstag") %>% 
              holidays_null("Fronleichnam")  %>% 
              holidays_null("Ostern") %>% 
              holidays_null("Allerheiligen") %>% 
              holidays_null("Heilige Drei Konige") %>% 
              holidays_null("Pfingsten") 
       
         
            
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
    
    # observe({print(df_full())})
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
    
    
    # 
    # observe({print(df_OFR())})
    # 



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
                 GROSS_SALES_AMNT = round(ss_sales * input$PRICE, 2),
                 NET_SALES_AMNT = round(ss_sales * input$PRICE, 2),
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
                 GRANULARITY = "D",
                 TIMESTAMP = format(obs_days, "%Y%m%d%H%M%S"),
                 PRC_TCD = "",
                 RETURNED_SALES = "",
                 UNIT_SALES = sales,
                 ZPROMO_FLAG = "",
                 FUNCTION = "I",
                 GROSS_SALES_AMNT = round(sales * OFR_price, 2),
                 NET_SALES_AMNT = round(sales * OFR_price, 2),
                 CURRENCY_ISO = input$LOC_CURRENCY,
                 UOM_ISO = input$SALES_UOM
          ) %>%
          select(c(EXT_LOC_ID, EXT_PROD_ID, LOC_TCD, GRANULARITY, OFR_ID, TIMESTAMP,
                   PRC_TCD, RETURNED_SALES, UNIT_SALES, SALES_UOM, LOC_CURRENCY, ZPROMO_FLAG, GROSS_SALES_AMNT, NET_SALES_AMNT,
                   CURRENCY_ISO, UOM_ISO, FUNCTION))

      df <-   rbind(df_s, df_os)

      if (input$n_LOC_ID > 1) {
        
          LOC_ID <- as.numeric(input$EXT_LOC_ID)
            
            generate_locations <- function(start, end) {
              sprintf("%04d", seq(start, end))
            }
            
            # Number of locations needed
            num_locations <- input$n_LOC_ID
            
            # Generate vector of locations
            locations <- generate_locations(LOC_ID + 1, LOC_ID + num_locations)
            
            # Duplicate and update location for each location
            duplicated_dfs <- purrr::map(locations, ~ mutate(df, EXT_LOC_ID = .x))
            
            # Merge all duplicated dataframes into one dataframe
            merged_df <- dplyr::bind_rows(duplicated_dfs)
            

            df <- rbind(df, merged_df)
          }
       else {
          # Return the original dataframe when n_LOC_ID is not greater than 1
          df
       }
      })
        


      # observe({print(df_table())})
     #
     output$downloadData <- downloadHandler(
         filename = function() {
           ifelse(input$n_LOC_ID > 1, paste(input$EXT_PROD_ID, df_table()$EXT_LOC_ID[1], "to", tail(df_table()$EXT_LOC_ID), ".csv", sep = "_"), paste(input$EXT_PROD_ID, df_table()$EXT_LOC_ID, ".csv", sep = "_"))
         },
         content = function(file) {
             write_delim(df_table(), file, delim = ";")
         })


    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
