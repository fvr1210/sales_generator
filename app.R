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
library(DT)
library(shinyjs)

# source


eval(parse("scripts/create_holiday.R", encoding="UTF-8"))

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
                dateInput("sales_from", "Start date:", value = Sys.Date()-719, format = "dd/mm/yyyy"),
                dateInput("sales_to", "End date:", value  = Sys.Date(), format = "dd/mm/yyyy")
                ),             
            
                                
                                 
            fluidRow(  
                h3("Master Data"), 
                #  EXT_PROD_ID
                column(3, textInput(inputId = "EXT_PROD_ID", label = "EXT_PROD_ID", value = "", width = NULL, placeholder = NULL)),
                    verbatimTextOutput("EXT_PROD_ID"),
                #  EXT_LOC_ID
                column(3, textInput(inputId = "EXT_LOC_ID", label = "EXT_LOC_ID", value = "", width = NULL, placeholder = NULL)),
                verbatimTextOutput("EXT_LOC_ID"),
                #  LOC_TCD
                column(3,textInput(inputId = "LOC_TCD", label = "LOC_TCD", value = "1040", width = NULL, placeholder = NULL)),
                verbatimTextOutput("LOC_TCD"),
                #  SALES_UOM
                column(2, textInput(inputId = "SALES_UOM", label = "SALES_UOM", value = "EA", width = NULL, placeholder = NULL)),
                verbatimTextOutput("SALES_UOM"),
                #  SALES_UOM
                column(2, textInput(inputId = "LOC_CURRENCY", label = "LOC_CURRENCY", value = "EUR", width = NULL, placeholder = NULL)),
                verbatimTextOutput("LOC_CURRENCY")),
                
           
                
            fluidRow(    
            h3("Variablen weight"),
                tags$h4("Week Day"),
                
                    column(3, numericInput(inputId="wday_Montag", label = 'Weight Monday:',
                                          min=0, value=1)),
                    column(3, numericInput(inputId="wday_Dienstag", label = 'Weight Tuesday:',
                                          min=0,  value=1)),
                    column(3, numericInput(inputId="wday_Mittwoch", label = 'Weight Wednesday:',
                                          min=0, value=1)),
                    column(3, numericInput(inputId="wday_Donnerstag", label = 'Weight Thursday:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="wday_Freitag", label = 'Weight Friday:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="wday_Samstag", label = 'Weight Saturday:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="wday_Sonntag", label = 'Weight Sunday:',
                                   min=0, value=1))),
            
            fluidRow( 
                tags$h4("Month"),
            
                    column(3, numericInput(inputId="month_Januar", label = 'Weight January:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="month_Februar", label = 'Weight February:',
                                   min=0,  value=1)),
                    column(3, numericInput(inputId="month_März", label = 'Weight March:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="month_April", label = 'Weight April:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="month_Mai", label = 'Weight Mai:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="month_Juni", label = 'Weight June:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="month_Juli", label = 'Weight July:',
                                   min=0, value=1)),
                    column(3, numericInput(inputId="month_August", label = 'Weight August:',
                                    min=0, value=1)),
                    column(3, numericInput(inputId="month_September", label = 'Weight September:',
                                    min=0,  value=1)),
                    column(3, numericInput(inputId="month_Oktober", label = 'Weight October:',
                                    min=0, value=1)),
                    column(3, numericInput(inputId="month_November", label = 'Weight November:',
                                    min=0, value=1)),
                    column(3, numericInput(inputId="month_Dezember", label = 'Weight Dezember:',
                                     min=0, value=1))
                ),
            
            fluidRow(    
                tags$h4("Year"),
                
                column(3, numericInput(inputId="year_X2020", label = 'Weight 2020:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="year_X2021", label = 'Weight 2021:',
                                       min=0,  value=1)),
                column(3, numericInput(inputId="year_X2022", label = 'Weight 2022:',
                                       min=0, value=1))),
            
            fluidRow( 
                tags$h4("Holiday"),
                
                column(3, numericInput(inputId="holiday_Christi.Himmelfahrt", label = 'Weight Himmelfahrt:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="holiday_Erster.Mai", label = 'Weight Erster Mai:',
                                       min=0,  value=1)),
                column(3, numericInput(inputId="holiday_Erster.Weihnachtstag", label = 'Weight Weihnachtstag:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="holiday_Karfreitag", label = 'Weight Karfreitag:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="holiday_Neujahr", label = 'Weight Neujahr:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="holiday_Ostermontag", label = 'Weight Ostermontag:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="holiday_Pfingstmontag", label = 'Weight Pfingstmontag:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="holiday_Tag.der.Deutschen.Einheit", label = 'Weight Tag der Deutschen Einheit:',
                                       min=0, value=1)),
                column(3, numericInput(inputId="holiday_Zweiter.Weihnachtstag", label = 'Weight Weihnachtstag:',
                                       min=0,  value=1))
            ), 
            # 
            # fluidRow(
            #     tags$h4("DIF"),
            #     actionButton('insertBtn', 'Insert DIF'),
            #     actionButton('removeBtn', 'Remove DIF'),
            #     tags$div(id = 'DIF')
            # ),
            # 
            
            fluidRow(
                tags$h4("Sales Data"),

                column(3, numericInput(inputId="lambda", label = 'Expected average sales value',
                                       min = 1, value=5))),
            
           
            actionButton("generate", "Generate Sales data", style="color: #fff; background-color: #d73925; border-color: #c34113;
                                border-radius: 10px; 
                             border-width: 2px"),
            # fluidRow(    
            #     tags$h4("DIF"),
            #     
            #     column(3, numericInput(inputId="DIF_1", label = 'First DIF:',
            #                            min=0, value=1)),
            #     column(3, numericInput(inputId="DIF_2", label = 'Second DIF',
            #                            min=0,  value=1)),
            #     column(3, numericInput(inputId="DIF_3", label = 'Third DIF:',
            #                            min=0, value=1)))

            fluidRow(
                # br(),
                # dataTableOutput("wei_stand_dis_table"),
                br(),
                # dataTableOutput("sales_table"),
                plotOutput("sales_plot")
            ),

            # Button
            downloadButton("downloadData", "Download")
            
                            
                
                
                    )))


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    

    
    # Date
    # dates <- reactive({c(input$sales_to, input$sales_from)})
    
    # df1 <- reactive({
    # obs_days <- as_date(ymd(dates()[2]):ymd(dates()[1]))
    # # 
    # df1 <- data.frame(obs_days)  %>% 
    #     mutate(wday = weekdays(obs_days)) %>% 
    #     mutate(month = months(obs_days)) %>% 
    #     mutate(year = year(obs_days)) 
    # 
    # holiday <- make_holidays_df(c(df1$year), "DE")
    # 
    # df1 <- left_join(df1, holiday, by = c("obs_days" = "ds"))
    # 
    # 
    # df1 <- df1 %>%  
    #     mutate(year = as.factor(year)) %>% 
    #     recipe( ~ .) %>% 
    #     step_dummy(wday, month, year, holiday, one_hot = T ) %>% 
    #     prep() %>% 
    #     bake(df1) %>% 
    #     replace(is.na(.), 0) %>% 
    #     select(where(~ any(. != 0)))
    # 
    # 
    # }
    # )
    
    # observe(print(df1()))
    

    
    # 
    observeEvent(input$insertBtn, {
        insertUI(
            selector = '#DIF',
            ui = dateInput("DIF_from", "DIF Start date:", value = Sys.Date()-720, format = "dd/mm/yyyy"),
                 dateInput("DIF_to", "DIF End date:", value  = Sys.Date(), format = "dd/mm/yyyy"),
            # ui_3 = numericInput(inputId="DIF_effect", label = 'DIF Effect',
            #              min=0, value=1)
             
        )
    })
    
    # observeEvent(input$removeBtn, {
    #     removeUI(
    #         selector = 'div:has(> #sld1)'
    #     )
    # })
    
    

    

    # Weights
    
    
    df_full <- eventReactive(input$generate,{
        

        dates <- c(input$sales_to, input$sales_from)
        
        
        obs_days <- as_date(ymd(dates[2]):ymd(dates[1]))
        # 
        df_d <- data.frame(obs_days)  %>% 
            mutate(wday = weekdays(obs_days)) %>% 
            mutate(month = months(obs_days)) %>% 
            mutate(year = year(obs_days)) 
        
        holiday <- make_holidays_df(c(df_d$year), "DE")
        
        df_dh <- left_join(df_d, holiday, by = c("obs_days" = "ds"))
        
        
        df1 <- df_dh %>%  
            mutate(year = as.factor(year)) %>% 
            recipe( ~ .) %>% 
            step_dummy(wday, month, year, holiday, one_hot = T ) %>% 
            prep() %>% 
            bake(df_dh) %>% 
            replace(is.na(.), 0) %>% 
            select(where(~ any(. != 0)))
        


    input_df <- NULL
    df_row <- NULL

            for(i in 1:length(names(input))){
                df_row <- as.data.frame(cbind(names(input)[i], input[[names(input)[i]]]))
                input_df <- as.data.frame(dplyr::bind_rows(input_df, df_row))
            }
            names(input_df) <- c("input_id", "input_val")

        weight_inputs <- input_df %>%
            filter(grepl('wday_', names(input)) | grepl('holiday_', names(input)) | grepl('year_', names(input)) | grepl('month_', names(input))) %>%
            pivot_wider(names_from=c(input_id), values_from=input_val) %>%
            slice(rep(1, each = nrow(df1)))

         df2 <- df1 %>% select(obs_days) %>%
             cbind(.,weight_inputs)

         df2 <- as.tibble(df2)  %>%
             mutate(across(where(is.character), as.double))



         df1_dt <- as.data.table(df1)

         df2_dt <- as.data.table(df2)

         nm1 <- names(df1)[-1]

         setDT(df1_dt)[df2_dt,  (nm1) := Map(`*`, mget(nm1),
                                             mget(paste0("i.", nm1))) , on = "obs_days"]



         log_data <- log(df1_dt[,2:ncol(df1_dt)])
         #
         
         
         
         
         
         df3 <- df_dh %>%
             cbind(log_data) %>% 
             as_tibble() %>% 
             mutate(year = as.factor(year)) %>% 
             mutate_if(is.numeric, ~ replace(., is.infinite(.), NA)) %>% 
             mutate(sum_log = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>% 
             mutate(sum = exp(sum_log)) %>% 
             mutate(sum = case_when(
                 is.na(wday_Montag) & wday == "Montag" ~ 0,
                 is.na(wday_Dienstag) & wday == "Dienstag" ~ 0,
                 is.na(wday_Mittwoch) & wday == "Mittwoch" ~ 0,
                 is.na(wday_Donnerstag) & wday == "Donnerstag" ~ 0,
                 is.na(wday_Freitag) & wday == "Freitag" ~ 0,
                 is.na(wday_Samstag) & wday == "Samstag" ~ 0,
                 is.na(wday_Sonntag) & wday == "Sonntag" ~ 0,
                 is.na(holiday_Christi.Himmelfahrt) & holiday == "Christi Himmelfahrt" ~ 0,
                 is.na(holiday_Erster.Mai) & holiday == "Erster Mai" ~ 0,
                 is.na(holiday_Erster.Weihnachtstag) & holiday == "Erster Weihnachtstag" ~ 0,
                 is.na(holiday_Karfreitag) & holiday == "Karfreitag" ~ 0,
                 is.na(holiday_Neujahr) & holiday == "Neujahr" ~ 0,
                 is.na(holiday_Ostermontag) & holiday == "Ostermontag" ~ 0,
                 is.na(holiday_Pfingstmontag) & holiday == "Pfingstmontag" ~ 0,
                 is.na(holiday_Tag.der.Deutschen.Einheit) & holiday == "Tag der Deutschen Einheit" ~ 0,
                 is.na(holiday_Zweiter.Weihnachtstag) & holiday == "Zweiter Weihnachtstag" ~ 0,
                 TRUE ~  sum))

         lambda <- input$lambda

         df4 <- df3 %>%
             select(obs_days, sum) %>%
             mutate(sales = rpois(nrow(df3), lambda = lambda)) %>%
             mutate(ss_sales = sales*sum) %>% 
             as_tibble


         # df5 <- df4 %>%
         #     mutate(EXT_PROD_ID = input$EXT_PROD_ID,
         #            EXT_LOC_ID = input$EXT_LOC_ID,
         #            LOC_TCD = input$LOC_TCD,
         #            SALES_UOM = input$SALES_UOM,
         #            LOC_CURRENCY = input$LOC_CURRENCY,
         #            GRANULARITY = "1",
         #            OFR_ID = "",
         #            TIMESTAMP = format(obs_days, "%Y%m%d%H%M%S"),
         #            PRC_TCD = "",
         #            RETURNED_SALES = "",
         #            UNIT_SALES = ss_sales,
         #            ZPROMO_FLAG = "",
         #            FUNCTION = "",
         #            GROSS_SALES_AMNT = ss_sales * 5,
         #            NET_SALES_AMNT = ss_sales * 5,
         #            CURRENCY_ISO = input$LOC_CURRENCY,
         #            UOM_ISO = input$SALES_UOM
         #     ) %>%
         #     select(c(EXT_LOC_ID, EXT_PROD_ID, LOC_TCD, GRANULARITY, OFR_ID, TIMESTAMP,
         #              PRC_TCD, RETURNED_SALES, UNIT_SALES, SALES_UOM, LOC_CURRENCY, ZPROMO_FLAG, GROSS_SALES_AMNT, NET_SALES_AMNT,
         #              CURRENCY_ISO, UOM_ISO, FUNCTION))



        
    })
    
# 
#     df_full <- reactive({
#         df5 <- as_tibble(df5())
#     })

    
    # 
    # df3 <- eventReactive(input$generate, {
    #     
    #     df1_dt <- as.data.table(df1())
    #     
    #     df2_dt <- as.data.table(df2())
    #     
    #     nm1 <- names(df1())[-1]
    #     
    #     setDT(df1_dt)[df2_dt,  (nm1) := Map(`*`, mget(nm1),
    #                                            mget(paste0("i.", nm1))) , on = "obs_days"]
    # 
    # 
    # 
    #     log_data <- log(df1_dt[,2:ncol(df1_dt)]) 
    #     # 
    #     log_data_tib <- df1() %>%
    #         select(obs_days) %>%
    #         cbind(log_data) %>%
    #         mutate_if(is.numeric, ~ replace(., is.infinite(.), 0)) %>%
    #         mutate(sum_log = rowSums(across(where(is.numeric)))) %>%
    #         mutate(sum = exp(sum_log))
    # })
    
    observe(print(df_full()))

    # 

    output$sales_plot <- renderPlot(df_full() %>% ggplot() +
                                        geom_line(aes(obs_days, ss_sales)) +
                                        theme_bw())
    
    
    

     df_table <- reactive({
         df_full() %>%
         mutate(EXT_PROD_ID = input$EXT_PROD_ID,
                EXT_LOC_ID = input$EXT_LOC_ID,
                LOC_TCD = input$LOC_TCD,
                SALES_UOM = input$SALES_UOM,
                LOC_CURRENCY = input$LOC_CURRENCY,
                GRANULARITY = "1",
                OFR_ID = "",
                TIMESTAMP = format(obs_days, "%Y%m%d%H%M%S"),
                PRC_TCD = "",
                RETURNED_SALES = "",
                UNIT_SALES = ss_sales,
                ZPROMO_FLAG = "",
                FUNCTION = "",
                GROSS_SALES_AMNT = ss_sales * 5,
                NET_SALES_AMNT = ss_sales * 5,
                CURRENCY_ISO = input$LOC_CURRENCY,
                UOM_ISO = input$SALES_UOM
         ) %>%
         select(c(EXT_LOC_ID, EXT_PROD_ID, LOC_TCD, GRANULARITY, OFR_ID, TIMESTAMP,
                  PRC_TCD, RETURNED_SALES, UNIT_SALES, SALES_UOM, LOC_CURRENCY, ZPROMO_FLAG, GROSS_SALES_AMNT, NET_SALES_AMNT,
                  CURRENCY_ISO, UOM_ISO, FUNCTION))
    
     })



     output$downloadData <- downloadHandler(
         filename = function() {
             paste(input$EXT_PROD_ID, input$EXT_LOC_ID, ".csv", sep = "_")
         },
         content = function(file) {
             write_csv2(df_table(), file)
         })


    
    
}



# Run the application 
shinyApp(ui = ui, server = server)
