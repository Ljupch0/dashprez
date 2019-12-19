
library(shiny)
library(shinydashboard)
library(tidyverse)
library(rvest)
library(treemap)
library(d3treeR)
library(quantmod)
library(dygraphs)
library(DT)
library(tools) #try to remove with own function

date_converter <- function(date) {
    months <- c("January","February","March","April","May","June","July","August","September", "October","November","December")
    converted_date <- paste(months[as.numeric(substring(date, 6, 7))],paste0(substring(date, 9, 10),","), substring(date,1,4))
    return(converted_date)
}


style_widget <- function(hw=NULL, style="", addl_selector="") {
    stopifnot(!is.null(hw), inherits(hw, "htmlwidget"))
    
    # use current id of htmlwidget if already specified
    elementId <- hw$elementId
    if(is.null(elementId)) {
        # borrow htmlwidgets unique id creator
        elementId <- sprintf(
            'htmlwidget-%s',
            htmlwidgets:::createWidgetId()
        )
        hw$elementId <- elementId
    }
    
    htmlwidgets::prependContent(
        hw,
        htmltools::tags$style(
            sprintf(
                "#%s %s {%s}",
                elementId,
                addl_selector,
                style
            )
        )
    )
}


theme_ljupcho <- function() {
    theme_minimal() %+replace%
        theme(
            panel.grid.major.x =element_blank(),
            panel.grid.minor.x =element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.x=element_blank(),
            legend.background = element_blank(),
            legend.key        = element_blank(),
        )
}

#Prices df------------------------------------------
aex <- read_html("https://finance.yahoo.com/quote/%5EAEX/components/")

aex <- aex %>% 
    html_nodes("table") %>% 
    html_table()

aex <- aex[[1]]

daily_AEX2 <- read_html("https://finance.yahoo.com/quote/%5EAEX?p=%5EAEX")

daily_AEX2 <- daily_AEX2 %>% 
    html_nodes("table") %>% 
    html_table()

daily_range <- strsplit(daily_AEX2[[2]][1,2], split=' - ', fixed=TRUE)


names(aex) <- tolower(names(aex))
names(aex) <- sub(" ","_", names(aex))
aex$symbol <- sub(".AS", "", aex$symbol)

names(aex)[5] <- "percent_change"

aex$percent_change <- parse_number(aex$percent_change)
aex$volume <- parse_number(aex$volume)

#Company info df------------------------------------
company_info <- read_html("https://en.wikipedia.org/wiki/AEX_index")

company_info <- company_info  %>% 
    html_nodes("table") %>% 
    html_table(fill=TRUE)

company_info <- company_info[[2]]

names(company_info) <- tolower(names(company_info))
names(company_info) <- sub(" ","_", names(company_info))
company_info <- rename(company_info, index_weighting = `index_weighting (%)`, symbol = ticker_symbol)
company_info$icb_sector<- toTitleCase(company_info$icb_sector)
company_info <- company_info %>% 
    select(-company)

aex <- left_join(aex, company_info, by="symbol")
rm(company_info)

aex <- aex %>% 
    mutate(
        symbol_percent = paste0(symbol," ", percent_change,"%"),
        names_size=ifelse(index_weighting<2.5,"Smaller Cap",symbol_percent))

#AEX time series-------------------------------------
invisible(getSymbols('^AEX'))

daily_AEX <- read_html("https://tradingeconomics.com/netherlands/stock-market") %>% 
    html_nodes("table") %>% 
    html_table(fill=TRUE)

aex_current <- daily_AEX[[4]][1,2]
aex_previous <- daily_AEX[[4]][1,3]
aex_change_day <- daily_AEX[[2]][1,6]
aex_change_year <- daily_AEX[[2]][1,7]
aex_min <- daily_range[[1]][1]
aex_max <- daily_range[[1]][2]


aex_dt <- aex %>% 
    select(symbol,company_name,last_price,percent_change,index_weighting, icb_sector)

tree1<- treemap(
    dtf= aex,
    index =c("names_size","symbol_percent"),
    vSize="index_weighting",
    vColor="percent_change",
    type="value",
    fun.aggregate="weighted.mean",
    palette =c("#590000","#b50a00","white","#00ba0c", "#035900"),
    mapping=c(-5, 0, 5),
    #position.legend = "none"
)

#for ranking plot coloring:
aex <- aex %>% 
    mutate(daily_gain_loss = ifelse(percent_change>0, "gain", "loss"))

#############_END_DATA_PROCESSSING_#################

ui <- dashboardPage(
    dashboardHeader(title = "AEX Daily"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
        fluidRow(
            uiOutput("aex_current"),
            uiOutput("aex_previous_close"),
            uiOutput("aex_change_day"),
            uiOutput("aex_change_year"),
            uiOutput("aex_min"),
            uiOutput("aex_max")
        ),
        fluidRow(
            column(6, d3tree3Output("treemap", height = "620px")),
            
            
            tabBox(height = "600px",
                   tabPanel("Top Daily Gainers", DTOutput("dt_gainers"),
                            style = "height:530px; overflow-y: scroll;"),
                   tabPanel("Top Daily Losers", DTOutput("dt_losers"),
                            style = "height:530px; overflow-y: scroll;"),
                   tabPanel("Ranking Plot", plotOutput("ranking_plot")),
                   tabPanel("Historical Index Data", dygraphOutput("historical", height = "535px"))
            )
        )
    )
)


##############################################


server <- function(input, output) {
    
    output$aex_current <- renderUI(
        valueBox(
            aex_current, "Current Index", width=2, color="light-blue", icon=icon("chart-bar")
        )
    )
    
    output$aex_previous_close <- renderUI(
        valueBox(
            aex_previous, "Previous Close", width=2, color="light-blue", icon=icon("angle-left"))
    )
    
    output$aex_change_day <- renderUI(
        valueBox(
            aex_change_day, "Change from Prev. Close", width=2,
            color = if (parse_number(aex_change_day)>0) {
                "green"
            } else {
                "red"
            },
            icon = if (parse_number(aex_change_day)>0) {
                icon("arrow-up")
            } else {
                icon("arrow-down")
            } 
        )
    )
    
    output$aex_change_year <- renderUI(
        valueBox(
            aex_change_year,"YTD Change", width=2,
            color = if (parse_number(aex_change_year)>0) {
                "green"
            } else {
                "red"
            },
            icon = if (parse_number(aex_change_year)>0) {
                icon("arrow-up")
            } else {
                icon("arrow-down")
            }
        )
    )
    
    output$aex_min <- renderUI(
        valueBox(
            aex_min,"Daily Minimum", width=2, color = "light-blue", icon=icon("caret-down"))
    )
    
    output$aex_max <- renderUI(
        valueBox(
            aex_max,"Daily Maximum", width=2, color="light-blue", icon=icon("caret-up"))
    )
    
    output$treemap <- renderD3tree3(
        style_widget(
            d3tree3(tree1, rootname ="AEX"),
            addl_selector="text",
            style="font-family:Ubuntu; font-size:15px; width"
        )
    )
    
    output$dt_gainers <- renderDT(
        datatable(aex_dt,
                  extensions = 'Buttons',
                  options=list(order = list(list(3, 'desc')),
                               paging = FALSE,
                               dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               pageLength = 7,
                               info = FALSE),
                  colnames = c("Ticker", "Company", "Last Price", "Percent Change","Index Weight","ICB Sector"),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;',
                      'Data source: finance.yahoo.com'))
    )
    
    
    output$dt_losers <- renderDT(
        datatable(aex_dt,
                  extensions = 'Buttons',
                  options=list(order = list(list(3, 'asc')),
                               paging = FALSE,
                               dom = 'Bfrtip',
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               info = FALSE),
                  colnames = c("Ticker", "Company", "Last Price", "Percent Change","Index Weight", "ICB Sector"),
                  rownames = FALSE,
                  caption = htmltools::tags$caption(
                      style = 'caption-side: bottom; text-align: center;',
                      'Data source: finance.yahoo.com'))
    )
    
    output$ranking_plot <- renderPlot({
        ggplot(aex, aes(reorder(symbol,percent_change, FUN = "identity"), percent_change))+
            geom_bar(stat="identity", aes(fill=daily_gain_loss), show.legend = FALSE)+
            geom_text(aes(label=paste0(percent_change,"%"),
                          hjust = ifelse(aex$percent_change>0,-0.1,1.1))
            )+
            scale_fill_manual(values = c("#00ba38","#f8766d")) + 
            scale_y_continuous(limits = c(min(aex$percent_change)-0.25, max(aex$percent_change)+0.25))+
            coord_flip()+
            theme_ljupcho()}, height = 540
    )
    
    output$historical <- renderDygraph({
        dygraph(AEX, main="AEX") %>%
            dyRangeSelector() %>%
            dyCandlestick() %>% 
            dySeries("AEX.Close", axis = 'y2') %>% 
            dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.3)
    })
    
}

shinyApp(ui, server)







































