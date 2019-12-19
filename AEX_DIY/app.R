
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


########## DATA SCRAPING & WRANGLING ############

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





############# UI #################


ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(),
    dashboardBody()
)






############ SERVER ##############


server <- function (input, output){
    
}

shinyApp(ui, server)
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                  
                