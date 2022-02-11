#install.packages("tidyverse")
#install.packages("shiny")
#install.packages("DT")
#install.packages("dplyr")
#install.packages("readr")
library(dplyr)
library(tidyverse)
library(shiny)
library(DT)
library(readr)

stock_meta <- read_csv("datasets/symbols_valid_meta.csv")

files <- list.files(path = "datasets/stocks", pattern = "*.csv", full.names = T)
a <- read_csv(files[11])

filter(stock_meta, ETF == "Y" & Round_Lot_Size >= 100, Category == "G")

ui <- fluidPage(
  textInput(inputId = "stock_name",
            label = "Stock Name/Symbol:"),
  DTOutput("tbl"),
  plotOutput("hist")
)

table_options <- function() {
  list(searching = FALSE)}

server <- function(input, output) {
  output$tbl <- renderDT(
    options = list(searching = FALSE), 
    if(toupper(input$stock_name == "")) {
      stocks
    } else {
      stock_meta %>% filter(grepl(input$stock_name, Symbol, ignore.case = TRUE) | grepl(input$stock_name, Name, ignore.case = TRUE))
    }
  )
  
  output$hist <- renderPlot(
    hist(a$Volume, 
         main="Volume of Trades",
         col="green")
  )
}

shinyApp(ui = ui, server = server)


