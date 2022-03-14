library(dplyr)
library(tidyverse)
library(shiny)
library(DT)
library(readr)
library(zoom)
library(scales)
library(plotrix)
library(markdown)
library(plotly)
library(dash)


#Installing the package
#install.packages("h2o")
#loading the library 
library(h2o)

#FILE READING AND TIDYING
###############################################################################################
#Reading all the FAANG csv files and tidying it up to combine them

amazon <- read_csv("https://query1.finance.yahoo.com/v7/finance/download/AMZN?period1=1615700083&period2=1647232483&interval=1d&events=history&includeAdjustedClose=true")
amazon <- mutate(amazon, Company="Amazon")
amazon$Date <- as.Date(amazon$Date,format ='%m/%d/%Y' )
amazon <- amazon %>% select(-one_of("Adj Close"))
apple <- read_csv("https://query1.finance.yahoo.com/v7/finance/download/AAPL?period1=1615698833&period2=1647231233&interval=1d&events=history&includeAdjustedClose=true")
apple <- mutate(apple, Company="Apple")
apple$Date <- as.Date(apple$Date,format ='%m/%d/%Y' )
apple <- apple %>% select(-one_of("Adj Close"))
facebook <- read_csv("https://query1.finance.yahoo.com/v7/finance/download/FB?period1=1615698873&period2=1647231273&interval=1d&events=history&includeAdjustedClose=true")
facebook <- mutate(facebook, Company="Facebook")
facebook$Date <- as.Date(facebook$Date,format ='%m/%d/%Y' )
facebook <- facebook %>% select(-one_of("Adj Close"))
google <- read_csv("https://query1.finance.yahoo.com/v7/finance/download/GOOG?period1=1615698854&period2=1647231254&interval=1d&events=history&includeAdjustedClose=true")
google <- mutate(google, Company="Google")
google$Date <- as.Date(df$Date,format ='%m/%d/%Y' )
google <- google %>% select(-one_of("Adj Close"))
netflix <- read_csv("https://query1.finance.yahoo.com/v7/finance/download/NFLX?period1=1615698792&period2=1647231192&interval=1d&events=history&includeAdjustedClose=true")
netflix <- mutate(netflix, Company="Netflix")
netflix$Date <- as.Date(netflix$Date,format ='%m/%d/%Y' )
netflix <- netflix %>% select(-one_of("Adj Close"))

#Combining all the FAANG companies into one dataframe
FAANG <- rbind(amazon, apple, facebook, google, netflix)

#Removing Open, High, Low, and Adj Close column
FAANG <- FAANG %>% select(-one_of("Adj Close"))
FAANG$Date <- as.Date(FAANG$Date,format ='%m/%d/%Y' )

#Rearranging the dataframe so that Company comes first
FAANG <- select(FAANG, Company, Date, Open, High , Low, Close, Volume)
###############################################################################################



# #AMAZON
# ##############################################################################################
# 
# #shifting n rows up of a given variable
# shift <- function(x, n) {
#   c(x[-(seq(n))], rep(NA, n))
# }
# 
# amazon$shifted <- shift(amazon$Close, 1)
# tail(amazon)
# 
# #remove NA observations
# amazon <- na.omit(amazon)
# write.csv(amazon, "amazon.csv")
# 
# #Initializing the Virtual Machine using all the threads (-1) and 16gb of memory
# h2o.init(nthreads = -1, max_mem_size = "16g")
# 
# amazon <- h2o.importFile("amazon.csv")
# h2o.describe(amazon)
# 
# y <- "shifted" #variable we want to forecast
# x <- setdiff(names(amazon), y)
# 
# set.seed(8)
# parts <- h2o.splitFrame(amazon, .80)
# train <- parts[[1]]
# test <- parts[[2]]
# 
# #Train the Model
# automodel <- h2o.automl(x, y, train, test, max_runtime_secs = 60, seed=8)
# 
# #Obtained a list of models in order of performance. To learn more about them just call
# automodel@leader
# 
# #Apply the Model
# predictions <- h2o.predict(automodel@leader, amazon)
# 
# #Make it into a data frame I can use
# stock_predictions <- as.data.frame(predictions)
# 
# amazon <- read_csv("amazon.csv")
# amazon$Date <- as.Date(amazon$Date,format ='%m/%d/%Y' )
# amazon <- mutate(amazon, Company="Amazon")
# amazon <- select(amazon, Date, Close, Company)
# 
# #Tidy data
# new_row <- c(tail(predictions, n=1))
# stock_predictions <- mutate(stock_predictions, Date=amazon$Date)
# stock_predictions <- mutate(stock_predictions, Company="Amazon Prediction")
# stock_predictions <- rename(stock_predictions, Close=predict)
# stock_predictions <- select(stock_predictions, Date, Close, Company)
# stock_predictions <- arrange(stock_predictions, Date, Close, Company)
# 
# #Write as a csv file
# write.csv(stock_predictions, "stock_predictionsAmazon.csv")
# 
# testPredictAmazon <- rbind(amazon, stock_predictions)
# 
# ################################################################################################
# 
# #APPLE
# ###############################################################################################
# 
# #shifting n rows up of a given variable
# shift <- function(x, n) {
#   c(x[-(seq(n))], rep(NA, n))
# }
# 
# apple$shifted <- shift(apple$Close, 1)
# tail(apple)
# 
# #remove NA observations
# apple <- na.omit(apple)
# write.csv(apple, "apple.csv")
# 
# #Initializing the Virtual Machine using all the threads (-1) and 16gb of memory
# h2o.init(nthreads = -1, max_mem_size = "16g")
# 
# apple <- h2o.importFile("apple.csv")
# h2o.describe(apple)
# 
# y <- "shifted" #variable we want to forecast
# x <- setdiff(names(apple), y)
# 
# set.seed(8)
# parts <- h2o.splitFrame(apple, .80)
# train <- parts[[1]]
# test <- parts[[2]]
# 
# #Train the Model
# automodel <- h2o.automl(x, y, train, test, max_runtime_secs = 60, seed=8)
# 
# #Obtained a list of models in order of performance. To learn more about them just call
# automodel@leader
# 
# #Apply the Model
# predictions <- h2o.predict(automodel@leader, apple)
# 
# #Make it into a data frame I can use
# stock_predictions <- as.data.frame(predictions)
# 
# apple <- read_csv("apple.csv")
# apple$Date <- as.Date(apple$Date,format ='%m/%d/%Y' )
# apple <- mutate(apple, Company="Apple")
# apple <- select(apple, Date, Close, Company)
# 
# #Tidy data
# new_row <- c(tail(predictions, n=1))
# stock_predictions <- mutate(stock_predictions, Date=apple$Date)
# stock_predictions <- mutate(stock_predictions, Company="Apple Prediction")
# stock_predictions <- rename(stock_predictions, Close=predict)
# stock_predictions <- select(stock_predictions, Date, Close, Company)
# stock_predictions <- arrange(stock_predictions, Date, Close, Company)
# 
# #Write as a csv file
# write.csv(stock_predictions, "stock_predictionsApple.csv")
# 
# testPredictApple <- rbind(apple, stock_predictions)
# 
# ################################################################################################
# 
# #FACEBOOK
# ###############################################################################################
# 
# #shifting n rows up of a given variable
# shift <- function(x, n) {
#   c(x[-(seq(n))], rep(NA, n))
# }
# 
# facebook$shifted <- shift(facebook$Close, 1)
# tail(facebook)
# 
# #remove NA observations
# facebook <- na.omit(facebook)
# write.csv(facebook, "facebook.csv")
# 
# #Initializing the Virtual Machine using all the threads (-1) and 16gb of memory
# h2o.init(nthreads = -1, max_mem_size = "16g")
# 
# facebook <- h2o.importFile("facebook.csv")
# h2o.describe(facebook)
# 
# y <- "shifted" #variable we want to forecast
# x <- setdiff(names(facebook), y)
# 
# set.seed(8)
# parts <- h2o.splitFrame(facebook, .80)
# train <- parts[[1]]
# test <- parts[[2]]
# 
# #Train the Model
# automodel <- h2o.automl(x, y, train, test, max_runtime_secs = 60, seed=8)
# 
# #Obtained a list of models in order of performance. To learn more about them just call
# automodel@leader
# 
# #Apply the Model
# predictions <- h2o.predict(automodel@leader, facebook)
# 
# #Make it into a data frame I can use
# stock_predictions <- as.data.frame(predictions)
# 
# facebook <- read_csv("facebook.csv")
# facebook$Date <- as.Date(facebook$Date,format ='%m/%d/%Y' )
# facebook <- mutate(facebook, Company="Amazon")
# facebook <- select(facebook, Date, Close, Company)
# 
# #Tidy data
# new_row <- c(tail(predictions, n=1))
# stock_predictions <- mutate(stock_predictions, Date=facebook$Date)
# stock_predictions <- mutate(stock_predictions, Company="Facebook Prediction")
# stock_predictions <- rename(stock_predictions, Close=predict)
# stock_predictions <- select(stock_predictions, Date, Close, Company)
# stock_predictions <- arrange(stock_predictions, Date, Close, Company)
# 
# #Write as a csv file
# write.csv(stock_predictions, "stock_predictionsFacebook.csv")
# 
# testPredictFacebook <- rbind(facebook, stock_predictions)
# 
# ################################################################################################
# 
# #GOOGLE
# ###############################################################################################
# 
# #shifting n rows up of a given variable
# shift <- function(x, n) {
#   c(x[-(seq(n))], rep(NA, n))
# }
# 
# google$shifted <- shift(google$Close, 1)
# tail(google)
# 
# #remove NA observations
# google <- na.omit(google)
# write.csv(google, "google.csv")
# 
# #Initializing the Virtual Machine using all the threads (-1) and 16gb of memory
# h2o.init(nthreads = -1, max_mem_size = "16g")
# 
# google <- h2o.importFile("google.csv")
# h2o.describe(google)
# 
# y <- "shifted" #variable we want to forecast
# x <- setdiff(names(google), y)
# 
# set.seed(8)
# parts <- h2o.splitFrame(google, .80)
# train <- parts[[1]]
# test <- parts[[2]]
# 
# #Train the Model
# automodel <- h2o.automl(x, y, train, test, max_runtime_secs = 60, seed=8)
# 
# #Obtained a list of models in order of performance. To learn more about them just call
# automodel@leader
# 
# #Apply the Model
# predictions <- h2o.predict(automodel@leader, google)
# 
# #Make it into a data frame I can use
# stock_predictions <- as.data.frame(predictions)
# 
# google <- read_csv("google.csv")
# google$Date <- as.Date(google$Date,format ='%m/%d/%Y' )
# google <- mutate(google, Company="Google")
# google <- select(google, Date, Close, Company)
# 
# #Tidy data
# new_row <- c(tail(predictions, n=1))
# stock_predictions <- mutate(stock_predictions, Date=google$Date)
# stock_predictions <- mutate(stock_predictions, Company="Google Prediction")
# stock_predictions <- rename(stock_predictions, Close=predict)
# stock_predictions <- select(stock_predictions, Date, Close, Company)
# stock_predictions <- arrange(stock_predictions, Date, Close, Company)
# 
# #Write as a csv file
# write.csv(stock_predictions, "stock_predictionsGoogle.csv")
# 
# testPredictGoogle <- rbind(google, stock_predictions)
# 
# ################################################################################################
# 
# #NETFLIX
# ###############################################################################################
# 
# #shifting n rows up of a given variable
# shift <- function(x, n) {
#   c(x[-(seq(n))], rep(NA, n))
# }
# 
# netflix$shifted <- shift(netflix$Close, 1)
# tail(amazon)
# 
# #remove NA observations
# netflix <- na.omit(netflix)
# write.csv(netflix, "netflix.csv")
# 
# #Initializing the Virtual Machine using all the threads (-1) and 16gb of memory
# h2o.init(nthreads = -1, max_mem_size = "16g")
# 
# netflix <- h2o.importFile("netflix.csv")
# h2o.describe(netflix)
# 
# y <- "shifted" #variable we want to forecast
# x <- setdiff(names(netflix), y)
# 
# set.seed(8)
# parts <- h2o.splitFrame(netflix, .80)
# train <- parts[[1]]
# test <- parts[[2]]
# 
# #Train the Model
# automodel <- h2o.automl(x, y, train, test, max_runtime_secs = 60, seed=8)
# 
# #Obtained a list of models in order of performance. To learn more about them just call
# automodel@leader
# 
# #Apply the Model
# predictions <- h2o.predict(automodel@leader, netflix)
# 
# #Make it into a data frame I can use
# stock_predictions <- as.data.frame(predictions)
# 
# netflix <- read_csv("netflix.csv")
# netflix$Date <- as.Date(netflix$Date,format ='%m/%d/%Y' )
# netflix <- mutate(netflix, Company="Netlix")
# netflix <- select(netflix, Date, Close, Company)
# 
# #Tidy data
# new_row <- c(tail(predictions, n=1))
# stock_predictions <- mutate(stock_predictions, Date=netflix$Date)
# stock_predictions <- mutate(stock_predictions, Company="Netflix Prediction")
# stock_predictions <- rename(stock_predictions, Close=predict)
# stock_predictions <- select(stock_predictions, Date, Close, Company)
# stock_predictions <- arrange(stock_predictions, Date, Close, Company)
# 
# #Write as a csv file
# write.csv(stock_predictions, "stock_predictionsNetflix.csv")
# 
# testPredictNetflix <- rbind(netflix, stock_predictions)
# 
# ################################################################################################
# 

#SHINY APP
################################################################################################
ui <- fluidPage(
  navbarPage("StockTools",
             tabPanel("Closing Day Predictions",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("stockType", "Stock",
                                             c("AAPL"=1, "AMZN"=2, "FB"=3, "GOOG"=4, "NFLX"=5)
                                )
                              ),
                              mainPanel(
                                plotlyOutput("ml_plot")
                              )
                            )
                      ),
             tabPanel("FAANG Summary",
                      plotOutput("FAANG_plot")
                      ),
             tabPanel("Contact",
                      plotlyOutput("plotly_out")
                      )
  )
)

server <- function(input, output, session) {
  
    output$FAANG_plot <- renderPlot({
      ggplot(FAANG, aes(x=Date, y=Close, color=Company)) +
        ggtitle("FAANG Stock Closing Prices") +
        geom_line()
    })
    
    output$ml_plot <- renderPlotly({
      whichStock <- input$stockType
      if (whichStock == 1) {
        date_stock = amazon$date
        stock <- testPredictAmazon
        min_date <- date_stock[1][1]
        min_date = as.Date(min_date,format ='%m/%d/%Y')
        max_date <- date_stock[length(date_stock)][1]
        max_date = as.Date(max_date,format ='%m/%d/%Y')}
      else if (whichStock == 2) {
        stock <- testPredictApple
        date_stock = apple$date
        min_date <- date_stock[1][1]
        min_date = as.Date(min_date,format ='%m/%d/%Y')
        max_date <- date_stock[length(date_stock)][1]
        max_date = as.Date(max_date,format ='%m/%d/%Y')
          }
      else if (whichStock == 3) {
        stock <- testPredictFacebook
        date_stock = facebook$date
        min_date <- date_stock[1][1]
        min_date = as.Date(min_date,format ='%m/%d/%Y')
        max_date <- date_stock[length(date_stock)][1]
        max_date = as.Date(max_date,format ='%m/%d/%Y')
          }
      else if (whichStock == 4) {
        stock <- testPredictGoogle
        date_stock = google$date
        min_date <- date_stock[1][1]
        min_date = as.Date(min_date,format ='%m/%d/%Y')
        max_date <- date_stock[length(date_stock)][1]
        max_date = as.Date(max_date,format ='%m/%d/%Y')
          }
      else if (whichStock == 5) {
        stock <- testPredictNetflix
        date_stock = netflix$date
        min_date <- date_stock[1][1]
        min_date =as.Date(min_date,format ='%m/%d/%Y')
        max_date <- date_stock[length(date_stock)][1]
        max_date = as.Date(max_date,format ='%m/%d/%Y')
        }
      
      fig <- plot_ly(stock, type = 'scatter', mode = 'lines')%>%
        add_trace(x = ~Date, y = ~Close, color=~Company)%>%
        layout(showlegend = F, 
               title= 'Stock Prediction vs Actual Stock Closing Price',
               xaxis = list(autorange = TRUE),
               yaxis = list(autorange = TRUE)
               )
      fig <- fig %>%
        layout(
          xaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          yaxis = list(zerolinecolor = '#ffff',
                       zerolinewidth = 2,
                       gridcolor = 'ffff'),
          plot_bgcolor='#e5ecf6', margin = 0.1, width = 900)
      fig
    })
  
} 
shinyApp(ui = ui, server = server)


