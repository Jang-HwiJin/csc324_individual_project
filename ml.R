library(dplyr)
library(tidyverse)
library(shiny)
library(DT)
library(readr)
library(zoom)
library(scales)
library(plotrix)

#Installing the package
#install.packages("h2o")
#loading the library 
library(h2o)

###############################################################################################
#Reading all the FAANG csv files and tidying it up to combine them

amazon <- read_csv("datasets/AMZN.csv")
amazon <- mutate(amazon, Company="Amazon")
amazon$Date <- as.Date(amazon$Date,format ='%m/%d/%Y' )
amazon <- amazon %>% select(-one_of("Adj Close"))
apple <- read_csv("datasets/AAPL.csv")
apple <- mutate(apple, Company="Apple")
apple$Date <- as.Date(apple$Date,format ='%m/%d/%Y' )
apple <- apple %>% select(-one_of("Adj Close"))
facebook <- read_csv("datasets/FB.csv")
facebook <- mutate(facebook, Company="Facebook")
facebook$Date <- as.Date(facebook$Date,format ='%m/%d/%Y' )
facebook <- facebook %>% select(-one_of("Adj Close"))
google <- read_csv("datasets/GOOG.csv")
google <- mutate(google, Company="Google")
google$Date <- as.Date(df$Date,format ='%m/%d/%Y' )
google <- google %>% select(-one_of("Adj Close"))
netflix <- read_csv("datasets/NFLX.csv")
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

###############################################################################################

#shifting n rows up of a given variable
shift <- function(x, n) {
  c(x[-(seq(n))], rep(NA, n))
}

amazon$shifted <- shift(amazon$Close, 1)
tail(amazon)

#remove NA observations
amazon <- na.omit(amazon)
write.csv(amazon, "amazon.csv")

#Initializing the Virtual Machine using all the threads (-1) and 16gb of memory
h2o.init(nthreads = -1, max_mem_size = "16g")

amazon <- h2o.importFile("amazon.csv")
h2o.describe(amazon)

y <- "shifted" #variable we want to forecast
x <- setdiff(names(amazon), y)

set.seed(7)
parts <- h2o.splitFrame(amazon, .80)
train <- parts[[1]]
test <- parts[[2]]

#Train the Model
automodel <- h2o.automl(x, y, train, test, max_runtime_secs = 10, seed=7)

#Obtained a list of models in order of performance. To learn more about them just call
automodel@leader

#Apply the Model
predictions <- h2o.predict(automodel@leader, amazon)

#Make it into a data frame I can use
stock_predictions <- as.data.frame(predictions)

amazon <- read_csv("datasets/AMZN.csv")
amazon$Date <- as.Date(amazon$Date,format ='%m/%d/%Y' )
amazon <- mutate(amazon, Company="Amazon")
amazon <- select(amazon, Date, Close, Company)

#Tidy data
new_row <- c(tail(predictions, n=1))
stock_predictions <- rbind(stock_predictions, new_row)
stock_predictions <- mutate(stock_predictions, Date=amazon$Date)
stock_predictions <- mutate(stock_predictions, Company="Amazon Prediction")
stock_predictions <- rename(stock_predictions, Close=predict)
stock_predictions <- select(stock_predictions, Date, Close, Company)
stock_predictions <- arrange(stock_predictions, Date, Close, Company)

#Write as a csv file
write.csv(stock_predictions, "stock_predictions.csv")

testPredict <- rbind(amazon, stock_predictions)

ggplot(testPredict, aes(x=Date, y=Close, color=Company)) +
  ggtitle("Amazon's actual vs prediction") +
  geom_line()


################################################################################################

#MSE(Mean Squared Error)
#The MSE metric measures the average of the squares of the errors or deviations. MSE takes the distances from the points to the regression line (these distances are the "errors") and squaring them to remove any negative signs. 
#MSE also gives more weight to larger differences. The bigger the error, the more it is penalized. 
#The smaller the MSE, the better the model's performance.

#RMSE(Root Mean Squared Error)
#The RMSE metric evaluates how well a model can predict a continuous value. 
#The smaller the RMSE, the better the model's performance. 
#RMSE penalizes large gaps more harshly than MAE

#MAE(Mean Absolute Error)
#The mean absolute error is an average of the absolute errors. 
#The smaller the MAE the better the model's performance. (Tip: MAE is robust to outliers. 
#If you want a metric that is sensitive to outliers, try root mean squared error (RMSE).)

#RMSLE(Root Mean Squared Logarithmic Error)
#This metric measures the ratio between actual values and predicted values and takes the log of the predictions and actual values. 
#RMSLE penalizes large gaps among small output-values more harshly than large gaps among large output-values
#In simple words, more penalty is incurred when the predicted Value is less than the Actual Value. On the other hand, Less penalty is incurred when the predicted value is more than the actual value.

################################################################################################
min_date = "1997-05-15"
max_date = "2022-02-11"

ui <- fluidPage(
  dateRangeInput("inDateRange", "Input date range"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  observe({
    updateDateRangeInput(session = getDefaultReactiveDomain(), "inDateRange",
      label = paste("Date range label"),
      start = as.Date(input$inDateRange[1], format ='%m/%d/%Y'),
      end = as.Date(input$inDateRange[2], format ='%m/%d/%Y'),
      min = min_date,
      max = max_date
    )
  
    range_date =  c(as.Date(input$inDateRange[1], format ='%m/%d/%Y'), as.Date(input$inDateRange[2], format ='%m/%d/%Y'))
    output$plot <- renderPlot({
      ggplot(testPredict, aes(x=Date, y=Close, color=Company)) +
        ggtitle("Amazon's actual vs prediction") +
        geom_line() + 
        xlim(range_date)
    })
  })
} 
shinyApp(ui = ui, server = server)

