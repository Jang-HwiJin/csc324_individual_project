#install.packages("tidyverse")
#install.packages("shiny")
#install.packages("DT")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("zoom")
#install.packages("scales")
#install.packages("plotrix")
library(dplyr)
library(tidyverse)
library(shiny)
library(DT)
library(readr)
library(zoom)
library(scales)
library(plotrix)

###############################################################################################
#Reading all the FAANG csv files and tidying it up to combine them

amazon <- read_csv("datasets/AMZN.csv")
amazon <- mutate(amazon, Company="Amazon")
amazon$Date <- as.Date(amazon$Date,format ='%m/%d/%Y' )
apple <- read_csv("datasets/AAPL.csv")
apple <- mutate(apple, Company="Apple")
apple$Date <- as.Date(apple$Date,format ='%m/%d/%Y' )
facebook <- read_csv("datasets/FB.csv")
facebook <- mutate(facebook, Company="Facebook")
facebook$Date <- as.Date(facebook$Date,format ='%m/%d/%Y' )
google <- read_csv("datasets/GOOG.csv")
google <- mutate(google, Company="Google")
google$Date <- as.Date(df$Date,format ='%m/%d/%Y' )
netflix <- read_csv("datasets/NFLX.csv")
netflix <- mutate(netflix, Company="Netflix")
netflix$Date <- as.Date(netflix$Date,format ='%m/%d/%Y' )

#Combining all the FAANG companies into one dataframe
FAANG <- rbind(amazon, apple, facebook, google, netflix)

#Removing Open, High, Low, and Adj Close column
FAANG <- FAANG %>% select(-one_of("Adj Close"))
FAANG$Date <- as.Date(FAANG$Date,format ='%m/%d/%Y' )

#Rearranging the dataframe so that Company comes first
FAANG <- select(FAANG, Company, Date, Open, High , Low, Close, Volume)
###############################################################################################
#Calculating the avergae highs of inidividual and FAANG

amazon_avg = mean(amazon$High)
amazon_avg
apple_avg = mean(apple$High)
apple_avg
facebook_avg = mean(facebook$High)
facebook_avg
google_avg = mean(google$High)
google_avg
netflix_avg = mean(netflix$High)
netflix_avg

faang_avg <- c(amazon_avg, apple_avg, facebook_avg, google_avg, netflix_avg)
labels <- c("Amazon", "Apple", "Facebook", "Google", "Netflix")
piepercent<- round(faang_avg*100/sum(faang_avg), 1)
piepercent <- percent(piepercent/100, accuracy = .1)

#Pie chart with no percents
pie(faang_avg, labels, main = "Average Highs of FAANG pie chart", col = rainbow(length(faang_avg)))

#Pie chart with percents
pie(faang_avg, labels = piepercent, main="Average Highs of FAANG pie chart", col = rainbow(length(faang_avg))) 
    legend("topright", labels, fill = rainbow(length(faang_avg)))

#3D pie chart 
pie3D(faang_avg,labels = labels, explode = 0.1, main = "Average Highs of FAANG pie chart")
###############################################################################################

#Individual stock graph
ggplot(data = amazon) + 
  geom_line(mapping = aes(x = Date, y = Close, color=Company)) #Amazon

ggplot(data = apple) + 
  geom_line(mapping = aes(x = Date, y = Close, color=Company)) #Apple

ggplot(data = facebook) + 
  geom_line(mapping = aes(x = Date, y = Close, color=Company)) #Facebook

ggplot(data = google) + 
  geom_line(mapping = aes(x = Date, y = Close, color=Company)) #Google

ggplot(data = netflix) + 
  geom_line(mapping = aes(x = Date, y = Close, color=Company)) #Netflix

#The FAANG graph
ggplot(FAANG, aes(x=Date, y=Close, color=Company)) +
  ggtitle("FAANG Stock Closing Prices") +
  geom_line()


###############################################################################################
#Experimenting with Shiny
ui <- fluidPage(
  plotOutput("allStocks", click="plot_click")
)

server <- function(input, output) {
  output$allStocks <- renderPlot({
    plot(amazon$Date, amazon$Close)
  })
}

shinyApp(ui = ui, server = server)