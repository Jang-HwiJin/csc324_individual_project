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
#amazon <- mutate(amazon, Company="Amazon")
amazon$Date <- as.Date(amazon$Date,format ='%m/%d/%Y' )
amazon <- amazon %>% select(-one_of("Adj Close"))
amazon <- amazon %>% select(-one_of("Date"))
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

parts <- h2o.splitFrame(amazon, .80)
train <- parts[[1]]
test <- parts[[2]]

#Train the Model
automodel <- h2o.automl(x, y, train, test, max_runtime_secs = 120)

#Obtained a list of models in order of performance. To learn more about them just call
automodel@leader

#Apply the Mode
predictions <- h2o.predict(automodel@leader, test)





