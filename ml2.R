#install.packages("pROC")
#install.packages("quantmod")
#install.packages("TTR")
#install.packages("caret")
#install.packages("corrplot")
#install.packages("FSelector")
#install.packages("rJava")
#install.packages("klaR")
#install.packages("randomForest")
#install.packages("kernlab")
library(quantmod)
library(pROC)
library(TTR)
library(caret)
library(corrplot)
library(FSelector)
library(rJava)
library(klaR)
library(randomForest)
library(kernlab)
library(rpart)

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