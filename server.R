
server <- function(input, output, session) {
  
#SUMMARY 
  output$FAANG_plot <- renderPlotly({
    fig <- plot_ly(FAANG, type = 'scatter', mode = 'lines', colors = c("red", "blue", "brown", "black", "green"))%>%
      add_trace(x = ~Date, y = ~Close, color=~Company)%>%
      layout(showlegend = T,
             xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE)
      )
    fig <- fig %>%
      layout(
        title="FAANG Stocks Price Summary Over the Past Year",
        width = 1200, height = 800,
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6', margin = 0.1, width = 900)
    fig
  })
  
  
  
#STOCK PREDICTION
  output$ml_plot <- renderPlotly({
    whichStock <- input$stockType
    #Receive input from user of which stock they want to see and switch to that stock's date
    if (whichStock == 1) {
      date_stock = apple$date
      stock <- testPredictApple
      min_date <- date_stock[1][1]
      min_date = as.Date(min_date,format ='%m/%d/%Y')
      max_date <- date_stock[length(date_stock)][1]
      max_date = as.Date(max_date,format ='%m/%d/%Y')}
    else if (whichStock == 2) {
      stock <- testPredictAmazon
      date_stock = amazon$date
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
    
    #Plot the Prediction graph of the closing price
    fig <- plot_ly(stock, type = 'scatter', mode = 'lines', colors = c("red", "blue"))%>%
      add_trace(x = ~Date, y = ~Close, color=~Company)%>%
      layout(showlegend = T,
             xaxis = list(autorange = TRUE),
             yaxis = list(autorange = TRUE)
      )
    fig <- fig %>%
      layout(
        title="Closing Price ML Prediction",
        width = 1200, height = 800,
        xaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        yaxis = list(zerolinecolor = '#ffff',
                     zerolinewidth = 2,
                     gridcolor = 'ffff'),
        plot_bgcolor='#e5ecf6', margin = 0.1, width = 900)
    fig
  })
  
  
  
#VOLUME
#Create a bubble chart that shows the volume vs day price change
  output$volumeStock <- renderPlotly({
    fig <- plot_ly(FAANG, x = ~Volume, y = ~volumeDiff, color = ~Company, type = 'scatter', hoverinfo = 'text',
                   text = ~paste('Company:', Company, '<br>Date:', Date, '<br>Volume:', Volume,
                                 '<br>Price Change:', volumeDiff))
    fig <- fig %>% layout(width = 1400, height = 700,
              title="Volume affects Stock Prices to a Certain Degree",
               xaxis = list(title = 'Volume (Number of trades per day)',
                  showgrid = TRUE),
               yaxis = list(title = 'Stock Price Change (Close - Open)',
                  showgrid = TRUE))
    
    fig
  })
  
  
  
#Growth
#Create a pie chart with the percentage of their growth
  output$growth <- renderPlotly({
    fig <- plot_ly(
      title="FAANG stocks' growth over the past year",
      x = c("Apple", "Amazon", "Facebook", "Google", "Netflix"),
      y = c(apple_diff[1,1], amazon_diff[1,1], facebook_diff[1,1], google_diff[1,1], netflix_diff[1,1]),
      name = "Price Changes of Stocks (Growth)",
      type = "bar"
    )
    
    fig
  })
  
} 