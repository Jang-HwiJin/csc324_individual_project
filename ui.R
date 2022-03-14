ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  navbarPage("StockTools",
             tabPanel("Home",
                      HTML('<center><img src="stocks2.gif" width="500" height="300"></center>'),
                      h1("StockTools", align = "center"),
                      h3("Explore and Analyze stocks", align = "center"),
                      p("Press on the tabs above to look at the prediction closing day prices, stock analysis, and etc.", align = "center"),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      p("Hwi Jin Jang \U00A9 2022", align="center")
             ),
             tabPanel("Closing Day Predictions",
                      h1("What is the closing price going to be today?"),
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
             tabPanel("Volume",
                      h1("How much does volume affect the price? Positive or Negative Impact?"),
                      plotlyOutput("volumeStock")
             ),
             tabPanel("Growth",
                      h1("Which stock performed better percentage-wise?"),
                      plotlyOutput("growth")
             ),
             tabPanel("FAANG Summary",
                      h1("FAANG SUmmary Plot"),
                      plotlyOutput("FAANG_plot")
             )
             
  )
)
