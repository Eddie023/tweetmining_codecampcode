library(shiny)
library(shinythemes)

shinyUI(
   
             
                        fluidPage( theme = shinytheme("flatly"),
                                
                              titlePanel("Tweet Mine",title = h1("Tweets Visualization", align = "center")),
                                  sidebarLayout(
                                
                             
                                sidebarPanel(h2("Enter Data to Start Mining"),
                                  
                                  
                                    textInput("searchWord",
                                              "Search Twitter for:",
                                              "metallica"),
                                    
                                    sliderInput("noTweets", 
                                                "Select Number of Tweets to Mine", 
                                                min = 500, 
                                                max = 2000, 
                                                value = 1500),
                                    
                                    checkboxInput("rt_remove", "Eliminate Retweets",
                                                  value=T),
                                    checkboxInput("isUser", "Search is a Screen Name",
                                                  value=F),
                                    actionButton("update", "Search")
                                ),
                                
                                mainPanel(
                                  tabsetPanel(
                                  tabPanel("World Cloud",plotOutput("plot"),verbatimTextOutput("tweetCount")),
                                  tabPanel("Sentiment",plotOutput("sentiment")),
                                  tabPanel("Tweets",DT::dataTableOutput('tweet_table'))
                   
                                             ) 
                
                                         )
                            )       
                        )
               )
               
    


