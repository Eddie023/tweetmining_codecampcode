library(shiny)

function(input, output, session) {
    
    statuses <- reactive({
        input$update
      
        isolate({
            withProgress({
                setProgress(message = "Extracting tweets...")
                getTweets(input$searchWord, input$noTweets, 
                          input$rt_remove, input$isUser)
            })
        })
    })
  
    
    
    textdata <- reactive({
              input$update
      
        getTextData(statuses())
    })
    
   
    
    sentiments <- reactive({
      
        input$update
      
        
            withProgress({
                setProgress(message = "Gathering sentiments...")
                sentiments <- getSentiments(textdata())
            })
        
    })
    
    run <- reactive({
      
      withProgress({
        setProgress(message = "Running ...")
        doPCA(textdata(), statuses(), sentiments())
      })
      
    })
   
    
    # Make the wordcloud 
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plot <- renderPlot({
        wordcloud_rep(textdata(),
                      scale=c(4,0.8),
                      min.freq=3,
                      max.words=300,
                      colors=brewer.pal(8, "Spectral"),
                      random.order=F, 
                      rot.per=0.35,
                      use.r.layout=F)
    })
    
    
    
    output$tweetCount  <- renderText({
        df <- statuses()
        if(input$isUser){
            paste("Number of Tweets Found: ", as.character(nrow(statuses())),
                  "\nUser: ",as.character(df$user[1]),
                  "\nDescription: ",as.character(getUser(df$user[1])$description)
                  )
        }else{
            paste("Number of Tweets Found: ", as.character(nrow(df)))
        }
        
    })
    
    output$sentiment <- renderPlot({
        v <- sentiments()
        emotions <- data.frame("count"=colSums(v[,c(1:8)]))
        emotions <- cbind("sentiment" = rownames(emotions), emotions)
        ggplot(data = emotions, aes(x = sentiment, y = count)) +
            geom_bar(aes(fill = sentiment), stat = "identity") +
            xlab("Sentiment") + ylab("Total Count") + 
            scale_fill_brewer(palette='RdBu') + 
            theme_bw() + theme(legend.position='none')
    })
    
   
    
    output$tweet_table <- DT::renderDataTable({
      df <- run()[1]$statuses
      text <- df$text
 
      temp <- data.frame(text=text)
    
      
      DT::datatable(temp)
    })
}
