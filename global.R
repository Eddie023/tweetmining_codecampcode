library(twitteR)
library(tm)
library(rjson)
library(wordcloud)
library(dplyr)
library(caret)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(syuzhet) 
library(scales)
library(rbokeh)
library(base64enc) 
library(SnowballC) 
library(plyr)
library(reshape2)
runOnline = T

#getting authorization
if(runOnline){
  consumer_key <- 'X7y9wg2GahcTWnIBJO0LKoYBu'
  consumer_secret <- 'U20M2NtDyGGCAQEGZ6imkL6oEYE8oYd1D2Oq2HyJJuVDu7aPgx'
  access_token <- '1163245578-OmQumQ3Ck8OxttsUJzCUnL8AmDHMvAwpCQGCuCt'
  access_secret <- 'MtAjDdvXeBOIBs6XVMVjDfFmB404I817N4PLDlKNXr5H1'
  setup_twitter_oauth(consumer_key,consumer_secret, access_token, access_secret)
}



# getting tweets
getTweets <- function(searchWord, noTweets, rt_remove, isUser){
    
    if(runOnline & !isUser){
        st <- searchTwitter(searchWord, n=noTweets, resultType = 'recent', lang = 'en')
        
        statuses <- data.frame(text=sapply(st, function(x) x$getText()),
                               user=sapply(st, function(x) x$getScreenName()),
                               RT=sapply(st, function(x) x$isRetweet),
                               latitude=sapply(st, function(x) as.numeric(x$latitude[1])),
                               longitude=sapply(st, function(x) as.numeric(x$longitude[1])),
                               time=sapply(st, function(x) format(x$created, format='%F %T'))
        )
    }
    
    if(isUser){
        
        st <- userTimeline(searchWord, n=noTweets, includeRts=!rt_remove)
        statuses <- data.frame(text=sapply(st, function(x) x$getText()),
                               user=sapply(st, function(x) x$getScreenName()),
                               RT=sapply(st, function(x) x$isRetweet),
                               time=sapply(st, function(x) format(x$created, format='%F %T'))
        )
    }
    
   
    
    if(rt_remove){
        print('Removing Retweets')
        statuses <-
            statuses %>%
            filter(!RT)
    }
    
    
    return(statuses)
}






getTextData <- function(statuses) {
    # Getting corpus
    textdata <- Corpus(VectorSource(statuses$text))
    textdata <- 
        textdata %>%
        tm_map(removeWords, stopwords("english"), mc.cores=1) %>%
        tm_map(removePunctuation, mc.cores=1) %>%
        tm_map(content_transformer(function(x) iconv(x, from='ASCII', 
                                                     to='UTF-8', sub='byte')),
               mc.cores=1) %>%
        tm_map(content_transformer(tolower), mc.cores=1) %>%
        tm_map(content_transformer(function(x) str_replace_all(x, "@\\w+", "")), 
               mc.cores=1) %>% # remove twitter handles
        tm_map(removeNumbers, mc.cores=1) %>%
       ## tm_map(stemDocument, mc.cores=1) %>%
        tm_map(stripWhitespace, mc.cores=1)
}

# geting sentiments data
getSentiments <- function(textdata){
    sentiments <- sapply(textdata, function(x) get_nrc_sentiment(as.character(x)))
    
    sentiments <- as.data.frame(aperm(sentiments)) # transpose and save as dataframe
    sentiments <- as.data.frame(lapply(sentiments, as.numeric)) # a bit more to organize
    sentiments <-
        sentiments %>%
        mutate(positivity = positive - negative)
}



