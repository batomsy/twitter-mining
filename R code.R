library(twitteR)


api_key <-
api_secret_key <- 
access_token <- 
access_token_secret <-
setup_twitter_oauth(api_key,api_secret_key,access_token,access_token_secret)

1
#search for tweets and exclude retweets

tweets <- searchTwitter('obidient -filter:retweets', n=3000)

#count the number of tweets

n.tweets <- length(tweets)

#convert tweets to dataframe

tweets.df <- twListToDF(tweets)
View(tweets.df)

#cleaning tweets

library(tm)
pobi <- Corpus(VectorSource(tweets.df$text))
pobi <- tm_map(pobi, removeWords, stopwords())

#you can remove custom stopwords here
mystopwords <- c(stopwords('english'),"obidnt")
pobi <- tm_map(pobi, removeWords, mystopwords)

remove_url <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
pobi <- tm_map(pobi,  content_transformer(remove_url))

#remove anything other than english letters and space

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
pobi <- tm_map(pobi,  content_transformer(removeNumPunct))
pobi <- tm_map(pobi,  content_transformer(tolower))
pobi <- tm_map(pobi, stripWhitespace)
pobi <- tm_map(pobi, stemDocument)

dtm <- DocumentTermMatrix(pobi)

library(wordcloud)
library(RColorBrewer)
wordcloud(pobi, min.freq = 10, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
