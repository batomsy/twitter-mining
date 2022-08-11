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

#you can add custom stop words here
mystopwords <- c(stopwords('english'),"obidi",'obidnt','obidient','obidients','peter','obi','im','na','dey','dont','word')
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

####Word cloud creates a cloud of all the frequently used words by size

library(wordcloud)
library(RColorBrewer)
library(tidyverse)
wordcloud(pobi, min.freq = 10, random.order = FALSE, colors = brewer.pal(8, "Dark2"))


##############################################################################
library(TSstudio)
library(tidytext)
library(tidyverse)

tweets.df$text <-  gsub("https\\S*", "",tweets.df$text)
tweets.df$text <-  gsub("@\\S*", "", tweets.df$text) 
tweets.df$text  <-  gsub("amp", "", tweets.df$text) 
tweets.df$text <-  gsub("[\r\n]", "", tweets.df$text)
tweets.df$text  <-  gsub("[[:punct:]]", "", tweets.df$text)
###rm(cleantweets)
cleantweets <- tweets.df %>%
  select(text) %>%
  unnest_tokens(word, text)
cleantweets <- cleantweets %>%
  anti_join(stop_words)

 
##convert custom stopwords to data frame and rename column as word to use anti_join
mystopwords<-data.frame(mystopwords)

colnames(mystopwords)<- c('word')
cleantweets <- cleantweets %>% anti_join(mystopwords)

# gives you a bar chart of the most frequent words found in the tweets
cleantweets %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in 'obidient' tweets",
       subtitle = "Custom stop words removed from the list")

###############################################################################

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

sentscore <- iconv(tweets.df$text)
sentscore2<- get_nrc_sentiment(sentscore)
head(sentscore2)

barplot(colSums(sentscore2),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for "obidient" Tweets')