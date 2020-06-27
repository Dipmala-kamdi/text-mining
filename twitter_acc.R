###################################### for perticular user ###################################### 
library("twitteR")
library("ROAuth")
#install.packages("rtweet")
library("rtweet")
library("dplyr")
library("tidyr")
library("tidytext")
library("ggplot2")
library(tm)
library("textdata")
library("wordcloud")
library(wordcloud2)

?create_token
create_token(app = "mytwitterapp", 
             consumer_key = "d980501janv" , 
             consumer_secret = "de980501janv", 
             access_token = "access_token", 
             access_secret = "access_secret")

data <- OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb',
                         consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', 
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         #authURL='https://api.twitter.com/oauth/authorize')
                         authURL ='https://api.twitter.com/oauth/authenticate')

save(data, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")

library(base64enc)

library(httpuv)


setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", 
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO",
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  

#Tweets_user <- userTimeline('BillGates', n = 1000,includeRts = T)
gates <- get_timeline("@BillGates", n= 1000)

#g_tweet <- gates %>% select(screen_name, text)
#g_tweet
#head(g_tweet$text)                                       

# Remove retweets
gates_tweets <- gates[gates$is_retweet==FALSE, ] 
# Remove replies
gates_tweets <- subset(gates_tweets, is.na(gates_tweets$reply_to_status_id)) 

# find highest no of retweets r like
gates_tweets <- gates_tweets %>% arrange(-favorite_count)
gates_tweets[1,5]
gates_tweets <- gates_tweets %>% arrange(-retweet_count)
gates_tweets[1,5]

# Keeping only the retweets
gates_retweets <- gates[gates$is_retweet==TRUE,]
# Keeping only the replies
gates_replies <- subset(gates, !is.na(gates$reply_to_status_id))

# Creating a data frame
data_1 <- data.frame(
  category=c("Organic", "Retweets", "Replies"),
  count=c(869, 38, 93)
  )

View(data_1)

# Adding columns 
data_1$fraction = data_1$count / sum(data_1$count)
data_1$percentage = data_1$count / sum(data_1$count) * 100
data_1$ymax = cumsum(data_1$fraction)
data_1$ymin = c(0, head(data_1$ymax, n=-1))

View(data_1)

library(dendroTools)
# Rounding the data to two decimal points
data_1 <- round_df(data_1, 2)

# Specify what the legend should say
Type_of_Tweet <- paste(data_1$category, data_1$percentage, "%")

ggplot(data_1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet)) +
  geom_rect() +
  coord_polar(theta="y") + 
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# frequency of tweet
colnames(gates)[colnames(gates)=="screen_name"] <- "Twitter_Account"

ts_plot(dplyr::group_by(gates, Twitter_Account), "year") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of Tweets from Bill Gates",
    subtitle = "Tweet counts aggregated by year",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

gates_app <- gates %>% 
  select(source) %>% 
  group_by(source) %>%
  summarize(count=n())

gates_app <- subset(gates_app, count > 11)

data_2 <- data.frame(
  category=gates_app$source,
  count=gates_app$count)

data_2$fraction = data_2$count / sum(data_2$count)
data_2$percentage = data_2$count / sum(data_2$count) * 100
data_2$ymax = cumsum(data_2$fraction)
data_2$ymin = c(0, head(data_2$ymax, n=-1))

data_2 <- round_df(data_2, 2)

Source <- paste(data_2$category, data_2$percentage, "%")

ggplot(data_2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "right")

# most frequent word

gates_tweets$text <-  gsub("https\\S*", "", gates_tweets$text)
gates_tweets$text <-  gsub("@\\S*", "", gates_tweets$text) 
gates_tweets$text  <-  gsub("amp", "", gates_tweets$text) 
gates_tweets$text  <-  gsub("[\r\n]", "", gates_tweets$text)
gates_tweets$text  <-  gsub("[[:punct:]]", "", gates_tweets$text)

tweets <- gates_tweets %>%
  select(text) %>%
  unnest_tokens(word, text)

tweets <- tweets %>%
  anti_join(stop_words)

# gives you a bar chart of the most frequent words found in the tweets
tweets %>% 
  count(word, sort = TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Most frequent words found in the tweets of Bill Gates",
       subtitle = "Stop words removed from the list")

# most frequently used hashtag

gates_tweets$hashtags <- as.character(gates_tweets$hashtags)
gates_tweets$hashtags <- gsub("c\\(", "", gates_tweets$hashtags)

set.seed(1234)
wordcloud(gates_tweets$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# most retweeted account
set.seed(1234)
wordcloud(gates_retweets$retweet_screen_name, min.freq=3, scale=c(2, .5), random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

############################## Sentimental Analysis ###############################

library(syuzhet)
# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(tweets, from="UTF-8", to="ASCII", sub="")

# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)

# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))

sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)

rownames(sentimentscores) <- NULL

ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()
