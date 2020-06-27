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

####################### for perticular hashtag

?create_token
create_token(app = "mytwitterapp", 
             consumer_key = "d980501janv" , 
             consumer_secret = "de980501janv", 
             access_token = "access_token", 
             access_secret = "access_secret")
corona <- search_tweets("#stayhomestaysafe", lang='en', n=100, include_rts = FALSE)

#tweets.corona = corona %>% select.list(screenName, preselect = NULL, multiple = FALSE, title = NULL)
?select.list

tweets.corona1 = corona %>% select(screen_name, text)
tweets.corona1
head(tweets.corona1$text)                                       

#remove htttp element
tweets.corona1$stripped_text <- gsub("http\\s+", "", tweets.corona1$text)
tweets.corona1$stripped_text <- gsub("@\\S*", "", tweets.corona1$text)
tweets.corona1$stripped_text <- gsub("amp", "", tweets.corona1$text)
tweets.corona1$stripped_text <- gsub("[\r\n]", "", tweets.corona1$text)
tweets.corona1$stripped_text <- gsub("[[:punct:]]", "", tweets.corona1$text)


#remove panchuation and add id
tweets.corona1_stem <- tweets.corona1%>% select(stripped_text) %>% unnest_tokens(word, stripped_text)

head(tweets.corona1_stem)

#remove stop word
clean_tweets.corona1 <- tweets.corona1_stem %>% anti_join(stop_words)

head(clean_tweets.corona1)
#######################################

##tweets_words <-  clean_tweets.corona1 %>%select(text) %>%unnest_tokens(word, text)
words <- clean_tweets.corona1 %>% count(word, sort=TRUE)
#######################################
#top 10 words in tweet

clean_tweets.corona1 %>% count(word, sort = TRUE)%>% top_n(5) %>%
  mutate(word= reorder(word, n)) %>% 
  ggplot(aes(x=word, y=n)) +xlab(NULL)+coord_flip()+theme_classic()+ geom_col()+
  labs(x="count", y="unique words", title = "#corona tweet")

### Generate Word Cloud ###

set.seed(1234) # for reproducibility 
wordcloud(words = words$word, freq = words$n, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
############################################################################
?wordcloud2
wordcloud2(data=words, size=1.6, color='random-dark')
wordcloud2(data=words, size=0.7, color='random-dark', shape = 'pentagon')
wordcloud2(data=words, size=0.7, color='random-dark', shape = 'diamond')


#### Text Cleaning ####
#corpus <- Corpus(VectorSource(clean_tweets.corona1))
#corpus <- tm_map(corpus, content_transformer(function(s){
#  gsub(pattern = '[^a-zA-Z0-9\\s]+',x = s,replacement = " ",
#       ignore.case = TRUE,perl = TRUE)}))

#### Sentimental Analysis ####
#install.packages('sentimentr')
library(sentimentr)

corona_tweet <- get_sentences(clean_tweets.corona1)
tweet_data <- sentiment(corona_tweet)
View(tweet_data)

write.table(t_data,"corona_tweets.csv",row.names = F)
getwd()

library("syuzhet")

ss <- as.vector(as.character(words$word))

# syuzhet = ranges from -1(indicating most negative) to +1(indicating most positive)
syuzhet_vector<- get_sentiment(ss, method="syuzhet")
syuzhet_vector
summary(syuzhet_vector)
head(syuzhet_vector)

#bing = binary scale with -1 indicating negative and +1 indicating positive sentiment
bing_vector <- get_sentiment(ss, method="bing")
bing_vector
summary(bing_vector)
head(bing_vector)

#afinn - integer scale ranging from -5 to +5
afinn_vector <- get_sentiment(ss, method="afinn")
afinn_vector
summary(afinn_vector)
head(afinn_vector)

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)
##first element of each row is showing same value i. e. 0,1.

sen <-get_nrc_sentiment(ss)
head (sen,10)

#transpose
td_1<-data.frame(t(sen))
td_1_new <- data.frame(rowSums(td_1[2:1054]))
#Transformation and cleaning
names(td_1_new)[1] <- "count"
td_1_new <- cbind("sentiment" = rownames(td_1_new), td_1_new)
td_1_new
rownames(td_1_new) <- NULL
td_new_2<-td_1_new[1:8,]
#Plot One - count of words associated with each sentiment
library(ggplot2)
quickplot(sentiment, data=td_new_2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")

