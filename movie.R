library(rvest)
library(magrittr)
library(XML)
library(tm)
a<-10
joker<-NULL
url_joker<-"https://www.imdb.com/title/tt7286456/reviews?ref_=tt_ql_3"
#for(i in 0:30){
#  url<-read_html(as.character(paste(url_joker,i*a,sep="")))
#  ping<-url %>%
#    html_nodes("#tn15content p") %>%
#    html_text() 
#  joker<-c(joker,ping)
#}
?html_nodes

for(i in 0:30){
  url<-read_html(as.character(paste(url_joker,i*a,sep="")))
  ping<-url %>%
    #html_nodes("center") %>% html_nodes("font") %>%
    html_text() 
  joker<-c(joker,ping)
}

joker
write.csv(joker,file="joker1.csv")
getwd()
write.table(joker,"joker_2019.txt")
J_2019<-read.csv(file.choose())
View(J_2019)
html_text(html_text(html_node("#tn15content p",url_joker)))

##########################

#### Text Cleaning ####
corpus_1 <- Corpus(VectorSource(joker))
corpus_j <- tm_map(corpus_1, stripWhitespace)
corpus_j <- tm_map(corpus_j, removeWords, stopwords('english'))
corpus_j <- tm_map(corpus_j, removePunctuation)

df_j <- data.frame(text = get("content", corpus_j))
View(df_j)
######
docs_1 <- corpus_1 %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_1 <- tm_map(docs_1, content_transformer(tolower))
docs_1 <- tm_map(docs_1, removeWords, stopwords("english"))

dtm_1 <- TermDocumentMatrix(docs_1) 
matrix_1 <- as.matrix(dtm_1) 
words <- sort(rowSums(matrix_1),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)
View(df)

### Generate Word Cloud ###

set.seed(1234) # for reproducibility 
wordcloud(words = df$word, freq = df$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
############################################################################
?wordcloud2
wordcloud2(data=df, size=1.6, color='random-dark')
wordcloud2(data=df, size=0.7, color='random-dark', shape = 'pentagon')
wordcloud2(data=df, size=0.7, color='random-dark', shape = 'diamond')



#### Sentimental Analysis ####
#install.packages('sentimentr')
library(sentimentr)

my_text <- get_sentences(df_j)
text_data <- sentiment(my_text)
View(text_data)

my_1 <- get_sentences(df)
my_1 <- sentiment(my_1)
View(my_1)

## for sentiment analysis
#install.packages("syuzhet") 
library("syuzhet")

ss <- as.vector(as.character(df$word))

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
##first element of each row is either 0, 1, -1.

sen <-get_nrc_sentiment(ss)
head (sen,10)

#transpose
td_1<-data.frame(t(sen))
td_1_new <- data.frame(rowSums(td_1[2:3043]))
#Transformation and cleaning
names(td_1_new)[1] <- "count"
td_1_new <- cbind("sentiment" = rownames(td_1_new), td_1_new)
td_1_new
rownames(td_1_new) <- NULL
td_new_2<-td_1_new[1:8,]
#Plot One - count of words associated with each sentiment
library(ggplot2)
quickplot(sentiment, data=td_new_2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")
