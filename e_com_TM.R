setwd("D:\\excelr_DS\\assignment\\text mining")
library(rvest)
library(XML)
library(magrittr)
library(tm)
#install.packages("wordcloud")
library(wordcloud)
#install.packages("RColorBrewer")
library(RColorBrewer)
#install.packages("wordcloud2")
library(wordcloud2)

#######################################################################################
#######################################################################################

### Amazon Reviews ###
link <- "https://www.amazon.in/Apple-iPhone-11-64GB-Product/dp/B07XVKG5XV?th=1"
amazon_reviews <- NULL
for (i in 1:10){
  murl <- read_html(as.character(paste(link,i,sep="=")))
  rev <- murl %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,rev)
}
write.table(amazon_reviews,"iphone.txt",row.names = F)
length(amazon_reviews)

#### Text Cleaning ####
corpus <- Corpus(VectorSource(amazon_reviews))
cor_pus <- tm_map(corpus, stripWhitespace)
corpus_1 <- tm_map(cor_pus, removeWords, stopwords('english'))

df <- data.frame(text = get("content", corpus_1))
View(df)
#dataframe<-data.frame(text=unlist(sapply(corpus_1, '[', "content")), stringsAsFactors=FALSE) 
# Convert data back to data frame from corpus
#sentence<-as.character(dataframe)
#write.table(corpus_1,"iphone_1.txt",row.names = F)

############################################################################
docs <- corpus %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df_1 <- data.frame(word = names(words),freq=words)
View(df_1)

### Generate Word Cloud ###

set.seed(1234) # for reproducibility 
wordcloud(words = df_1$word, freq = df_1$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
############################################################################
?wordcloud2
wordcloud2(data=df_1, size=1.6, color='random-dark')
wordcloud2(data=df_1, size=0.7, color='random-dark', shape = 'pentagon')

#### Sentimental Analysis ####
install.packages('sentimentr')
library(sentimentr)

mytext <- get_sentences(df)
text <- sentiment(mytext)
View(text)

write.table(text,"iphone_data.txt",row.names = F)

# for sentiment analysis
install.packages("syuzhet") 
library("syuzhet")

qq <- as.vector(as.character(df_1$word))

# syuzhet = ranges from -1(indicating most negative) to +1(indicating most positive)
syuzhet_text<- get_sentiment(qq, method="syuzhet")
syuzhet_text
summary(syuzhet_text)

#bing = binary scale with -1 indicating negative and +1 indicating positive sentiment
bing_text <- get_sentiment(qq, method="bing")
bing_text
summary(bing_text)

#afinn - integer scale ranging from -5 to +5
afinn_text <- get_sentiment(qq, method="afinn")
afinn_text
summary(afinn_text)

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_text)),
  sign(head(bing_text)),
  sign(head(afinn_text))
)
##first element of each rowis 0
##indicating that all three methods have calculated a negative sentiment score

d<-get_nrc_sentiment(qq)
head (d,10)

#transpose
td<-data.frame(t(d))
td_new <- data.frame(rowSums(td[2:242]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
td_new
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
library(ggplot2)
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")


#######################################################################################
#######################################################################################


### Snapdeal reviews ###
link_1 <- "https://www.snapdeal.com/product/ptron-rhythm-smart-watches-white/649663821259/reviews?page="
link_2 <- "&sortBy=RECENCY&rateFilter=4#defRevPDP"

#link_1 <- "https://www.snapdeal.com/product/samsung-galaxy-J3-8gb-4g/676860597612/ratedreviews?page="
#link_2 <- "&sortBy=HELPFUL&ratings=4,5#defRevPDP"

snapdeal_reviews <- NULL
for (i in 1:30){
  surl <- read_html(as.character(paste(link_1,link_2,sep=as.character(i))))
  srev <- surl %>%
    html_text()
  snapdeal_reviews <- c(snapdeal_reviews,srev)
}

write.table(snapdeal_reviews,"smart_watch.txt",row.names = FALSE)
getwd()

length(snapdeal_reviews)

#### Text Cleaning ####
corpus_snap <- Corpus(VectorSource(snapdeal_reviews))
cor_pus_snap <- tm_map(corpus_snap, stripWhitespace)
cor_pus_snap <- tm_map(cor_pus_snap, removeWords, stopwords('english'))
cor_pus_snap <- tm_map(cor_pus_snap, removePunctuation)

df_snap <- data.frame(text = get("content", cor_pus_snap))
View(df_snap)
##################

############################################################################
docs_1 <- corpus_snap %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs_1 <- tm_map(docs_1, content_transformer(tolower))
docs_1 <- tm_map(docs_1, removeWords, stopwords("english"))

dtm_1 <- TermDocumentMatrix(docs_1) 
matrix_1 <- as.matrix(dtm_1) 
words_snap <- sort(rowSums(matrix_1),decreasing=TRUE) 
snap_df <- data.frame(word = names(words_snap),freq=words_snap)
View(snap_df)

### Generate Word Cloud ###

set.seed(1234) # for reproducibility 
wordcloud(words = snap_df$word, freq = snap_df$freq, min.freq = 1, max.words=200, 
          random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
############################################################################
?wordcloud2
wordcloud2(data=snap_df, size=1.6, color='random-dark')
wordcloud2(data=snap_df, size=0.7, color='random-dark', shape = 'pentagon')
wordcloud2(data=snap_df, size=0.7, color='random-dark', shape = 'diamond')


#### Sentimental Analysis ####
library(sentimentr)

mytext_snap <- get_sentences(df_snap)
text_snap <- sentiment(mytext_snap)
View(text_snap)

my_1 <- get_sentences(snap_df)
my_1 <- sentiment(my_1)
View(my_1)

write.table(text_snap,"watch_data.txt",row.names = F)
getwd()

# for sentiment analysis
#install.packages("syuzhet") 
library("syuzhet")

aa <- as.vector(as.character(snap_df$word))

# syuzhet = ranges from -1(indicating most negative) to +1(indicating most positive)
syuzhet_vector<- get_sentiment(aa, method="syuzhet")
syuzhet_vector
summary(syuzhet_vector)
head(syuzhet_vector)

#bing = binary scale with -1 indicating negative and +1 indicating positive sentiment
bing_vector <- get_sentiment(aa, method="bing")
bing_vector
summary(bing_vector)
head(bing_vector)

#afinn - integer scale ranging from -5 to +5
afinn_vector <- get_sentiment(aa, method="afinn")
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

sen <-get_nrc_sentiment(aa)
head (sen,10)

#transpose
td_1<-data.frame(t(sen))
td_1_new <- data.frame(rowSums(td_1[2:670]))
#Transformation and cleaning
names(td_1_new)[1] <- "count"
td_1_new <- cbind("sentiment" = rownames(td_1_new), td_1_new)
td_1_new
rownames(td_1_new) <- NULL
td_new_2<-td_1_new[1:8,]
#Plot One - count of words associated with each sentiment
library(ggplot2)
quickplot(sentiment, data=td_new_2, weight=count, geom="bar", fill=sentiment, ylab="count")+ggtitle("Survey sentiments")
