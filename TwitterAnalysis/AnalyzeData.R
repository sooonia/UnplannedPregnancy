library(tm)
library(topicmodels)
library(SnowballC)
load('tweets.rda')

useful.vars <- c('text', 'source', 'is_retweet', 'favorite_count', 'retweet_count', 'hashtags', 'lang')
tweets <- tw[useful.vars]
corpus <- Corpus(VectorSource(tweets$text))

#cleaning
#remove stopwords
corpus <- tm_map(corpus, content_transformer(removeWords), stopwords("english"))
#lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
#stem words --- good idea??
#corpus <- tm_map(corpus, stemDocument)
#strip whitespace
corpus <- tm_map(corpus, stripWhitespace) 

#tweets$text.clean <- data.frame(text=sapply(corpus, identity), stringsAsFactors=F)$text

dtm <- DocumentTermMatrix(corpus)
rowTotals <- apply(dtm , 1, sum)
dtm   <- dtm[rowTotals> 0, ]


ld <- LDA(dtm, k = 20)
terms(ld, 5)