library(tm)
library(topicmodels)
library(SnowballC)
library(RSentiment)
#code for sonia
#setwd("~/GitHub/UnplannedPregnancy/TwitterAnalysis")
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

#prep for LDA
dtm <- DocumentTermMatrix(corpus)
rowTotals <- apply(dtm , 1, sum)
dtm   <- dtm[rowTotals> 0, ]
tweets$text.clean <- data.frame(text=sapply(corpus, identity), stringsAsFactors=F)$text
tweets <- tweets[rowTotals> 0, ]

#perform LDA
num_topics <- 20
ld <- LDA(dtm, k = num_topics)
terms(ld, 5)
tweets$topic_num <- as.data.frame(topics(ld))$`topics(ld)`

tweets$score <- as.data.frame(matrix(0, dim(tweets)[1], 1))$V1
for (i in 1:dim(tweets)[1]) {
  score <- calculate_score(tweets[i,'text.clean'])
  tweets[i, 'score'] <- as.data.frame(score)$score
}

save(tweets, file = 'tweets_an.rda') 
save(ld, file = 'lda.rda')
load('tweets_an.rda')

library(ggplot2)

qplot(tweets$topic_num,
      geom="histogram", 
      main = "Distribution of tweets by topic",
      xlab = "Topic Number",
      fill=I("darkolivegreen2"),
      col=I("black"),
      binwidth = 1)


qplot(tweets$score,
      geom="histogram", 
      main = "Distribution of sentiment",
      xlab = "Topic Number",
      fill=I("darkolivegreen2"),
      col=I("black"),
      binwidth = 1)

for (i in 1:20){
  #print(ggplot(tweets[tweets$topic_num == i,], aes(x = score)) +
   # geom_density(fill = "darkolivegreen2", colour = 'black') +
    #ggtitle(paste("Density plot of sentiment in Topic ",i)) )
  
  print(qplot(tweets[tweets$topic_num == i,'score'],
        geom="histogram", 
        main = paste("Distribution of sentiment in Topic ",i),
        xlab = "Topic Number",
        fill=I("darkolivegreen2"),
        col=I("black"),
        binwidth = 1))
}

for (i in c(1,2,6,9,10,12)){
  for (j in c(1,2,6,9,10,12)){
    if( i < j){
      print(i)
      print(j)
      print(t.test(subset(tweets, topic_num == i)$score, subset(tweets, topic_num == j)$score))
    }
  }
}

