stories <- read.csv("/Users/amberlehman/Documents/MATH 552/Unplanned Pregnancy Stories.csv")
setwd("/Users/amberlehman/Documents/MATH 552")
cat(stories$story,sep="\n\n",file="stories2.txt",append=T)
write.table(stories$story,"filename.txt",sep="\n\n",row.names=FALSE)

# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
inspect(docs)
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#create TDC
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
findFreqTerms(dtm, lowfreq = 30)
findAssocs(dtm, terms = "abortion", corlimit = 0.2)
findAssocs(dtm, terms = "baby", corlimit = 0.4)
findAssocs(dtm, terms = "defundpp", corlimit = 0.2)
head(d,10)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

text <- readLines(file.choose())
docs <- Corpus(VectorSource(text))
inspect(docs)
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
#create TDC
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                       max.words=200, random.order=FALSE, rot.per=0.35, 
                       colors=brewer.pal(8, "Dark2"))
findFreqTerms(dtm, lowfreq = 200)
findAssocs(dtm, terms = "abortion", corlimit = 0.4)
findAssocs(dtm, terms = "baby", corlimit = 0.4)
findAssocs(dtm, terms = "unexpected", corlimit = 0.4)
head(d,10)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


  
