library(textclean)
library(rtweet)
library(httpuv)
library(twitteR)

#Script to get data
getData <- function(n= 10000, x){
  
  #this x is the requested terms to search for separated by a comma and in quotes
  
  
  #do you want the tweets cleaned of punctuation and make everything lowercase
  clean<-1
  string<-x[1]
  for(i in 2:length(x)){
    string<-paste(string,'+',x[i])
  }
  
  
  #This function searches twitter for tweets containing all requested terms from the last 10 days
  tw<-NULL
  
  new.tweets<-search_tweets(x,n=n,full_text='extended')
  if(clean>0){
    for(j in 1:nrow(new.tweets)){
      str<-as.character(new.tweets[j,5])
      str <- gsub('[[:punct:]]','',str)
      str<-replace_emoji_identifier(str,emoji_dt = lexicon::hash_emojis_identifier)
      str<-replace_emoji(str,emoji_dt = lexicon::hash_emojis)
      str<-replace_emoticon(str, emoticon_dt = lexicon::hash_emoticons)
      str<-gsub("<...","",str)
      str<-gsub(" http.*","",str)
      new.tweets[j,5]<-str
    }
  }
  tw<-rbind(tw,new.tweets)
  if(nrow(new.tweets)==n){print(paste('more tweets of: ',x[i],' may be possible'))}
  
  return(tw)
}


#DO NOT GIVE OUT THE FOLLOWING INFORMATION TO ANYONE
consumer_key <- 	'3bxyiuG3DJzmXlUvAF6NRQpv5'
consumer_secret <- 'i3Td2j5iQToDCcSTm0kLshb6vIIETutoT2YHYY40kbPt9oqr4V'
access_token <- "953818124243087360-kEijMPCzceyjWH3D9Hf8BSImq9K3ihp"
access_secret <- "GjDCLKyT1klUeRVJoEsSH74z7tFt8fEc5cj9Ks3YYp6vs"


#This authorizes your computer to receive the information from twitter
# This should only be run once per R session
twitter_tokens <- create_token(app = "API Tester App for MAT 552",consumer_key = consumer_key, consumer_secret = consumer_secret)
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Call to twitter for tweets containing specific hashtags or words
#n is the number of tweets requested for each term

x <- c('pregnancy OR planned parenthood OR #istandwithpp OR #standwithpp')
n <- 10000
tw <- getData(n, x)
dim(tw)
View(tw)

getwd()
setwd('C:/Users/john_allen/Documents/cofc')
write.csv(tw,file='twitterdata.csv')



#clean the tweets
for(j in 1:nrow(g)){
  str<-as.character(g[j,5])
  str <- gsub('[[:punct:]]','',str)
  str<-replace_emoji_identifier(str,emoji_dt = lexicon::hash_emojis_identifier)
  str<-replace_emoji(str,emoji_dt = lexicon::hash_emojis)
  str<-replace_emoticon(str, emoticon_dt = lexicon::hash_emoticons)
  str<-gsub("<...","",str)
  str<-gsub(" http.*","",str)
  g[j,5]<-str
}