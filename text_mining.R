install.packages("twitteR")
install.packages("ROAuth")
install.packages("tm")
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("plyr")
install.packages("gridExtra")
library("ggplot2")
library("plyr")
library("gridExtra")
library("wordcloud")
library("twitteR")
library("ROAuth")
library("tm")
library("dplyr")
# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

#create an object to save the authenticated object that we can use for later sessions
cred <- OAuthFactory$new(consumerKey='xxxxxxxxxxxxxxxx',
                         consumerSecret='xxxxxxxxxxxxxxx',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

# Executing the next step generates an output --> To enable the connection, please direct your web browser to: <hyperlink> . Note:  You only need to do this part once
cred$handshake(cainfo="cacert.pem")

#save for later use for Windows
save(cred, file="twitter_auth.Rdata")
load("twitter_auth.Rdata")
setup_twitter_oauth('xxxxxxxxxxxx','xxxxxxxxxxxx', 'xxxxxxxx', 'xxxxxxxxxxxx')

search.string <- "#Amazongo"
searchterm <- "amazongo1"
no.of.tweets <- 3000

tweets <- searchTwitter(searchterm, n=no.of.tweets, lang="en")
tweets

tweets.text <- sapply(tweets, function(x) x$getText())

# Clean the tweets
tweets.text <- gsub("rt", "", tweets.text)
tweets.text <- gsub("@\\w+", "", tweets.text)
tweets.text <- gsub("[[:punct:]]", "", tweets.text)
tweets.text <- gsub("http\\w+", "", tweets.text)
tweets.text <- gsub("[ |\t]{2,}", "", tweets.text)
tweets.text <- gsub("^ ", "", tweets.text)
tweets.text <- gsub(" $", "", tweets.text)
Encoding(tweets.text) <- "latin1"
tweets.text <- iconv(tweets.text, "latin1", "ASCII", sub="")
tweets.text <- tolower(tweets.text)

#create corpus
tweets.text.corpus <- Corpus(VectorSource(tweets.text))

#clean up by removing stop words
tweets.text.corpus <- tm_map(tweets.text.corpus, function(x)removeWords(x,stopwords()))

#generate wordcloud
wordcloud(tweets.text.corpus,min.freq = 2, scale=c(7,0.5),colors=brewer.pal(8, "Dark2"),  random.color= TRUE, random.order = FALSE, max.words = 150)


#tweets scoring function
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub("http\\w+", "", sentence)
    sentence <- gsub("[ |\t]{2,}", "", sentence)
    sentence <- gsub("^ ", "", sentence)
    sentence <- gsub(" $", "", sentence)
    sentence <- gsub("rt", "", sentence)
    sentence <- gsub("@\\w+", "", sentence)
    Encoding(sentence) <- "latin1"
    sentence <- iconv(sentence, "latin1", "ASCII", sub="")
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

pos.words = scan(file="positive-words.txt",what="charcter", comment.char=";")
neg.words = scan(file="negative-words.txt",what="charcter", comment.char=";")


df <- twListToDF(tweets)
df <- df[, order(names(df))]
df$created <- strftime(df$created, '%Y-%m-%d')
if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
#merge last access with cumulative file and remove duplicates
stack <- read.csv(file=paste(searchterm, '_stack.csv'))
stack <- rbind(stack, df)
stack <- subset(stack, !duplicated(stack$text))
write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)

Dataset <- stack
Dataset$text <- as.factor(Dataset$text)
scores <- score.sentiment(Dataset$text, pos.words, neg.words, .progress='text')
write.csv(scores, file=paste(searchterm, '_scores.csv'), row.names=TRUE) #save evaluation results into the file


stat <- scores
stat$created <- stack$created
stat$created <- as.Date(stat$created)
#stat <- mutate(stat, tweet=ifelse(stat$score > 0, 'positive', ifelse(stat$score < 0, 'negative', 'neutral')))
stat <- mutate(stat, tweet=ifelse(stat$score >= 0, 'positive', 'negative'))
by.tweet <- group_by(stat, tweet, created)
by.tweet <- summarise(by.tweet, number=n())
write.csv(by.tweet, file=paste(searchterm, '_opin.csv'), row.names=TRUE)

p <- ggplot(by.tweet, aes(created, number)) + geom_line(aes(group=tweet, color=tweet), size=1) +
  geom_point(aes(group=tweet, color=tweet), size = 1) +
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90, vjust=1))
  ggtitle(searchterm)
ggsave(file=paste(searchterm, '_plot.jpeg'))
