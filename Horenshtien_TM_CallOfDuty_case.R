#' Title: Call Of Duty Case
#' Purpose: Marketing and business analytics on Tweetes from Twitter related to Call of Duty Leauge
#' NAME: Atalia Horenshtien
#' Date: Jan 23 2021
#' 

# Pre processing: 

## setup

### working directory
setwd("~/Documents/MsBA/textAnalytics/Cases/text_mining_callOfDutyLeague/Data")

### Load the needed libraries
library(tm)
library(qdap)
library(plyr)
library(pbapply)
library(stringr)
library(stringi)
library(fst)
library(tidytext)

### Support functions and data objects

source('~/Documents/MsBA/textAnalytics/Cases/text_mining_callOfDutyLeague/Z_otherScripts/ZZZ_supportingFunctions.R')
emoji <- read.csv('emojis.csv')

## read and filter the data

### read the data 

#### tweets among the 13 teams
Tweets <- read_fst('student_TeamTimelines.fst') 
org_num_rows <- nrow(Tweets)

### filter the data 

#### the top 3 COD teams I will focus on, based on the number of followers
top3_COD_teams <- c('Immortals', 'OpTicCHI', 'LAThieves')
top3_COD_teams_full_name <- c('Immortals', 'Optic Chicago', 'LA Thieves')

#### Create custom stop words
stops <- c(stopwords('SMART'), 'nba', tryTolower(top3_COD_teams), unlist(strsplit(tryTolower(top3_COD_teams_full_name), " "), use.names=FALSE), 
           'amp', 'game', 'games', 'team', 'sports', 'team', 'teams', 'play', 'playing', 'player', 'points', 'fans', 'move',
           'time', 'years', 'year', 'thinking', 'the', 'season')


#### filter the data for only the top top3_COD_teams
Tweets <- subset(Tweets, screen_name %in% top3_COD_teams)
#### 17.6% from the tweets are represented by the top 3 teams
nrow(Tweets)/org_num_rows
write.csv(Tweets,'filteredTweets.csv')

# Clean and Organize
TweetsClean <- cleanCorp('filteredTweets.csv',
                         'text',
                         collapse        = F, 
                         customStopwords = stops)

TweetsDTM <- cleanMatrix(txtCorpus = TweetsClean, 
                         type            = 'DTM', 
                         wgt             = 'weightTf')

# switch back to DTM because TweetsDTM is a Matrix
TweetsDTM <- as.DocumentTermMatrix(TweetsDTM, weighting = weightTf ) 
tidyTweets <- tidy(TweetsDTM)


# Sentiments setup

## Load the needed libraries
library(lexicon)
library(dplyr)

## Lexicons

###Bing
### get bing lexicon - The Bing lexicon uses a binary categorization model that sorts words into positive or negative positions.
bing <- get_sentiments(lexicon = c("bing"))

### add the sentiment data to  the original data by Inner Join
bingSent <- inner_join(tidyTweets, bing, by=c('term' = 'word'))

###nrc
### get nrc lexicon - The NRC lexicon categorizes sentiment words into positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise and trust
nrc <- nrc_emotions

### Cleaning and find columns having value > 0
terms <- subset(nrc, rowSums(nrc[,2:9])!=0)
sent  <- apply(terms[,2:ncol(terms)], 1, function(x)which(x>0))

###  Reorganize the data
nrcLex <- list()
for(i in 1:length(sent)){
  x <- sent[[i]]
  x <- data.frame(term      = terms[i,1],
                  sentiment = names(sent[[i]]))
  nrcLex[[i]] <- x
}
nrcLex <- do.call(rbind, nrcLex)

### add the sentiment data to  the original data by Inner Join
nrcSent <- inner_join(tidyTweets,nrcLex, by=c('term' = 'term'))

### reorganize nrcSentL group by document and select the most numerous 
grpSent <- nrcSent %>% group_by(document, sentiment) %>% summarise(n = sum(count))
grpSent$document <- as.numeric(as.character(grpSent$document))

### Cast the data into wide format
wideSent <- dcast(grpSent, document~sentiment,fun.aggregate = sum,value.var = "n")
wideSent[grep('\\b100\\b',wideSent$document),] 
wideSent$maxEmotion <- ifelse(rowSums(wideSent[,2:ncol(wideSent)])>0,
                              names(wideSent)[2:ncol(wideSent)][max.col(wideSent[,2:ncol(wideSent)])],
                              'noEmotion')

### Extract the clean and subbed text to use polarity 
cleanTweets <- data.frame(document = seq_along(Tweets$status_id), #simple id order
                          status_id = Tweets$status_id, # keep track of posts
                          text = unlist(sapply(TweetsClean, `[`, "content")),stringsAsFactors=F)

### build clean Tweets data setwith ID, moderators, polarity, and emotional sentiment
cleanTweets <- left_join(cleanTweets, wideSent, by = c('document'='document'))
cleanTweets$maxEmotion[is.na(cleanTweets$maxEmotion)] <- 'noEmotion' #NA introduced from join on docs that had no emotion

cleanTweets
# Visualization:

## Load the needed libraries
library(wordcloud)
library(radarchart)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(viridisLite)
library(viridis)

## Using Bing

###  Visualization 1 - bar chart representation for negative vs positive words.
sentiment_sum <- aggregate(count~sentiment,bingSent, sum)

#### the bar chart
ggplot(sentiment_sum, aes(x=sentiment, y=count)) + 
  geom_bar(stat="identity", fill='#ADD8E6') + 
  theme_gdocs() +
  geom_text(aes(label=count), colour="Black",size=3.0)

###  Visualization 2 - bar chart representation for most frequent words and their sentiment 

#### Looking for natural words - we can see there is no natural
subset(bingSent, sentiment=='natural')

#### top frequent words 
topWords <- subset(bingSent, bingSent$count >= 2) 
topWords  <- topWords[order(topWords$count, decreasing= T),]
topWords <- head(topWords, 30)

#### the bar chart
ggplot(topWords, aes(x=term, y=count, fill=sentiment)) + 
  geom_bar(stat="identity") + 
  theme_gdocs() 


## Using NCR 

###  Visualization 3 - Radar chart: The emotions among the tweets
#### the Radar chart
emos <- data.frame(table(nrcSent$sentiment))
chartJSRadar(scores = emos, labelSize = 20, showLegend = F)

###  Visualization 4 - comparison cloud: bold words per emotion category.

#### build the emotion list
emotionLst <- list()
for(i in 1:length(unique(cleanTweets$maxEmotion))){
  x <- subset(cleanTweets$text, cleanTweets$maxEmotion == unique(cleanTweets$maxEmotion)[i])
  x <- paste(x, collapse = ' ')
  emotionLst[[unique(cleanTweets$maxEmotion)[i]]] <- x
}

#### Using the list
allEmotionClasses <- do.call(rbind, emotionLst)
allEmotionClasses <- VCorpus(VectorSource(allEmotionClasses))
allEmotionClasses <- TermDocumentMatrix(allEmotionClasses)
allEmotionClasses <- as.matrix(allEmotionClasses)

#### Make sure order is the same between the two
colnames(allEmotionClasses) <- names(emotionLst)

####the comparison cloud
comparison.cloud(allEmotionClasses, 
                 max.words=700, 
                 random.order=FALSE,
                 title.size=1,
                 colors=viridis(10),
                 scale=c(3,0.1))

# End