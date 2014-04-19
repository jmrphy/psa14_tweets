### Analysis of #PSA14 Tweets from the UK Political Studies Conference of 2014
### Justin Murphy
### jmrphy.net
### @jmrphy

# This script borrows liberally from Gaston Sanchez
# (https://github.com/gastonstat/Mining_Twitter/tree/master/Rscripts),
# Michael Bommarito (http://bommaritollc.com/), and probably others.
# Sentiment lexicons come from Hu and Liu
# (http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html)
# And most of the heavy lifting is done by Jeff Gentry's twitteR package for R
# (http://cran.r-project.org/web/packages/twitteR/twitteR.pdf).

# load packages
library(twitteR)
library(igraph)
library(stringr)
library(RJSONIO)
require(ROAuth)
require(lubridate)
require(ggplot2)
require(tm)
require(Snowball)
require(RColorBrewer)


# These two lines below download the tweets (1500 max) and convert to dataframe
# Only use if you want to download tweets from scratch (results will be different than mine)
# dm_tweets = searchTwitter("#psa14", n=1500, lang="en")
# df<-twListToDF(dm_tweets)

# Start below this point if you're starting with my downloaded tweets
# 1. Download psa_tweets.csv
# 2. Download dm_tweets.Rdata
# Set R's working directory to whatever directory holds these files

# Loads the data into memory
df<-read.csv("psa_tweets.csv")
load("dm_tweets.Rdata")

# Turn the time stamp into a recognized data format
df$time<-as.POSIXct(df$created)

###############################################################################
# Make a bar plot time-series
###############################################################################
time.series<-ggplot(data=df, aes(x=time)) +
  geom_bar(aes(fill=..count..), binwidth=30*60) +
  theme_bw() + ylab("# of Tweets") + xlab("Time") +
  labs(y="# of Tweets", x="Time", title="Time-Series of #PSA14 Tweets from the 2014 Conference of the Political Studies Association")

###############################################################################
# Make a bar plot of tweet frequencies by account (for all who tweeted more than 10 times)
###############################################################################
counts<-as.data.frame(table(df$screenName))
counts<-subset(counts, Freq>10)
frequencies<-ggplot(data=counts) +
  geom_bar(aes(x=reorder(Var1, Freq), y=Freq)) +
  labs(x="Number of Tweets", y="Username", title="Number of Tweets on #PSA14 by Author") +#PSA14") +
  theme_bw() +
  coord_flip()

###############################################################################
# Make bar plot of how many tweets were retweeted (greater than 5)
###############################################################################
df$text=sapply(df$text,function(row) iconv(row,to='UTF-8')) #remove odd characters
trim <- function (x) sub('@','',x) # remove @ symbol from user names
# extract who's been retweeted by whom
df$rt=sapply(df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))
sum(!is.na(df$rt)) # see how many tweets are retweets
sum(!is.na(df$rt))/length(df$rt) # the ratio of retweets to tweets
countRT<-table(df$rt)
countRT<-sort(countRT)
countRT.subset=subset(countRT,countRT>5) # subset those RT’d more than 5 times
countRT.subset.df<-data.frame(people=as.factor(unlist(dimnames(countRT.subset))),RT_count=as.numeric(unlist(countRT.subset)))

retweets<-ggplot(countRT.subset.df, aes(reorder(people,RT_count),RT_count)) +
  xlab("Author") + ylab("Number of tweets retweeted by others") +
  labs(title="Number of Tweets Retweeted on #PSA14 by Author") +
  geom_bar() + coord_flip() + theme_bw() +
  opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) +
  opts(axis.title.y=theme_text(size = 14, angle=90))

###############################################################################
# Make bar plot of retweets as ratio of all tweets (for those who tweeted more than 5 times)
###############################################################################
t<-as.data.frame(table(df$screenName)) # make table with counts of tweets per person
rt<-as.data.frame(table(df$rt)) # make table with counts of retweets per person
t.rt<-merge(t,rt,by="Var1") # combine tweet count and retweet count per person
t.rt["ratio"]<-t.rt$Freq.y / t.rt$Freq.x # creates new col and adds ratio tweet/retweet
sort.t.rt<-t.rt[order(t.rt$ratio),] # sort it to put names in order by ratio
sort.t.rt.subset<-subset(sort.t.rt,sort.t.rt$Freq.y>5) # exclude those with less than 5 tweets
sort.t.rt.subset.drop<-droplevels(sort.t.rt.subset) # drop unused levels that got in there

ratios<-ggplot(sort.t.rt.subset, aes(reorder(Var1,ratio),ratio)) +
  xlab("Author") + ylab("Retweets as a ratio of total tweets") +
  labs(title="Ratio of Retweets to Total Tweets on #PSA14 by Username") +
  geom_bar() + coord_flip() + theme_bw() +
  opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) +
  opts(axis.title.y=theme_text(size = 14, angle=90))

###############################################################################
### Bar plot of most popular links (those retweeted at least 2 times) #########
###############################################################################
df$link=sapply(df$text,function(tweet) str_extract(tweet,("http[[:print:]]+"))) # creates new field and extracts the links contained in the tweet
df$link=sapply(df$text,function(tweet) str_extract(tweet,"http[[:print:]]{16}")) # limits to just 16 characters after http so I just get the shortened link. They are all shortened, so this is fine, but there might be a better way using regex.
countlink<-table(df$link) # get frequencies of each link
countlink<-sort(countlink) # sort them
countlink<-data.frame(table(na.omit((df$link))))
countlink<-subset(countlink,countlink$Freq>3) # exclude those with 300 tweets or less

links<-ggplot(countlink, aes(reorder(Var1, Freq), Freq)) +
  geom_bar() + coord_flip() + theme_bw() +
  xlab("Link") + ylab("Frequency") +
  labs(title="Most Shared Links on #PSA14") +
  opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) +
  opts(axis.title.y=theme_text(size = 14, angle=90))

#################################################################################
### Make retweet network (borrowed from Gaston Sanchez, see link above) #########
#################################################################################

# get text
dm_txt = sapply(dm_tweets, function(x) x$getText())

# Let's Identify retweets
# regular expressions to find retweets
grep("(RT|via)((?:\\b\\W*@\\w+)+)", dm_tweets, 
     ignore.case=TRUE, value=TRUE)

# which tweets are retweets
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", 
                   dm_txt, ignore.case=TRUE)

# show retweets (these are the ones we want to focus on)
dm_txt[rt_patterns] 

# Collect who retweeted and who posted
# We'll use these results to form an edge list in order to create the graph
# create list to store user names
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))

# for loop
for (i in 1:length(rt_patterns))
{ 
  # get tweet with retweet entity
  twit = dm_tweets[[rt_patterns[i]]]
  # get retweet source 
  poster = str_extract_all(twit$getText(),
                           "(RT|via)((?:\\b\\W*@\\w+)+)") 
  #remove ':'
  poster = gsub(":", "", unlist(poster)) 
  # name of retweeted user
  who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  # name of retweeting user 
  who_retweet[[i]] = rep(twit$getScreenName(), length(poster)) 
}

# unlist results
who_post = unlist(who_post)
who_retweet = unlist(who_retweet)

# Create graph from an edglist
# two column matrix of edges
retweeter_poster = cbind(who_retweet, who_post)

# generate graph
rt_graph = graph.edgelist(retweeter_poster)

# Plot graph

rt_graph2 = delete.vertices(rt_graph, V(rt_graph)[ degree(rt_graph)==0 ])
V(rt_graph2)$label = NA
V(rt_graph2)$color =  rgb(1,0,0,.1)
V(rt_graph2)$size = 2 

E(rt_graph2)$width = .3
E(rt_graph2)$color = rgb(.5,.5,0,.1)

# par(bg="white", mar=c(1,1,1,1))
# plot.igraph(rt_graph2, layout=layout.fruchterman.reingold, main="#PSA14 Retweet Network")

### Calculate and Analyze Network Stats

cent<-data.frame(bet=betweenness(rt_graph),eig=evcent(rt_graph)$vector)
res<-lm(eig~bet,data=cent)$residuals
cent<-transform(cent,res=res)
set.seed(1405)
key.accounts<-ggplot(cent,aes(x=bet,y=eig,
                              label=rownames(cent),colour=res)) +
  xlab("Betweenness Centrality") + 
  ylab("Eigenvector Centrality") +
  labs(title="Key Twitter Accounts on #PSA14") +
  theme_bw() +
  geom_text(position = position_jitter(width = 2000))


#################################################################################
### Text #########
#################################################################################

df$cleantext<-gsub('http.* *', ' ', df$text)
df$cleantext<-str_replace_all(df$cleantext, "[^[:alnum:]]", " ")
corp<-Corpus(VectorSource(df$cleantext))
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, stripWhitespace)

my_stopwords <- c(stopwords('english'), paste(unique(df$screenName)))
corp <- tm_map(corp, removeWords, my_stopwords)


corp <- tm_map(corp, tolower)

corp.full<-corp

corp <- tm_map(corp, stemDocument, language = "english") #reduce all English words to their roots

other_stop_words<-c("psa", "polit", "rt", "panel", "amp", "politicalspik", "angeliawilson",
                    "politicsir", "peterjohn", "the")
corp <- tm_map(corp, removeWords, other_stop_words)   # remove specific word (the hashtag of interest)

dtm <-DocumentTermMatrix(corp) # make a matrix of each document by every single term
dtm <- removeSparseTerms(dtm, 0.99)

#set.seed(666)
#wss <- (nrow(dtm)-1)*sum(apply(dtm,2,var), na.rm=TRUE)
#for (i in 2:200) wss[i] <- sum(kmeans(dtm,
#                                     centers=i)$withinss)
#wss<-as.data.frame(wss)
#wss$numbers<-1:200
#k.diagnosis<-ggplot(wss, aes(x=numbers, y=wss)) +
#                      geom_line() +
#                      theme_bw() +
#                      labs(title="Within group sum of squares by number of clusters",
#                           x="Number of clusters",
#                           y="Within groups sum of squares")

#wss$numbers[diff(wss$wss)>=100]

set.seed(666) # very satanic
kclusters <- kmeans(dtm, centers=10)
for (i in 1:length(kclusters$withinss)) {       
  #For each cluster, this defines the documents in that cluster
  inGroup <- which(kclusters$cluster==i)
  within <- dtm[inGroup,]
  if(length(inGroup)==1) within <- t(as.matrix(within))
  out <- dtm[-inGroup,]
  words <- apply(within,2,mean) - apply(out,2,mean) #Take the difference in means for each term
  print(c("Cluster", i), quote=F)
  labels <- order(words, decreasing=T)[1:20] #Take the top 20 Labels
  print(names(words)[labels], quote=F) #From here down just labels
}


tdm <-TermDocumentMatrix(corp) # make a matrix of each document by every single term
tdm <- removeSparseTerms(tdm, 0.985)
m = as.matrix(tdm)
# create a matrix 'good' with most frequent words
# word counts
wc = rowSums(m)
# get those words above the 3rd quantile
lim = quantile(wc, probs=0.5)
good = m[wc > lim,]

# remove columns (documents) with zeros
good = good[,colSums(good)!=0]

# Obtain an adjacency matrix and create a graph
M = good %*% t(good)

# set zeros in the diagonal
diag(M) = 0

# create a graph based on the adjacency matrix
g = graph.adjacency(M, weighted=TRUE, mode="undirected",
                    add.rownames=TRUE)
# get a graph layout (eg fruchterman.reinglod)
glay = layout.fruchterman.reingold(g)

# Prepare ingredients for plotting the network
# (you can tweak a lot of parameters in your graph)
V(g)$size = 8
V(g)$label = V(g)$name
V(g)$degree = degree(g)

# if want to plot the labels with different sizes 
# just change the label.cex parameter
V(g)$label.cex = .9 * log10(V(g)$degree)
V(g)$color =  rgb(1,0,0,.1)
V(g)$size = 5 

E(g)$width = 1
E(g)$color = rgb(.5,.5,.5,.3)
# plot network again with words in different sizes
#par(bg="white", mar=c(1,1,1,1))
#plot(g, layout=glay)
#title("\nNetwork Graph of Tweets from #PSA14",
#    col.main="gray40", cex.main=1)

### Sentiment Analysis

hu.liu.pos=scan("positive-words.txt",what='character',comment.char=';') #load +ve sentiment word list
hu.liu.neg=scan("negative-words.txt",what='character',comment.char=';') #load -ve sentiment word list
pos.words=c(hu.liu.pos)
neg.words=c(hu.liu.neg)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

corp<-unlist(corp.full)

corp.scores<-score.sentiment(corp,pos.words,neg.words,.progress='text') # get scores for the tweet text 

sentiment.plot<-ggplot(corp.scores, aes(x=score)) + geom_histogram(binwidth=1) + xlab("Sentiment score") + ylab("Frequency") + theme_bw()  + opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) + opts(axis.title.y=theme_text(size = 14, angle=90, vjust = .2)) +
  labs(title="The Distribution of Sentiment on #PSA14") # plots nice histogram

corp.pos<-subset(corp.scores,corp.scores$score>=4) # get tweets with only very +ve scores
corp.neg<-subset(corp.scores,corp.scores$score<=-4) # get tweets with only very -ve scores

names(corp.scores)<-c("sentiment", "reduced.text")

df<-cbind(df, corp.scores)

df$hour<-hour(df$time)
sentiment.hour<-aggregate(df$sentiment, by=list(df$hour), mean)
names(sentiment.hour)<-c("Group.1", "Sentiment")
sentiment.hour$hour.of.day<-as.factor(sentiment.hour$Group.1)
sentiment.day.plot<-ggplot(data=sentiment.hour, aes(x=hour.of.day, y=Sentiment)) + geom_bar(aes(fill=Sentiment)) + theme_bw() + labs(x="Hour of Day", y="Mean Sentiment Score", title="Mean Sentiment of #PSA14 Tweets by Hour of Day")

df$day<-day(df$time)
df$day.hour<-substr(df$time, 1, 13)
sentiment.day<-aggregate(df$sentiment, by=list(df$day.hour), mean)
names(sentiment.day)<-c("day.hour", "sentiment")

df$counter<-1
day.counts<-aggregate(df$counter, by=list(df$day.hour), sum)
names(day.counts)<-c("Group1", "Count")

sentiment.day<-cbind(sentiment.day, day.counts)
sentiment.day$weighted.sent<-sentiment.day$sentiment*sentiment.day$Count

sentiment.time<-ggplot(data=sentiment.day, aes(x=day.hour, y=sentiment)) +
  geom_bar(aes(fill=Count)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  labs(y="Mean Sentiment Score", x="Hour",
       title="Sentiment Score of #PSA14 Tweets by the Hour")

sentiment.time.weighted<-ggplot(data=sentiment.day, aes(x=day.hour, y=weighted.sent)) +
  geom_bar(aes(fill=Count)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70, hjust = 1)) +
  labs(y="Mean Sentiment Score Weighted by Tweet Count", x="Hour",
       title="Weighted Sentiment Score of #PSA14 Tweets by the Hour")


