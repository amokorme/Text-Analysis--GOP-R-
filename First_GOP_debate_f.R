
#Read the data from object storage.
debate<- read.csv("~/project-objectstorage/First_GOP_Debate.csv", stringsAsFactors=FALSE)

#Checking the row counts and column counts 
dim(debate)
#number of tweets per sentiment
table(debate$SENTIMENT)

#number of tweets per candidate 
table(debate$CANDIDATE)

#number of tweets per sentiment by candidate 
table(debate$CANDIDATE, debate$SENTIMENT)

#Number of tweets by airline per negative reason
table(debate$SENTIMENT, debate$CANDIDATE)

#Pie chart for the candidate 
#Reset the margin
par(mar=c(1,1,1,1))
#Use default colors
pie (table(debate$CANDIDATE))
#Change colors
pie (table(debate$CANDIDATE), col=c("blue", "yellow", "green", "purple", "pink", "orange"))

#load the plyr package
library("plyr")
#List the most common complaints
reasonCounts<-na.omit(plyr::count(debate$CANDIDATE))
reasonCounts<-reasonCounts[order(reasonCounts$freq, decreasing=TRUE), ]
reasonCounts

# Using ggplot2 package
# Complaints frequency plot
# Load the "ggplot2" package
library(ggplot2)

# Create a data frame from reasonCounts
wf <- data.frame(reasonCounts)

# Create a ggplot object
p <- ggplot(wf, aes(x, freq, fill = x)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add color palette using scale_fill_*
p <- p + scale_fill_brewer(palette = "Set1")

# Display the plot
p
#Number of retweets per negative reason
ddply(debate, ~ SENTIMENT, summarize, numRetweets = sum(RETWEET_COUNT, na.rm = TRUE))

#Posts that have 10 retweets
as.character(subset(debate, RETWEET_COUNT ==10)$TEXT)

#number of posts per day
posts<-as.Date(debate$TWEET_CREATED, tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y"),optional = FALSE)
table(posts)
#day with the maximum number of posts
table(posts)[which.max(table(posts))]


#Apriori rules method
library (arules)
dfa<-debate[ , c('CANDIDATE', 'SENTIMENT', 'SUBJECT_MATTER', 'RETWEET_COUNT', 'USER_TIMEZONE')]
dfa$CANDIDATE<-as.factor(dfa$CANDIDATE)
dfa$SENTIMENT<-as.factor(dfa$SENTIMENT)
dfa$SUBJECT_MATTER<-as.factor(dfa$SUBJECT_MATTER)
dfa$USER_TIMEZONE<-as.factor(dfa$USER_TIMEZONE)

dfa$RETWEET_COUNT<-cut(dfa$RETWEET_COUNT, breaks=c(0, 1, 2, Inf), right=F, labels=c("0", "1",  "2+"))

rules<-apriori(dfa)
arules::inspect(rules[1:2])

library(arulesViz)
plot(rules, method="graph", alpha=1, cex=0.9)
summary(rules)
#Text Mining
#install tm, wordcloud, SnowballC for stemming.  Do it only once.
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")

#Load the packages in memory
library("tm")
library("wordcloud")
library ("SnowballC")

#Load the tweets with positive sentiment into the data frame positive
positive<-debate[debate$SENTIMENT=='Positive', c('TEXT')]

docs<-VectorSource(positive)
docs<-Corpus(docs)
inspect(docs[[1]])
inspect(docs[[2]])
inspect(docs[[20]])

#Strip the white space
docs <- tm_map(docs, stripWhitespace)

#Remove the URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
docs <- tm_map(docs, content_transformer(removeURL))

#Remove non ASCII character
removeInvalid<-function(x) gsub("[^\x01-\x7F]", "", x)
docs <- tm_map(docs, content_transformer(removeInvalid))

#Remove punctuation
docs <- tm_map(docs, removePunctuation)

#remove the numbers
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, tolower)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "@")   #Remove @
docs <- tm_map(docs, toSpace, "RT")   #Remove @
docs <- tm_map(docs, toSpace, "/")   #Remove /
docs <- tm_map(docs, toSpace, "\\|") #Remove |
docs <- tm_map(docs, toSpace, "gopdeb") #Remove gopdeb
docs <- tm_map(docs, toSpace, "gop") #Remove gop
docs <- tm_map(docs, toSpace, "didnt") #Remove didnt
docs <- tm_map(docs, toSpace, "ate") #Remove ate
docs <- tm_map(docs, toSpace, "night") #Remove night
docs <- tm_map(docs, toSpace, "amp") #Remove amp
docs <- tm_map(docs, toSpace, "deb") #Remove deb
docs <- tm_map(docs, toSpace, "RT") #Remove RT
#Remove the stop word
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, stopwords("SMART"))

docs <- tm_map(docs, stemDocument)

#Remove the white space introduced during the pre-processing
docs <- tm_map(docs, stripWhitespace)
dtm <- DocumentTermMatrix(docs)
#
dtm <- removeSparseTerms(dtm, 0.99)
#
m <- as.matrix(dtm)   #Convert dtm to a matrix
dim(m)                # Display number of terms and number of documents
View(m[1:10, 1:10])   # Preview the first 10 rows and the first 10 columns in m

#find the terms that appear at least 100 times
findFreqTerms(dtm, lowfreq=100)
#find the terms asosciated with trump and rwsurfergirl with correlation at least 0.15
findAssocs(dtm, c("trump", "rwsurfergirl"), corlimit=0.15)

dtms <- removeSparseTerms(dtm, 0.6) # Prepare the data (max 60% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
#plot the word cloud for the sentiment 
wordcloud(words = debate$SENTIMENT, freq = freq, min.freq = 50,          
          max.words=100, random.order=FALSE, rot.per=0.35,            
          colors=brewer.pal(8, "Dark2"))#
###################################################
# Terms Cluster dendogram
library("cluster")

library("dendextend")

dtms <- removeSparseTerms(dtm, 0.98)    #Remove sparse terms
d <- dist(t(dtms), method="euclidian")  #Build the dissimilarity matrix
fit <- hclust(d=d, method="ward.D2")

# Create a dendrogram object
dend <- as.dendrogram(fit)

# Cut the dendrogram into clusters
clusters <- cutree(fit, k = 3)

# Color the branches based on clusters
colored_dend <- color_branches(dend, clusters = clusters, col = c("red", "green", "blue"))

# Plot the dendrogram
plot(colored_dend, main = "Cluster Dendrogram", xlab = "Candidates", ylab = "Distance")

#End