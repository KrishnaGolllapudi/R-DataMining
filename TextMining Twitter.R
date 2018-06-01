install.packages('twitterR', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("installr") # install installr
library(installr) #load
updateR()
install.packages('tm')
install.packages('wordcloud')
install.packages('RCurl')
install.packages('XML')


rdmTweets<-load("C:/Users/gvsva/Desktop/R DataMining/rdmTweets.RData")
rdmTweets[11:15]


install.packages(c("devtools", "rjson", "bit64", "httr"))
library(devtools)
install_github("geoffjentry/twitteR")
install.packages("twitterR")

consumerKey <- "00a8bI7h73NufHWvaSMmHhMTl"
consumersecret<-"pdUm3jiLo3ZhOxohcPjB5UJe8VYn9FqKGMFfNUeau9HNuDxypu"
accesstoken<-"972137313915539457-9nR2cAKhlsQ5lABOdJ8zvoOV6gZ3Zgi"
accesstokensecret<-"17ws0sd4vx1LuTDwzgLglyoSE9RRGr8rMS1wh9w4JyS5p"

setup_twitter_oauth(consumerKey ,consumersecret,accesstoken ,accesstokensecret)

rdmTweets <- userTimeline("rdatamining", n=200) 
(nDocs <- length(rdmTweets))
rdmTweets[11:15]

for (i in 11:15) {cat(paste0("[[", i, "]] ", sep=""))+writeLines(strwrap(rdmTweets[[i]]$getText(), width=73))}

# convert tweets to a data frame
df <- twListToDF(rdmTweets)
dim(df)

library(tm)
# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(df$text))
myCorpus <- tm_map(myCorpus, tolower)   

#remove url
# remove URLs 
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) 

# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x) 
myCorpus <- tm_map(myCorpus, removePunctuation) 
myCorpus <- tm_map(myCorpus, removeNumbers) 

# add two extra stop words: "available" and "via"
myStopwords <- c(stopwords('english'), "available", "via") 
# remove "r" and "big" from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big")) 
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords) 
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, removeNumbers)

#Stemming
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)

for (i in 11:15) {  cat(paste("[[", i, "]] ", sep="")) + writeLines(strwrap(myCorpus[[i]], width=73))  }

for (i in 11:15) {  cat(paste0("[", i, "] ")) + writeLines(strwrap(as.character(myCorpus[[i]]), 60))  }

stemCompletion2 <- function(x, dictionary) { 
                      x <- unlist(strsplit(as.character(x), " ")) 
                      # Unexpectedly, stemCompletion completes an empty string to
                      # a word in dictionary. Remove empty string to avoid above issue. 
                      x <- x[x != ""] 
                      x <- stemCompletion(x, dictionary=dictionary) 
                      x <- paste(x, sep="", collapse=" ")
                      PlainTextDocument(stripWhitespace(x)) } 

myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy) 
myCorpus <- Corpus(VectorSource(myCorpus))
inspect(myCorpus[11:15])

# count frequency of "mining" 
miningCases <- lapply(myCorpusCopy,  function(x) { grep(as.character(x), pattern = "\\<mining")} ) 
sum(unlist(miningCases))

# count frequency of "miner" 
minerCases <- lapply(myCorpusCopy,  function(x) {grep(as.character(x), pattern = "\\<miner")} ) 
sum(unlist(minerCases))

# replace "miner" with "mining"
myCorpus <- tm_map(myCorpus, content_transformer(gsub),  pattern = "miners", replacement = "mining")

#Building a Term-Document Matrix
tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf))) 
tdm

idx <- which(dimnames(tdm)$Terms == "r") 
inspect(tdm[idx+(0:5),101:110])
tdm <- TermDocumentMatrix(myCorpus, control=list(minWordLength=1))
rownames(tdm)

#frequent words
findFreqTerms(tdm, lowfreq=10)
termFrequency <- rowSums(as.matrix(tdm)) 
termFrequency <- subset(termFrequency, termFrequency>=10) 
install.packages('ggplot2')
library(ggplot2) 
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip()
barplot(termFrequency, las=2)

# which words are associated with "r"? 
findAssocs(tdm, "list", 0.25)

#Word Cloud
install.packages('wordcloud')
install.packages('RColorBrewer')
library(wordcloud)
m<-as.matrix(tdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE) 
# colors 
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)] 
# word cloud 
set.seed(375) # to make it reproducible 
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F,  colors=pal)

#Clustering Words
# remove sparse terms 
tdm2 <- removeSparseTerms(tdm, sparse=0.95) 
m2 <- as.matrix(tdm2)
# cluster terms
distMatrix <- dist(scale(m2)) 
fit <- hclust(distMatrix, method="ward.D")
plot(fit)
# cut tree into 10 clusters 
rect.hclust(fit, k=10) 
(groups <- cutree(fit, k=10))

#Clustering Tweets
#Clustering Tweets with the k-means Algorithm
# transpose the matrix to cluster documents (tweets) 
m3 <- t(m2) 
# set a fixed random seed 
set.seed(122) 
# k-means clustering of tweets 
k <- 8 
kmeansResult <- kmeans(m3, k) 
# cluster centers 
round(kmeansResult$centers, digits=3)

#we then check the top three words in every cluster.
s <- sort(kmeansResult$centers[i,], decreasing=T)
for (i in 1:k) {  cat(paste("cluster ", i, ": ", sep="")) + s  + cat(names(s)[1:3], "\n")}

#Clustering Tweets with the k-medoids Algorithm
install.packages('fpc')
library(fpc)
# partitioning around medoids with estimation of number of clusters 
pamResult <- pamk(m3, metric="manhattan")
# number of clusters identified
(k <- pamResult$nc)
pamResult <- pamResult$pamobject 
for (i in 1:k) {  
    cat(paste("cluster", i, ": "))
    cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n") 
    }
pamResult
# plot clustering result 
layout(matrix(c(1,2),2,1)) # set to two graphs per page    
plot(pamResult, color=F, labels=4, lines=0, cex=.8, col.clus=1,  col.p=pamResult$clustering)
layout(matrix(1)) # change back to one graph per page
pamResult2 <- pamk(m3, krange=2:8, metric="manhattan")
