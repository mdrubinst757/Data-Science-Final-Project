##This file first obtains the html code behind a list of websites. It then
##cleans the text and outputs a csv with the websites and the corresponding
##text.

setwd('C:\\Users\\mdrub_000\\Desktop\\Github\\Data-Science-Final-Project')

options(java.home="C:\\Program Files\\Java\\jre1.8.0_40")
options(java.parameters = "-Xmx10000m")
library(rJava)
library(RWeka)
library(tm)
library(glmnet)

################################################
###########N-GRAMS SCRAPER######################
################################################

scrape <- read.csv('scrape.csv')
urls <- scrape[,c(1)]

library(RCurl)
library(XML)

text = c()
for(i in 1:length(urls)){
  test <- try(getURL(urls[i]), silent=TRUE)
  if(inherits(test,'try-error')){text = c(text, 'NA'); next} else{
  doc <- try(htmlParse(test, asText = TRUE), silent=TRUE)
  if(inherits(doc, 'try-error')) {text = c(text, 'NA'); next} else { 
  plain.text <- try(xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue), silent=TRUE)
  if(inherits(plain.text, 'try-error')) {text = c(text, 'NA'); next} else{
  plain.text <- paste(plain.text, collapse = " ")
  text = c(text, plain.text)
}
}
}
  }

text1 <- as.data.frame(text)
text1$url <- urls

fix(text1) ##Look for websites that didn't work (permission denied,
           ##website moved, blank, etc.), mark as 1
text2 <- text1[-grep('1', text1$truena),] ##Remove failed webscrapes

#################################################
##############CLEANING THE HTML DATA#############
#################################################

text2t <- text2$text
urls <- text2$url

myCorpus <- Corpus(VectorSource(text2t))
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english'))
myCorpus <- tm_map(myCorpus, stemDocument)
myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))

GramTokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min=1, max=3))
}

dtm <- DocumentTermMatrix(myCorpus, 
                          control=list(tokenize=GramTokenizer))

dtm <- removeSparseTerms(dtm, 0.995) ##Removing sparse terms
mydtm_df <- data.frame(as.matrix(dtm))
mydtm_df$url <- urls

write.csv(mydtm_df, 'dtm.csv', row.names=F, sep=',')
