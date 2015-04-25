##This file first obtains the html code behind a list of websites. It then
##cleans the text and outputs a csv with the websites and the corresponding
##text.

setwd('C:\\Users\\mdrub_000\\Desktop\\Data Science Project')

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
urls <- urls[1:5]
head(urls)

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

for(i in 1:length(text)) {
  text[i] <- gsub('\\t', ' ', text[i], perl=TRUE)
  text[i] <- gsub('\\r', ' ', text[i], perl=TRUE)
}

htmlscrape <- cbind(urls, text)
write.table(htmlscrape, 'htmlscrape_r.csv', row.names=F, sep=',')

