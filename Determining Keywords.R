##This script creates a dataframe with all the n-grams (1-3) contained
##in the text and the counts associated with each website. It then subsets
##sparse terms, found in less than 0.5% of all observations, and uses a 
##chi-squared test to further subsets the n-grams are most and least associated
##with healthcare websites. These ngrams are then chosen as keywords to use
##to run different classification models to help categorize whether a company
##does work in healthcare or not.

setwd('C:\\Users\\mdrub_000\\Desktop\\Data Science Project')

options(java.home="C:\\Program Files\\Java\\jre1.8.0_40")
options(java.parameters = "-Xmx10000m")
library(rJava)
library(RWeka)
library(tm)
library(glmnet)

htmlscrape <- read.csv('htmlscrape_r.csv')
text <- htmlscrape[,2]

myCorpus <- Corpus(VectorSource(text))
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

dtm <- removeSparseTerms(dtm, 0.995) ##removing sparse terms
mydtm_df <- data.frame(as.matrix(dtm))
write.csv(mydtm_df, 'dtm.csv', row.names=F, sep=',')

a <- scrape[,c('Website', 'healthcat')]
a$id <- c(1:nrow(a))
mydtm_df$id <- c(1:nrow(a))
final <- merge(a, mydtm_df, by='id')
final <- final[grep('FALSE', duplicated(final$Website)),]

#####CHI SQUARED SHIT

nonhealth <- final[final$healthcat==0, -c(1,2,3)]
health <- final[final$healthcat==1, -c(1,2,3)]

nhcount <- sapply(nonhealth, sum, 2)
hcount <- sapply(health, sum, 2)

fin1 <- rbind(nhcount, hcount)
fin1 <- as.data.frame(fin1)
fin1 <- t(fin1)

library(MASS)
chi <- chisq.test(fin1)
res <- chi$stdres
res[order(res[,2]),]

quantile(res[,1], c(0.05,0.95))[2]
quantile(res[,2], c(0.05,0.95))[2]

resselect <- res[res[,1]>quantile(res[,1], c(0.05,0.95))[2]
                 | res[,2]> quantile(res[,2], c(0.05,0.95))[2],]
resselect <- t(resselect)
fingrams <- colnames(resselect)

fin2 <- mydtm_df[,fingrams]
fin2$id <- c(1:nrow(fin2))

final <- merge(a, fin2, by='id')
final <- final[grep('FALSE', duplicated(final$Website)),]

names(final[1:5])

###LASSO REGRESSION
grid = 10^seq(10, -2, length=100) ##set lambda parameters
set.seed(235)
train=sample(1:nrow(final), nrow(final)/2) ##create training data rows
test = (-train) ##create test data rows

y.test = final$healthcat[test]
x.test = as.matrix(final[test,-c(1,2,3)])

y.train = final$healthcat[-test]
x.train = as.matrix(final[-test,-c(1,2,3)])

lasso.mod=glmnet(x.train, y.train, alpha=1, lambda=grid)

set.seed(46)
cv.out = cv.glmnet(x.train, y.train, alpha=1) ##cross-validate to determine
##the best lambda
plot(cv.out)
bestlam=cv.out$lambda.min

##Using Tuning Parameter on Test Data
lasso.pred=predict(lasso.mod, s=bestlam, newx=x.test)
out=glmnet(x.test, y.test, alpha=1, lambda=grid)
lasso.coef=predict(out, type='coefficients', s=bestlam)

cv.out ##gives you the mse and lambda of model on training data
min(cv.out$cvm)

out1=glmnet(x.test, y.test, alpha=1, lambda=bestlam)
out1$dev.ratio ##Test Deviance 

#Out of Sample Correlation
cor.test(lasso.pred, y.test) 
yhat <- ifelse(lasso.pred>0.5,1,0)
table(yhat, y.test)
cor.test(yhat, y.test)

values <- as.numeric(lasso.coef[lasso.coef!=0])
names <- rownames(lasso.coef)
names <- names[grep("TRUE", as.vector(lasso.coef) %in% values)]

mylist <- cbind(values, names)
mylist <- as.data.frame(mylist)
mylist <- mylist[order(values),]

write.table(mylist, 'keywords.csv', row.names=F, sep=',')