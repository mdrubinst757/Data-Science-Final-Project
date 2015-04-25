setwd('C:\\Users\\mdrub_000\\Desktop\\Data Science Project')

options(java.home="C:\\Program Files\\Java\\jre1.8.0_40")
options(java.parameters = "-Xmx10000m")
library(rJava)
library(RWeka)
library(tm)
library(glmnet)

scrape <- read.csv('scrape.csv')
scrape <- scrape[,c(1,12)]

a <- read.csv('htmlscrape.csv', na.strings='')
a <- a[,c(2,3)]
a[,2] <- as.character(a[,2])

a <- a[grep('FALSE', is.na(a[,2])),]

for(i in 1:nrow(a)) {
  a[i,2] <- gsub('<.*/>','', a[i,2])
  a[i,2] <- gsub('\n','', a[i,2])
}

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
a$id <- c(1:nrow(a))
mydtm_df$id <- c(1:nrow(a))
final <- merge(a, mydtm_df, by='id')
final <- final[grep('FALSE', duplicated(final$website.x)),]
scrape <- scrape[grep('FALSE', duplicated(scrape$Website)),]

final <- merge(final, scrape, by.x = 'website.x', by.y='Website')

#####CHI SQUARED SHIT

nonhealth <- final[final$healthcat==0, -c(1,2,3,3642)]
health <- final[final$healthcat==1, -c(1,2,3,3642)]

nhcount <- sapply(nonhealth, sum, 2)
hcount <- sapply(health, sum, 2)

hcount
fin1 <- rbind(nhcount, hcount)
fin1 <- as.data.frame(fin1)
fin1 <- t(fin1)
head(fin1)

library(MASS)
chi <- chisq.test(fin1)
res <- chi$stdres
quantile(res[,1], c(0.05,0.95)) 

resselect <- res[res[,1]>4.45 | res[,2]>1.37,]
resselect <- t(resselect)
fingrams <- colnames(resselect)

fin2 <- mydtm_df[,fingrams]
fin2$id <- c(1:nrow(fin2))
final <- merge(a, fin2, by='id')
final <- final[grep('FALSE', duplicated(final$website)),]
final <- merge(final, scrape, by.x = 'website', by.y='Website')

names(final)[1:5]

###LASSO REGRESSION

grid = 10^seq(10, -2, length=100) ##set lambda parameters
set.seed(235)
train=sample(1:nrow(final), nrow(final)/2) ##create training data rows
test = (-train) ##create test data rows

y.test = final$healthcat[test]
x.test = as.matrix(final[test,-c(1,2,3,grep('healthcat', names(final)))])

y.train = final$healthcat[-test]
x.train = as.matrix(final[-test,-c(1,2,3,grep('healthcat', names(final)))])

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

mylist

