##This script creates a dataframe with all the n-grams (1-3) contained
##in the text and the counts associated with each website. It then subsets
##sparse terms, found in less than 0.5% of all observations, and uses a 
##chi-squared test to further subsets the n-grams are most and least associated
##with healthcare websites. 

##These n-grams are then used to create three sets of keywords to attempt
##to classify the websites. The three methods are as follows:

    ##1) Top 100 n-grams associated with healthcare websites
    ##2) Top 50 n-grams associated with healthcare websites, top 50
        ##not associated with healthcare websites
    ##3) N-grams from lasso using top 100 n-grams (12 remained)

##This provides two sets of 100 n-grams and one smaller set to attempt to
##classify the websites that are done using the Comparative Classifiers 2,3,
## and 4.R files.

setwd('C:\\Users\\mdrub_000\\Desktop\\Github\\Data-Science-Final-Project')

options(java.home="C:\\Program Files\\Java\\jre1.8.0_40")
options(java.parameters = "-Xmx10000m")
library(rJava)
library(RWeka)
library(tm)
library(glmnet)

scrape <- read.csv('scrape.csv')
mydtm_df <- read.csv('dtm.csv')

a <- scrape[,c('Website', 'healthcat')]
final <- merge(a, mydtm_df, by.x='Website', by.y='url')
final <- final[grep('FALSE', duplicated(final$Website)),]

#####CHI SQUARED ELIMINATION

nonhealth <- final[final$healthcat==0, -c(1,2)]
health <- final[final$healthcat==1, -c(1,2)]

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

chiorder <- t(resselect)
keywords.t100 <- rownames(chiorder[order(chiorder[,1]),][1:100,])
keywords.5050 <- rownames(chiorder[order(chiorder[,1]),][1:50,])
keywords.5050 <- c(keywords.5050, rownames(chiorder[order(chiorder[,2]),][1:50,]))

keywordslist <- cbind(keywords.t100, keywords.5050)
keywordslist <- as.data.frame(keywordslist)

write.table(keywordslist, 't100keywords.csv', sep=',', row.names=F)

resselect
final <- final[,c('Website', 'healthcat', keywords.t100)]

###LASSO REGRESSION
grid = 10^seq(10, -2, length=100) ##set lambda parameters

set.seed(235)
train=sample(1:nrow(final), nrow(final)/2) ##create training data rows
test = (-train) ##create test data rows

y.test = final$healthcat[test]
x.test = final[test,-c(1,2)]

y.train = final$healthcat[train]
x.train = final[train,-c(1,2)]

lasso.mod=glmnet(as.matrix(x.train), y.train, alpha=1, lambda=grid)
set.seed(46)
cv.out = cv.glmnet(as.matrix(x.train), y.train, alpha=1) ##cross-validate to determine
##the best lambda
plot(cv.out)
bestlam=cv.out$lambda.min

##Using Tuning Parameter on Test Data
lasso.pred=predict(lasso.mod, s=bestlam, newx=as.matrix(x.test))
out=glmnet(as.matrix(x.test), y.test, alpha=1, lambda=grid)
lasso.coef=predict(out, type='coefficients', s=bestlam)

out1=glmnet(as.matrix(x.test), y.test, alpha=1, lambda=bestlam)
out1$dev.ratio ##Test Deviance 

#Out of Sample Correlation
cor.test(lasso.pred, y.test) 
yhat <- ifelse(lasso.pred>0.5,1,0)
table(yhat, y.test)

values <- as.numeric(lasso.coef[lasso.coef!=0])
names <- rownames(lasso.coef)
names <- names[grep("TRUE", as.vector(lasso.coef) %in% values)]

mylist <- cbind(values, names)
mylist <- as.data.frame(mylist)
mylist <- mylist[order(values),]

mylist <- mylist$names
mylist <- mylist[-c(2,15,1)] ##eliminating suspect n-grams

write.table(mylist, 'keywords.csv', row.names=F, sep=',')
