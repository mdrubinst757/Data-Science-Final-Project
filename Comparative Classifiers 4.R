##############################################
#########COMPARATIVE CLASSIFIERS 4############
##############################################

##This file will find the classification error rates using the top ~50 keywords
##associated with healthrelated websites and the top ~50 keywords associated
##with non-health related websites. These keywords were selected by size of
##chi-squared residuals in the Determining Keywords.R file. Obviously incorrect
##keywords are removed.

library(class)
library(DAAG)
library(boot)
library(MASS)
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(e1071)

mydtm_df <- read.csv('dtm.csv')
mydtm_df$id <- c(1:nrow(mydtm_df))
keywords <- read.csv('t100keywords.csv')
scrape <- read.csv('scrape.csv')
scrape <- scrape[,c('healthcat', 'Website')]

keywords <- keywords[,2]
keywords <- keywords[-c(16,17)] ##Removing Nonsense

newmatrix <- mydtm_df[,grep("TRUE", names(mydtm_df) %in% keywords)]
urls <- mydtm_df$url
newmatrix <- cbind(newmatrix, urls)
newmatrix <- newmatrix[grep('FALSE', duplicated(newmatrix$url)),]
newmatrix <- merge(newmatrix, scrape, by.x='urls', by.y='Website')

trainmatrix <- newmatrix[,c(2:ncol(newmatrix))] ##subsetting to only vars

#####################################
##############KNN####################
#####################################

set.seed(30)
cv.est <- knn.cv(trainmatrix, cl=trainmatrix$healthcat)
knn.cm <- confusionMatrix(cv.est, trainmatrix$healthcat)
knn.table <- knn.cm$byClass

#####################################
##############LPM####################
#####################################

set.seed(98)
fit <- lm(healthcat ~ ., data=trainmatrix)
cv <- cv.lm(df=trainmatrix, fit, m=5)
binary <- ifelse(cv$cvpred>0.5,1,0)
lm.cm <- confusionMatrix(binary, trainmatrix$healthcat)
lm.table <- lm.cm$byClass

####################################
###########LDA AND QDA##############
####################################

set.seed(73)
fit <- lda(healthcat ~ . , data = trainmatrix, CV=TRUE)
lda.cm <- confusionMatrix(fit$class, trainmatrix$healthcat)
lda.table <- lda.cm$byClass

####################################
###########RANDOM FORESTS###########
####################################

set.seed(333)
trainmatrix$healthcat = as.factor(trainmatrix$healthcat)
rf = randomForest(healthcat~., trainmatrix)
rf.cm <- confusionMatrix(rf$predicted, trainmatrix$healthcat)
rf.table <- rf.cm$byClass

####################################
#######SUPPORT VECTOR MACHINES######
####################################

trainmatrix$healthcat <- as.factor(trainmatrix$healthcat)

##Linear Kernel
set.seed(35)
l.tune.out = tune(svm, healthcat~., data=trainmatrix, kernel='linear',
                  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

lbestmod = l.tune.out$best.model

##Cross-Validation for Test Error Estimate

set.seed(304)
randoms = sample(1:nrow(trainmatrix), nrow(trainmatrix))
train1 = trainmatrix[randoms[1:77],]
train2 = trainmatrix[randoms[78:154],]
train3 = trainmatrix[randoms[155:231],]
train4 = trainmatrix[randoms[232:308],]
train5 = trainmatrix[randoms[309:nrow(trainmatrix)],]

##CV FOLD 1
train = rbind(train2, train3, train4, train5)
test1 = train1

##Linear Kernel
set.seed(35)
l.tune.out = tune(svm, healthcat~., data=train, kernel='linear',
                  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
lbestmod.l1 = l.tune.out$best.model
svm.l1 <- predict(lbestmod.l1, test1)

##CV FOLD 2
train = rbind(train2, train3, train4, train1)
test2 = train5
set.seed(11)
l.tune.out = tune(svm, healthcat~., data=train, kernel='linear',
                  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
lbestmod.l2 = l.tune.out$best.model
svm.l2 <- predict(lbestmod.l2, test2)

##CV FOLD 3
train = rbind(train5, train3, train4, train1)
test3 = train2
set.seed(5)
l.tune.out = tune(svm, healthcat~., data=train, kernel='linear',
                  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
lbestmod.l3 = l.tune.out$best.model
svm.l3 <- predict(lbestmod.l3, test3)

##CV FOLD 4
train = rbind(train5, train2, train4, train1)
test4 = train3
set.seed(811)
l.tune.out = tune(svm, healthcat~., data=train, kernel='linear',
                  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
lbestmod.l4 = l.tune.out$best.model
svm.l4 <- predict(lbestmod.l4, test4)

##CV FOLD 5
train = rbind(train5, train2, train3, train1)
test5 = train4
set.seed(424)
l.tune.out = tune(svm, healthcat~., data=train, kernel='linear',
                  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
lbestmod.l5 = l.tune.out$best.model
svm.l5 <- predict(lbestmod.l5, test5)

svm.l1 <- as.data.frame(svm.l1)
svm.l2 <- as.data.frame(svm.l2)
svm.l3 <- as.data.frame(svm.l3)
svm.l4 <- as.data.frame(svm.l4)
svm.l5 <- as.data.frame(svm.l5)

svm.l1$id <- rownames(svm.l1)
svm.l2$id <- rownames(svm.l2)
svm.l3$id <- rownames(svm.l3)
svm.l4$id <- rownames(svm.l4)
svm.l5$id <- rownames(svm.l5)

names(svm.l1) <- c('pred', 'id')
names(svm.l2) <- c('pred', 'id')
names(svm.l3) <- c('pred', 'id')
names(svm.l4) <- c('pred', 'id')
names(svm.l5) <- c('pred', 'id')

svmlpred <- rbind(svm.l1, svm.l2, svm.l3, svm.l4, svm.l5)
trainmatrix$id <- c(1:nrow(trainmatrix))

lpred <- merge(trainmatrix, svmlpred, by='id')
l.svm <- confusionMatrix(lpred$pred, lpred$healthcat)
l.svm.table <- l.svm$byClass

####Radial Kernel####

##Cross-Validation for Test Error Estimate
set.seed(266)
randoms = sample(1:nrow(trainmatrix), nrow(trainmatrix))
train1 = trainmatrix[randoms[1:77],]
train2 = trainmatrix[randoms[78:154],]
train3 = trainmatrix[randoms[155:231],]
train4 = trainmatrix[randoms[232:308],]
train5 = trainmatrix[randoms[309:nrow(trainmatrix)],]

##CV FOLD 1
train = rbind(train2, train3, train4, train5)
test1 = train1
set.seed(8)
r.tune.out = tune(svm, healthcat~., data=train, kernel='radial',
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              gamma=c(0.5,1,2,3,4)))
rbestmod.r1 = r.tune.out$best.model
svm.r1 <- predict(rbestmod.r1, test1)

##CV FOLD 2
train = rbind(train2, train3, train4, train1)
test2 = train5
set.seed(235236)
r.tune.out = tune(svm, healthcat~., data=train, kernel='radial',
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              gamma=c(0.5,1,2,3,4)))
rbestmod.r2 = r.tune.out$best.model
svm.r2 <- predict(rbestmod.r2, test2)

##CV FOLD 3
train = rbind(train5, train3, train4, train1)
test3 = train2
set.seed(46622)
r.tune.out = tune(svm, healthcat~., data=train, kernel='radial',
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              gamma=c(0.5,1,2,3,4)))
rbestmod.r3 = r.tune.out$best.model
svm.r3 <- predict(rbestmod.r3, test3)

##CV FOLD 4
train = rbind(train5, train2, train4, train1)
test4 = train3
set.seed(1114)
r.tune.out = tune(svm, healthcat~., data=train, kernel='radial',
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              gamma=c(0.5,1,2,3,4)))
rbestmod.r4 = r.tune.out$best.model
svm.r4 <- predict(rbestmod.r4, test4)

##CV FOLD 5
train = rbind(train5, train2, train3, train1)
test5 = train4
set.seed(33525)
r.tune.out = tune(svm, healthcat~., data=train, kernel='radial',
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              gamma=c(0.5,1,2,3,4)))
rbestmod.r5 = r.tune.out$best.model
svm.r5 <- predict(rbestmod.r5, test5)

svm.r1 <- as.data.frame(svm.r1)
svm.r2 <- as.data.frame(svm.r2)
svm.r3 <- as.data.frame(svm.r3)
svm.r4 <- as.data.frame(svm.r4)
svm.r5 <- as.data.frame(svm.r5)

svm.r1$id <- rownames(svm.r1)
svm.r2$id <- rownames(svm.r2)
svm.r3$id <- rownames(svm.r3)
svm.r4$id <- rownames(svm.r4)
svm.r5$id <- rownames(svm.r5)

names(svm.r1) <- c('pred', 'id')
names(svm.r2) <- c('pred', 'id')
names(svm.r3) <- c('pred', 'id')
names(svm.r4) <- c('pred', 'id')
names(svm.r5) <- c('pred', 'id')

svmrpred <- rbind(svm.r1, svm.r2, svm.r3, svm.r4, svm.r5)

rpred <- merge(trainmatrix, svmrpred, by='id')
r.svm <- confusionMatrix(rpred$pred, rpred$healthcat)
r.svm.table <- r.svm$byClass

###DATAFRAME OF COMPARATIVE ERROR RATES###
table <- rbind(lm.table, lda.table, rf.table, l.svm.table, r.svm.table,
               knn.table)
table <- as.data.frame(table)
table
table$classifier <- rep(4, nrow(table))
write.csv(table, 'cc4.csv', row.names=T, sep=',')
