##########################################
#######AD HOC SCRAPER ANALYSIS############
##########################################

##Before running this script, it is important to run the Merging.R script

##This script uses the output of the Ad Hoc.py, using training1, training2,
##and training3.csv, and conducts several analyses on them, including
##KNN, LPM, LDA, RF, and SVM (with linear and radial kernels). All
##test errors are calculated using cross-validation. 

##A final dataframe is output as adhocresults.csv, containing information
##on the performance of each model. These results should then be merged with
##the results of CVVarSelectandTests.R using the Final Results.R file.

setwd('C:\\Users\\mdrub_000\\Desktop\\Github\\Data-Science-Final-Project')

data <- read.csv('scrape.csv')
mydtm_df <- read.csv('dtm.csv')

a <- data[,c('Website', 'healthcat')]
final <- merge(a, mydtm_df, by.x='Website', by.y='url')
final <- final[grep('FALSE', duplicated(final$Website)),]
data <- data[grep("TRUE", data$Website %in% final$Website),]
data <- data[grep('FALSE', duplicated(data$Website)),]

dmatrix <- data[,c(2:9,12)] ##DF of only y and x variables

library(class)
library(DAAG)
library(boot)
library(MASS)
library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(caret)

#####################################
#######K NEAREST NEIGHBORS###########
#####################################

set.seed(30)
cv.est <- knn.cv(dmatrix, cl=dmatrix$healthcat)
knn.cm <- confusionMatrix(cv.est, dmatrix$healthcat)
knn.table <- knn.cm$byClass

#####################################
##############LPM####################
#####################################

set.seed(98)
fit <- lm(healthcat ~ ., data=dmatrix)
cv <- cv.lm(df=dmatrix, fit, m=5)
binary <- ifelse(cv$cvpred>0.5,1,0)
lm.cm <- confusionMatrix(binary, dmatrix$healthcat)
lm.table <- lm.cm$byClass

####################################
###########LDA AND QDA##############
####################################

set.seed(73)
summary(dmatrix)
ldamatrix <- dmatrix[,-c(3,8)]
fit <- lda(healthcat ~ . , data = ldamatrix, CV=TRUE)
lda.cm <- confusionMatrix(fit$class, dmatrix$healthcat)
lda.table <- lda.cm$byClass

####################################
###########RANDOM FORESTS###########
####################################

set.seed(333)
dmatrix$healthcat = as.factor(dmatrix$healthcat)
rf = randomForest(healthcat~., dmatrix)
rf.cm <- confusionMatrix(rf$predicted, dmatrix$healthcat)
rf.table <- rf.cm$byClass

####################################
#######SUPPORT VECTOR MACHINES######
####################################
names(dmatrix)
svmcv <- dmatrix[-696,-c(3,8)] ##Getting Rid of Optical and Quality
svmcv$healthcat <- as.factor(svmcv$healthcat)
names(svmcv)

##Cross-Validation for Test Error Estimate

set.seed(304)
randoms = sample(1:nrow(svmcv), nrow(svmcv))

splits = split(randoms, ceiling(seq_along(randoms)/(length(randoms)/5)))

train1 = svmcv[splits$`1`,]
train2 = svmcv[splits$`2`,]
train3 = svmcv[splits$`3`,]
train4 = svmcv[splits$`4`,]
train5 = svmcv[splits$`5`,]

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
svmcv$id <- c(1:nrow(svmcv))

lpred <- merge(svmcv, svmlpred, by='id')
l.svm <- confusionMatrix(lpred$pred, lpred$healthcat)
l.svm.table <- l.svm$byClass

####Radial Kernel####

##Cross-Validation for Test Error Estimate
set.seed(383)
randoms = sample(1:nrow(svmcv), nrow(svmcv))
splits = split(randoms, ceiling(seq_along(randoms)/(length(randoms)/5)))
train1 = svmcv[splits$`1`,]
train2 = svmcv[splits$`2`,]
train3 = svmcv[splits$`3`,]
train4 = svmcv[splits$`4`,]
train5 = svmcv[splits$`5`,]

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

svmlpred <- rbind(svm.r1, svm.r2, svm.r3, svm.r4, svm.r5)
svmcv$id <- c(1:nrow(svmcv))

rpred <- merge(svmcv, svmlpred, by='id')
r.svm <- confusionMatrix(rpred$pred, rpred$healthcat)
r.svm.table <- r.svm$byClass

###DATAFRAME OF COMPARATIVE ERROR RATES###
table <- rbind(lm.table, lda.table, rf.table, l.svm.table, r.svm.table,
      knn.table)
table <- as.data.frame(table)

write.csv(table, 'adhocresults.csv', row.names=T)
