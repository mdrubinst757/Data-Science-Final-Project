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
keywords <- read.csv('keywords.csv')
scrape <- read.csv('scrape.csv')
scrape <- scrape[,c('healthcat', 'Website')]

newmatrix <- mydtm_df[,grep("TRUE", names(mydtm_df) %in% keywords[,1])]
urls <- mydtm_df$url
newmatrix <- cbind(newmatrix, urls)
newmatrix <- newmatrix[grep('FALSE', duplicated(newmatrix$url)),]
newmatrix <- merge(newmatrix, scrape, by.x='urls', by.y='Website')

trainmatrix <- newmatrix[,c(2:ncol(newmatrix))] ##subsetting to only vars

#####################################
#######K NEAREST NEIGHBORS###########
#####################################

set.seed(452)
cv.est <- knn.cv(trainmatrix, cl=trainmatrix$healthcat)
knn.table <- table(cv.est, trainmatrix$healthcat)
knn.table

table(cv.est, trainmatrix$healthcat)

#####################################
##############LPM####################
#####################################

set.seed(98)
fit <- lm(healthcat ~ ., data=trainmatrix)
cv <- cv.lm(df=trainmatrix, fit, m=5)
binary <- ifelse(cv$cvpred>0.5,1,0)
lm.table <- table(binary, trainmatrix$healthcat)
lm.table

####################################
###########LDA AND QDA##############
####################################

set.seed(73)
fit <- lda(healthcat ~ . , data = trainmatrix, CV=TRUE)
lda.table <- table(fit$class, trainmatrix$healthcat)
lda.table

summary(trainmatrix)

set.seed(334)
fit <- qda(healthcat ~ . , data = trainmatrix, CV=TRUE)
qda.table <- table(fit$class, trainmatrix$healthcat)
qda.table

set.seed(333)
dmatrix$healthcat = as.factor(dmatrix$healthcat)
test.x = dmatrix[,c(1:8)]
test.y = dmatrix[,9]

####################################
###########RANDOM FORESTS###########
####################################

set.seed(325)
rfmatrix <- trainmatrix
rfmatrix$healthcat <- as.factor(rfmatrix$healthcat)
rf = randomForest(healthcat~., rfmatrix, ntry=4)
rf

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

svm.l1 <- predict(lbestmod, test1)

##CV FOLD 2
train = rbind(train2, train3, train4, train1)
test2 = train5

svm.l2 <- predict(lbestmod, test2)

##CV FOLD 3
train = rbind(train5, train3, train4, train1)
test3 = train2

svm.l3 <- predict(lbestmod, test3)

##CV FOLD 4
train = rbind(train5, train2, train4, train1)
test4 = train3

svm.l4 <- predict(lbestmod, test4)

##CV FOLD 5
train = rbind(train5, train2, train3, train1)
test5 = train4

svm.l5 <- predict(lbestmod, test5)

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
l.svm <- table(lpred$pred, lpred$healthcat)
table(lpred$pred, lpred$healthcat)
l.svm

####Radial Kernel####
set.seed(843)
r.tune.out = tune(svm, healthcat~., data=trainmatrix, kernel='radial',
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              gamma=c(0.5,1,2,3,4)))
rbestmod = r.tune.out$best.model

##Cross-Validation for Test Error Estimate

set.seed(383)
randoms = sample(1:nrow(trainmatrix), nrow(trainmatrix))
train1 = trainmatrix[randoms[1:77],]
train2 = trainmatrix[randoms[78:154],]
train3 = trainmatrix[randoms[155:231],]
train4 = trainmatrix[randoms[232:308],]
train5 = trainmatrix[randoms[309:nrow(trainmatrix)],]

##CV FOLD 1
train = rbind(train2, train3, train4, train5)
test1 = train1

svm.r1 <- predict(rbestmod, test1)

##CV FOLD 2
train = rbind(train2, train3, train4, train1)
test2 = train5

svm.r2 <- predict(rbestmod, test2)

##CV FOLD 3
train = rbind(train5, train3, train4, train1)
test3 = train2

svm.r3 <- predict(rbestmod, test3)

##CV FOLD 4
train = rbind(train5, train2, train4, train1)
test4 = train3

svm.r4 <- predict(rbestmod, test4)

##CV FOLD 5
train = rbind(train5, train2, train3, train1)
test5 = train4

svm.r5 <- predict(rbestmod, test5)

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
trainmatrix$id <- c(1:nrow(trainmatrix))

rpred <- merge(trainmatrix, svmlpred, by='id')
r.svm <- table(rpred$pred, rpred$healthcat)
r.svm

####Polynomial Kernel####
set.seed(3630)
p.tune.out = tune(svm, healthcat~., data=trainmatrix, kernel='polynomial',
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              degree=c(1:10)))
pbestmod = p.tune.out$best.model

##Cross-Validation for Test Error Estimate
set.seed(121)
randoms = sample(1:nrow(trainmatrix), nrow(trainmatrix))
train1 = trainmatrix[randoms[1:77],]
train2 = trainmatrix[randoms[78:154],]
train3 = trainmatrix[randoms[155:231],]
train4 = trainmatrix[randoms[232:308],]
train5 = trainmatrix[randoms[309:nrow(trainmatrix)],]

##CV FOLD 1
train = rbind(train2, train3, train4, train5)
test1 = train1

svm.p1 <- predict(pbestmod, test1)

##CV FOLD 2
train = rbind(train2, train3, train4, train1)
test2 = train5

svm.p2 <- predict(pbestmod, test2)

##CV FOLD 3
train = rbind(train5, train3, train4, train1)
test3 = train2

svm.p3 <- predict(pbestmod, test3)

##CV FOLD 4
train = rbind(train5, train2, train4, train1)
test4 = train3

svm.p4 <- predict(pbestmod, test4)

##CV FOLD 5
train = rbind(train5, train2, train3, train1)
test5 = train4

svm.p5 <- predict(pbestmod, test5)

svm.p1 <- as.data.frame(svm.p1)
svm.p2 <- as.data.frame(svm.p2)
svm.p3 <- as.data.frame(svm.p3)
svm.p4 <- as.data.frame(svm.p4)
svm.p5 <- as.data.frame(svm.p5)

svm.p1$id <- rownames(svm.p1)
svm.p2$id <- rownames(svm.p2)
svm.p3$id <- rownames(svm.p3)
svm.p4$id <- rownames(svm.p4)
svm.p5$id <- rownames(svm.p5)

names(svm.p1) <- c('pred', 'id')
names(svm.p2) <- c('pred', 'id')
names(svm.p3) <- c('pred', 'id')
names(svm.p4) <- c('pred', 'id')
names(svm.p5) <- c('pred', 'id')

svmlpred <- rbind(svm.p1, svm.p2, svm.p3, svm.p4, svm.p5)
trainmatrix$id <- c(1:nrow(trainmatrix))

ppred <- merge(trainmatrix, svmlpred, by='id')
p.svm <- table(ppred$pred, ppred$healthcat)
p.svm

##Comparing SVM Designs:

##Note that the linear and polynomial model give identical results --
##this is likely bc the best tuning parameter was linear, so we ended
##up with basically the same model in both instances

###All tables
lm.table ##LPM Table
lda.table ##LDA Table
qda.table ##QDA Table (note: variables taken out here)
rf.errors ##Error Rates for RF (can generate table by multiplying these
##values by sample numbers)
l.svm ##Linear SVM
p.svm ##Polynomial SVM (which is linear)
r.svm ##Radial SVM
knn.table ##K-Nearest Neighbors