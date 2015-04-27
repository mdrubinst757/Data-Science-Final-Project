setwd('C:\\Users\\mdrub_000\\Desktop\\Github\\Data-Science-Final-Project')
data <- read.csv('scrape.csv')
dmatrix <- data[,c(2:9,12)] ##DF of only y and x variables

library(class)
library(DAAG)
library(boot)
library(MASS)
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(e1071)

##Plot Illustrating the Difficulty of
##Creating LInear Decision Boundaries


mycolor = c()
for(i in 1:nrow(dmatrix)){
  if(dmatrix$healthcat[i]==0){
    mycolor = c(mycolor, 'black')}
  else {mycolor = c(mycolor, 'red')}
}
names(dmatrix) <- sub('Dental.Count', 'Dental', names(dmatrix))
png('rplot.png')
pairs(dmatrix[,c(1:8)], col = mycolor, pch=20) 
dev.off()
?png
#####################################
#######K NEAREST NEIGHBORS###########
#####################################

set.seed(30)
cv.est <- knn.cv(dmatrix, cl=dmatrix$healthcat)
knn.table <- table(cv.est, dmatrix$healthcat)
knn.table

#####################################
##############LPM####################
#####################################

set.seed(98)
fit <- lm(healthcat ~ ., data=dmatrix)
cv <- cv.lm(df=dmatrix, fit, m=5)
binary <- ifelse(cv$cvpred>0.5,1,0)
lm.table <- table(binary, dmatrix$healthcat)
lm.table

####################################
###########LDA AND QDA##############
####################################

set.seed(73)
fit <- lda(healthcat ~ . , data = dmatrix, CV=TRUE)
lda.table <- table(fit$class, dmatrix$healthcat)
lda.table

summary(dmatrix) ##Determining variables with little variance

qda <- dmatrix[,c(1,2,4,5,6,7,9)] ##Removing Variables with little variance
##Otherwise rank deficiency error
##Removed Quality and Optical

set.seed(3334)
fit <- qda(healthcat ~ . , data = qda, CV=TRUE)
qda.table <- table(fit$class, dmatrix$healthcat)
qda.table

####################################
###########RANDOM FORESTS###########
####################################

##Not sure (at all) if I've done the CV right here
set.seed(333)
dmatrix$healthcat = as.factor(dmatrix$healthcat)
rf = randomForest(healthcat~., dmatrix)

####################################
#######SUPPORT VECTOR MACHINES######
####################################

svmcv <- dmatrix[-475,-3] ##Getting Rid of Optical
svmcv$healthcat <- as.factor(svmcv$healthcat)

##Linear Kernel
set.seed(35)
l.tune.out = tune(svm, healthcat~., data=svmcv, kernel='linear',
                  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))

lbestmod = l.tune.out$best.model

str(summary(lbestmod))

##Cross-Validation for Test Error Estimate

set.seed(304)
randoms = sample(1:nrow(svmcv), nrow(svmcv))
train1 = svmcv[randoms[1:95],]
train2 = svmcv[randoms[96:190],]
train3 = svmcv[randoms[191:285],]
train4 = svmcv[randoms[286:380],]
train5 = svmcv[randoms[381:nrow(svmcv)],]

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
svmcv$id <- c(1:nrow(svmcv))

lpred <- merge(svmcv, svmlpred, by='id')
l.svm <- table(lpred$pred, lpred$healthcat)
l.svm

####Radial Kernel####
svmcv <- svmcv[,-9]

set.seed(843)
r.tune.out = tune(svm, healthcat~., data=svmcv, kernel='radial',
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              gamma=c(0.5,1,2,3,4)))
rbestmod = r.tune.out$best.model

##Cross-Validation for Test Error Estimate

set.seed(383)
randoms = sample(1:nrow(svmcv), nrow(svmcv))
train1 = svmcv[randoms[1:95],]
train2 = svmcv[randoms[96:190],]
train3 = svmcv[randoms[191:285],]
train4 = svmcv[randoms[286:380],]
train5 = svmcv[randoms[381:nrow(svmcv)],]

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
svmcv$id <- c(1:nrow(svmcv))

rpred <- merge(svmcv, svmlpred, by='id')
r.svm <- table(rpred$pred, rpred$healthcat)
r.svm

####Polynomial Kernel####
svmcv <- svmcv[,-9]

set.seed(3630)
p.tune.out = tune(svm, healthcat~., data=svmcv, kernel='polynomial',
                  ranges=list(cost=c(0.1,1,10,100,1000),
                              degree=c(1:10)))
pbestmod = p.tune.out$best.model

##Cross-Validation for Test Error Estimate
set.seed(121)
randoms = sample(1:nrow(svmcv), nrow(svmcv))
train1 = svmcv[randoms[1:95],]
train2 = svmcv[randoms[96:190],]
train3 = svmcv[randoms[191:285],]
train4 = svmcv[randoms[286:380],]
train5 = svmcv[randoms[381:nrow(svmcv)],]

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
svmcv$id <- c(1:nrow(svmcv))

ppred <- merge(svmcv, svmlpred, by='id')
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
rf ##Error Rates for RF (can generate table by multiplying these
          ##values by sample numbers)
l.svm ##Linear SVM
p.svm ##Polynomial SVM (which is linear)
r.svm ##Radial SVM
knn.table ##K-Nearest Neighbors

