############################################################
#####CROSS VALIDATED VARIABLE SELECTION AND CLASSIFIER######
############################################################

##This script uses the dtm with all the n-grams (1-3) contained
##in the text and the counts associated with each website. It first
##divides the training set into five folds for cross-validation for
##variable selection and to later run classifiers.

##Next, it runs a chi-squared test to further subsets the n-grams are most 
##and least associated with healthcare websites. Lists are generated for
##each fold of the top 100 words most associated with healthwebsites, the
##top 50 and bottom 50 words, and the top 10 words. Finally, another list
##is generated using the lasso to select from the top 100 keywords. The
##ultimate results compare the estimated test errors using each of these
##keywords lists.

##Next, several models are run using each list of keywords (which differ by
##each fold). These models include K-Nearest-Neighbors, Linear Probability
##Models, Linear Discriminant Analysis, Random Forests, and Support Vector
##Machines (with linear and radial kernels). 

##The performance of each model is ultimately summarized in a dataframe that
##is output as a csv (results.csv) at the end of the script. The script also
##outputs flatfiles containing the lists of keywords for each fold.
 
##To merge these results with the results from the ad hoc list of keywords,
##use the Final Results.R file.

setwd('C:\\Users\\mdrub_000\\Desktop\\Github\\Data-Science-Final-Project')

options(java.home="C:\\Program Files\\Java\\jre1.8.0_40")
options(java.parameters = "-Xmx10000m")
library(rJava)
library(RWeka)
library(tm)
library(glmnet)
library(MASS)
library(caret)

scrape <- read.csv('scrape.csv')
mydtm_df <- read.csv('dtm.csv')

a <- scrape[,c('Website', 'healthcat')]
final <- merge(a, mydtm_df, by.x='Website', by.y='url')
final <- final[grep('FALSE', duplicated(final$Website)),]

##CREATING CROSS-VALIDATION FOLDS##
set.seed(68)
randoms = sample(1:nrow(final), nrow(final))
splits = split(randoms, ceiling(seq_along(randoms)/(length(randoms)/5))) 
 
fold1 = final[splits$`1`,]
fold2 = final[splits$`2`,]
fold3 = final[splits$`3`,]
fold4 = final[splits$`4`,]
fold5 = final[splits$`5`,]
 
train1 = rbind(fold2, fold3, fold4, fold5)
test1 = fold1

train2 = rbind(fold1, fold3, fold4, fold5)
test2 = fold2

train3 = rbind(fold2, fold1, fold4, fold5)
test3 = fold3

train4 = rbind(fold2, fold3, fold1, fold5)
test4 = fold4

train5 = rbind(fold2, fold3, fold4, fold1)
test5 = fold5

##############################################
######CROSS-VALIDATED VAR SELECTION###########
##############################################

#####CHI SQUARED ELIMINATION######

chiselection.10percent <- function(dataframe, ... ) {
   nonhealth <- dataframe[dataframe$healthcat==0, -c(1,2)]
   health <- dataframe[dataframe$healthcat==1, -c(1,2)]
   
   nhcount <- sapply(nonhealth, sum, 2)
   hcount <- sapply(health, sum, 2)
   
   fin1 <- rbind(nhcount, hcount)
   fin1 <- as.data.frame(fin1)
   fin1 <- t(fin1)
   
   chi <- chisq.test(fin1)
   res <- chi$stdres
   res[order(res[,2]),]
   
   quantile(res[,1], c(0.05,0.95))[2]
   quantile(res[,2], c(0.05,0.95))[2]
   
   resselect <- res[res[,1]>quantile(res[,1], c(0.05,0.95))[2]
                    | res[,2]> quantile(res[,2], c(0.05,0.95))[2],]
   resselect <- t(resselect)
   chiorder <- t(resselect)
   chiorder <- rownames(chiorder[order(chiorder[,1]),])
   chiorder
 }
 
chiselection.t100 <- function(dataframe, ... ) {
    nonhealth <- dataframe[dataframe$healthcat==0, -c(1,2)]
    health <- dataframe[dataframe$healthcat==1, -c(1,2)]

    nhcount <- sapply(nonhealth, sum, 2)
    hcount <- sapply(health, sum, 2)

    fin1 <- rbind(nhcount, hcount)
    fin1 <- as.data.frame(fin1)
    fin1 <- t(fin1)

    chi <- chisq.test(fin1)
    res <- chi$stdres
    res[order(res[,2]),]

    quantile(res[,1], c(0.05,0.95))[2]
    quantile(res[,2], c(0.05,0.95))[2]

    resselect <- res[res[,1]>quantile(res[,1], c(0.05,0.95))[2]
                 | res[,2]> quantile(res[,2], c(0.05,0.95))[2],]
    resselect <- t(resselect)
    chiorder <- t(resselect)
    keywords.t100 <- rownames(chiorder[order(chiorder[,1]),][1:100,])
    keywords.t100
}


chiselection.t50b50 <- function(dataframe, ... ) {
  nonhealth <- dataframe[dataframe$healthcat==0, -c(1,2)]
  health <- dataframe[dataframe$healthcat==1, -c(1,2)]
  
  nhcount <- sapply(nonhealth, sum, 2)
  hcount <- sapply(health, sum, 2)
  
  fin1 <- rbind(nhcount, hcount)
  fin1 <- as.data.frame(fin1)
  fin1 <- t(fin1)
  
  chi <- chisq.test(fin1)
  res <- chi$stdres
  res[order(res[,2]),]
  
  quantile(res[,1], c(0.05,0.95))[2]
  quantile(res[,2], c(0.05,0.95))[2]
  
  resselect <- res[res[,1]>quantile(res[,1], c(0.05,0.95))[2]
                   | res[,2]> quantile(res[,2], c(0.05,0.95))[2],]
  resselect <- t(resselect)
  chiorder <- t(resselect)
  keywords.5050 <- rownames(chiorder[order(chiorder[,1]),][1:50,])
  keywords.5050 <- c(keywords.5050, rownames(chiorder[order(chiorder[,2]),][1:50,]))
  keywords.5050
}

chiselection.t10 <- function(dataframe, ... ) {
  nonhealth <- dataframe[dataframe$healthcat==0, -c(1,2)]
  health <- dataframe[dataframe$healthcat==1, -c(1,2)]
  
  nhcount <- sapply(nonhealth, sum, 2)
  hcount <- sapply(health, sum, 2)
  
  fin1 <- rbind(nhcount, hcount)
  fin1 <- as.data.frame(fin1)
  fin1 <- t(fin1)
  
  chi <- chisq.test(fin1)
  res <- chi$stdres
  res[order(res[,2]),]
  
  quantile(res[,1], c(0.05,0.95))[2]
  quantile(res[,2], c(0.05,0.95))[2]
  
  resselect <- res[res[,1]>quantile(res[,1], c(0.05,0.95))[2]
                   | res[,2]> quantile(res[,2], c(0.05,0.95))[2],]
  resselect <- t(resselect)
  chiorder <- t(resselect)
  keywords.t10 <- rownames(chiorder[order(chiorder[,1]),][1:10,])
  keywords.t10
}

terms.1 <- chiselection.10percent(train1)
terms.2 <- chiselection.10percent(train2)
terms.3 <- chiselection.10percent(train3)
terms.4 <- chiselection.10percent(train4) 
terms.5 <- chiselection.10percent(train5)
 
keywords.f1 <- chiselection.t100(train1)
keywords.f2 <- chiselection.t100(train2)
keywords.f3 <- chiselection.t100(train3)
keywords.f4 <- chiselection.t100(train4)
keywords.f5 <- chiselection.t100(train5)

keywords.g1 <- chiselection.t50b50(train1)
keywords.g2 <- chiselection.t50b50(train2)
keywords.g3 <- chiselection.t50b50(train3)
keywords.g4 <- chiselection.t50b50(train4)
keywords.g5 <- chiselection.t50b50(train5)
 
keywords.h1 <- chiselection.t10(train1)
keywords.h2 <- chiselection.t10(train2)
keywords.h3 <- chiselection.t10(train3)
keywords.h4 <- chiselection.t10(train4)
keywords.h5 <- chiselection.t10(train5)

###LASSO REGRESSION FOR FURTHER SELECTION FROM KEYWORDS.F###
lassoselect <- function(dataframe, keywords,...){
    x.train = dataframe[,-c(1:2)]
    x.train = dataframe[, keywords]
    y.train = dataframe[,2]
    grid = 10^seq(10, -2, length=100) ##set lambda parameters
    lasso.mod=glmnet(as.matrix(x.train), y.train, alpha=1, lambda=grid)
    
    set.seed(46)
    cv.out = cv.glmnet(as.matrix(x.train), y.train, alpha=1)
    bestlam=cv.out$lambda.min

    ##Using Tuning Parameter on Train Data for Var Selection
    lasso.coef=predict(lasso.mod, type='coefficients', s=bestlam)
    values <- as.numeric(lasso.coef[lasso.coef!=0])
    names <- rownames(lasso.coef)
    names <- names[grep("TRUE", as.vector(lasso.coef) %in% values)]
}

lasso.keywords1 <- lassoselect(train1, keywords.f1)
lasso.keywords2 <- lassoselect(train2, keywords.f2)
lasso.keywords3 <- lassoselect(train3, keywords.f3)
lasso.keywords4 <- lassoselect(train4, keywords.f4)
lasso.keywords5 <- lassoselect(train5, keywords.f5)

###Removing Any Nonsense From Keywords Lists
 
lasso.keywords1 <- lasso.keywords1[-c(1,4)]
lasso.keywords2 <- lasso.keywords2[-c(1)]
lasso.keywords3 <- lasso.keywords3[-c(1)]
lasso.keywords4 <- lasso.keywords4[-c(1,5)]
lasso.keywords5 <- lasso.keywords5[-c(1)]
 
keywords.g1 <- keywords.g1[-c(6,7)]
keywords.g2 <- keywords.g2[-c(43,35)]
keywords.g4 <- keywords.g4[-c(8,16)]
keywords.g5 <- keywords.g4[-c(39)]
 
keywords.f1 <- keywords.f1[-c(6,7)]
keywords.f2 <- keywords.f2[-c(43,45)]
keywords.f3 <- keywords.f3[-c(77)]
keywords.f4 <- keywords.f4[-c(95,8,16)]
keywords.f5 <- keywords.f5[-c(6,7,47)]

keywords.h1 <- keywords.h1[-c(6,7)]
keywords.h4 <- keywords.h4[-8]

##Analysing Variability Between Positive and Negative Keywords Lists##
  ##These results are cited briefly in the final paper, but are not
  ##critical to this anlaysis.

nwords1 <- keywords.g1[49:98]
nwords2 <- keywords.g2[50:98]
nwords3 <- keywords.g3[51:100]
nwords4 <- keywords.g4[49:98]
nwords5 <- keywords.g5[49:97]

pwords1 <- keywords.g1[1:48]
pwords2 <- keywords.g2[1:49]
pwords3 <- keywords.g3[1:50]
pwords4 <- keywords.g4[1:48]
pwords5 <- keywords.g5[1:48]

mean(c(1-length(grep('FALSE', nwords1 %in% nwords2))/length(nwords1),
       1-length(grep('FALSE', nwords1 %in% nwords3))/length(nwords1),
       1-length(grep('FALSE', nwords1 %in% nwords4))/length(nwords1),
       1-length(grep('FALSE', nwords1 %in% nwords5))/length(nwords1),
       1-length(grep('FALSE', nwords2 %in% nwords3))/length(nwords2),
       1-length(grep('FALSE', nwords2 %in% nwords4))/length(nwords2),
       1-length(grep('FALSE', nwords2 %in% nwords5))/length(nwords2),
       1-length(grep('FALSE', nwords3 %in% nwords4))/length(nwords3),
       1-length(grep('FALSE', nwords3 %in% nwords5))/length(nwords3),
       1-length(grep('FALSE', nwords4 %in% nwords5))/length(nwords4),
       
       1-length(grep('FALSE', nwords2 %in% nwords1))/length(nwords2),
       1-length(grep('FALSE', nwords3 %in% nwords1))/length(nwords3),
       1-length(grep('FALSE', nwords4 %in% nwords1))/length(nwords4),
       1-length(grep('FALSE', nwords5 %in% nwords1))/length(nwords5),
       1-length(grep('FALSE', nwords3 %in% nwords2))/length(nwords3),
       1-length(grep('FALSE', nwords4 %in% nwords2))/length(nwords4),
       1-length(grep('FALSE', nwords5 %in% nwords2))/length(nwords5),
       1-length(grep('FALSE', nwords4 %in% nwords3))/length(nwords4),
       1-length(grep('FALSE', nwords5 %in% nwords3))/length(nwords5),
       1-length(grep('FALSE', nwords4 %in% nwords4))/length(nwords4)))

mean(c(1-length(grep('FALSE', pwords1 %in% pwords2))/length(pwords1),
       1-length(grep('FALSE', pwords1 %in% pwords3))/length(pwords1),
       1-length(grep('FALSE', pwords1 %in% pwords4))/length(pwords1),
       1-length(grep('FALSE', pwords1 %in% pwords5))/length(pwords1),
       1-length(grep('FALSE', pwords2 %in% pwords3))/length(pwords2),
       1-length(grep('FALSE', pwords2 %in% pwords4))/length(pwords2),
       1-length(grep('FALSE', pwords2 %in% pwords5))/length(pwords2),
       1-length(grep('FALSE', pwords3 %in% pwords4))/length(pwords3),
       1-length(grep('FALSE', pwords3 %in% pwords5))/length(pwords3),
       1-length(grep('FALSE', pwords4 %in% pwords5))/length(pwords4),
       
       1-length(grep('FALSE', pwords2 %in% pwords1))/length(pwords2),
       1-length(grep('FALSE', pwords3 %in% pwords1))/length(pwords3),
       1-length(grep('FALSE', pwords4 %in% pwords1))/length(pwords4),
       1-length(grep('FALSE', pwords5 %in% pwords1))/length(pwords5),
       1-length(grep('FALSE', pwords3 %in% pwords2))/length(pwords3),
       1-length(grep('FALSE', pwords4 %in% pwords2))/length(pwords4),
       1-length(grep('FALSE', pwords5 %in% pwords2))/length(pwords5),
       1-length(grep('FALSE', pwords4 %in% pwords3))/length(pwords4),
       1-length(grep('FALSE', pwords5 %in% pwords3))/length(pwords5),
       1-length(grep('FALSE', pwords5 %in% pwords4))/length(pwords5)))

##EXPORTING LISTS TO CSVS##
 ##Note: Not all cols are the same lengths; must manually delete observations
 ##b/c R automatically starts the list over again to fill in values
 
keywords.f <- cbind(keywords.f1,keywords.f2,keywords.f3,keywords.f4,keywords.f5)
write.table(keywords.f, 't100keywords.csv', row.names=F, sep=',')
 
keywords.g <- cbind(keywords.g1,keywords.g2,keywords.g3,keywords.g4,keywords.g5)
write.table(keywords.g, 't50b50keywords.csv', row.names=F, sep=',')

lasso.keywords <- cbind(lasso.keywords1,lasso.keywords2,lasso.keywords3,
                        lasso.keywords4,lasso.keywords5)
 
write.table(lasso.keywords, 'lkeywords.csv', row.names=F, sep=',')

####Creating Lists of Keywords###

f.keywords <- list(keywords.f1, keywords.f2, keywords.f3, keywords.f4,
                   keywords.f5)

g.keywords <- list(keywords.g1, keywords.g2, keywords.g3, keywords.g4,
                   keywords.g5)

h.keywords <- list(keywords.h1, keywords.h2, keywords.h3, keywords.h4,
                   keywords.h5)

l.keywords <- list(lasso.keywords1, lasso.keywords2, lasso.keywords3,
                   lasso.keywords4, lasso.keywords5)

terms <- list(terms.1, terms.2, terms.3, terms.4, terms.5)

cl <- final$healthcat[randoms] ##True Classification

##########################################
###################KNN####################
##########################################

library(class)

knntest <- function(train, test, keywords,...){
    trueclass.train <- train$healthcat
    trueclass.test <- test$healthcat
    training <- train[,keywords]
    testing <- test[,keywords]
    knn(training, testing, trueclass.train, k=1)
}

models.knn <- function(keywords, cl,...){
  pred1 <- knntest(train1, test1, keywords[[1]])
  pred2 <- knntest(train2, test2, keywords[[2]])
  pred3 <- knntest(train3, test3, keywords[[3]])
  pred4 <- knntest(train4, test4, keywords[[4]])
  pred5 <- knntest(train5, test5, keywords[[5]])

  pred <- c(pred1, pred2, pred3, pred4, pred5)
  pred = pred-1
  
  knn.cm <- confusionMatrix(pred, cl)
  knn.table <- knn.cm$byClass
  knn.table
  
}

knn1 <- models.knn(l.keywords,cl)
knn2 <- models.knn(f.keywords,cl)
knn3 <- models.knn(g.keywords,cl)
knn4 <- models.knn(h.keywords,cl)

knn <- rbind(knn1, knn2, knn3, knn4); knn <- as.data.frame(knn)
knn$classifier = rep('knn', nrow(knn)); knn$kwset = c('lasso', 't100', 't50b50', 't10')
 
#####################################
##############LPM####################
#####################################

lmpredict <- function(train, test, keywords,...){
    train = train[,c('healthcat', keywords)]
    test = test[,c('healthcat', keywords)]
    fit <- lm(healthcat ~ ., data=train)
    pred <- predict(fit, test)
    binary <- ifelse(pred>0.5,1,0)
    binary   
}

models.lpm <- function(keywords, cl,...){
  pred1 <- lmpredict(train1, test1, keywords[[1]])
  pred2 <- lmpredict(train2, test2, keywords[[2]])
  pred3 <- lmpredict(train3, test3, keywords[[3]])
  pred4 <- lmpredict(train4, test4, keywords[[4]])
  pred5 <- lmpredict(train5, test5, keywords[[5]])
  
  pred <- c(pred1, pred2, pred3, pred4, pred5)

  lm.cm <- confusionMatrix(pred, cl)
  lm.table <- lm.cm$byClass
  lm.table
}

lpm1 <- models.lpm(l.keywords,cl)
lpm2 <- models.lpm(f.keywords,cl)
lpm3 <- models.lpm(g.keywords,cl)
lpm4 <- models.lpm(h.keywords,cl)

lpm <- rbind(lpm1, lpm2, lpm3, lpm4); lpm <- as.data.frame(lpm)
lpm$classifier = rep('lpm', nrow(lpm)); lpm$kwset = c('lasso', 't100', 't50b50', 't10')
 
####################################
#################LDA################
####################################

ldapredict <- function(train, test, keywords,...){
    train = train[,c('healthcat', keywords)]
    test = test[,c('healthcat', keywords)]
    fit <- lda(healthcat ~ . , data = train, CV=FALSE)
    pred <- predict(fit, test)
    unlist(pred[1])
}

models.lda <- function(keywords, cl,...){
  pred1 <- ldapredict(train1, test1, keywords[[1]])
  pred2 <- ldapredict(train2, test2, keywords[[2]])
  pred3 <- ldapredict(train3, test3, keywords[[3]])
  pred4 <- ldapredict(train4, test4, keywords[[4]])
  pred5 <- ldapredict(train5, test5, keywords[[5]])
  pred <- c(pred1, pred2, pred3, pred4, pred5)
  pred = pred - 1
  lda.cm <- confusionMatrix(pred, cl)
  lda.table <- lda.cm$byClass
  lda.table
}

lda1 <- models.lda(l.keywords,cl)
lda2 <- models.lda(f.keywords,cl)
lda3 <- models.lda(g.keywords,cl) ##doesn't work
lda4 <- models.lda(h.keywords,cl)

lda <- rbind(lda1, lda2, lda3, lda4); lda <- as.data.frame(lda)
lda$classifier = rep('lda', nrow(lda)); lda$kwset = c('lasso', 't100', 't50b50', 't10')

####################################
###########RANDOM FORESTS###########
####################################

library(randomForest)

rfpredict <- function(train, test, keywords,...){
    train = train[,c('healthcat', keywords)]
    test = test[,c('healthcat', keywords)]
    train$healthcat = as.factor(train$healthcat)
    test$healthcat = as.factor(test$healthcat)
    rf = randomForest(healthcat~., train)
    pred = predict(rf, test)    
    pred
}

models.rf <- function(keywords, cl,...){
  pred1 <- rfpredict(train1, test1, keywords[[1]])
  pred2 <- rfpredict(train2, test2, keywords[[2]])
  pred3 <- rfpredict(train3, test3, keywords[[3]])
  pred4 <- rfpredict(train4, test4, keywords[[4]])
  pred5 <- rfpredict(train5, test5, keywords[[5]])
  pred <- c(pred1, pred2, pred3, pred4, pred5)
  pred = pred - 1
  rf.cm <- confusionMatrix(pred, cl)
  rf.table <- rf.cm$byClass
  rf.table
}

rf1 <- models.rf(l.keywords,cl)
rf2 <- models.rf(f.keywords,cl)
rf3 <- models.rf(g.keywords,cl)
rf4 <- models.rf(h.keywords,cl)

rf <- rbind(rf1, rf2, rf3, rf4); rf <- as.data.frame(rf)
rf$classifier = rep('rf', nrow(rf)); rf$kwset = c('lasso', 't100', 't50b50', 't10')
 
####################################
#######SUPPORT VECTOR MACHINES######
####################################
library(e1071)

##Linear Kernel
svm.l.predict <- function(train, test, keywords,...){
    train = train[,c('healthcat', keywords)]
    test = test[,c('healthcat', keywords)]
    train$healthcat = as.factor(train$healthcat)
    test$healthcat = as.factor(test$healthcat)
    set.seed(15)
    l.tune.out = tune(svm, healthcat~., data=train, kernel='linear',
                      ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
    lbestmod = l.tune.out$best.model
    pred <- predict(lbestmod, test)
    pred
}

models.svm.l <- function(keywords, cl,...){
  pred1 <- svm.l.predict(train1, test1, keywords[[1]])
  pred2 <- svm.l.predict(train2, test2, keywords[[2]])
  pred3 <- svm.l.predict(train3, test3, keywords[[3]])
  pred4 <- svm.l.predict(train4, test4, keywords[[4]])
  pred5 <- svm.l.predict(train5, test5, keywords[[5]])
  pred <- c(pred1, pred2, pred3, pred4, pred5)
  pred = pred - 1
  svm.l.cm <- confusionMatrix(pred, cl)
  svm.l.table <- svm.l.cm$byClass
  svm.l.table
}

svm.l1 <- models.svm.l(l.keywords,cl)
svm.l2 <- models.svm.l(f.keywords,cl)
svm.l3 <- models.svm.l(g.keywords,cl)
svm.l4 <- models.svm.l(h.keywords,cl)

svm.l <- rbind(svm.l1, svm.l2, svm.l3, svm.l4); svm.l <- as.data.frame(svm.l)
svm.l$classifier = rep('svm.l', nrow(svm.l)); svm.l$kwset = c('lasso', 't100', 't50b50', 't10')

##Radial Kernel
svm.r.predict <- function(train, test, keywords,...){
  train = train[,c('healthcat', keywords)]
  test = test[,c('healthcat', keywords)]
  train$healthcat = as.factor(train$healthcat)
  test$healthcat = as.factor(test$healthcat)
  set.seed(255)
  l.tune.out = tune(svm, healthcat~., data=train, kernel='radial',
                    ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100),
                                gamma=c(0.5,1,2,3,4)))
  lbestmod = l.tune.out$best.model
  pred <- predict(lbestmod, test)
  pred
}

models.svm.r <- function(keywords, cl,...){
  pred1 <- svm.r.predict(train1, test1, keywords[[1]])
  pred2 <- svm.r.predict(train2, test2, keywords[[2]])
  pred3 <- svm.r.predict(train3, test3, keywords[[3]])
  pred4 <- svm.r.predict(train4, test4, keywords[[4]])
  pred5 <- svm.r.predict(train5, test5, keywords[[5]])
  pred <- c(pred1, pred2, pred3, pred4, pred5)
  pred = pred - 1
  svm.r.cm <- confusionMatrix(pred, cl)
  svm.r.table <- svm.r.cm$byClass
  svm.r.table
}

svm.r1 <- models.svm.r(l.keywords,cl)
svm.r2 <- models.svm.r(f.keywords,cl)
svm.r3 <- models.svm.r(g.keywords,cl)
svm.r4 <- models.svm.r(h.keywords,cl)

svm.r <- rbind(svm.r1, svm.r2, svm.r3, svm.r4); svm.r <- as.data.frame(svm.r)
svm.r$classifier = rep('svm.r', nrow(svm.r)); svm.r$kwset = c('lasso', 't100', 't50b50', 't10')

######MAKING A TABLE#######
table <- rbind(knn, lpm, lda, rf, svm.l, svm.r)
table <- as.data.frame(table.m2)
table

write.csv(table, 'results.csv', row.names=F)
