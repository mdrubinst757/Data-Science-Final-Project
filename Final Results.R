################################################################
########################FINAL RESULTS###########################
################################################################

##This script merges the results of CVVarSelectandTests.R and Comparative
##It then outputs the results both as a aggplot and as a 
##table (exported to finalresults.csv). The file also exports a CSV of 
##some summary statistics at the beginning (sumstats.csv)

setwd('C:\\Users\\mdrub_000\\Desktop\\Github\\Data-Science-Final-Project')
library(reshape2)
library(ggplot2)

##Summary Statistics##

NAICS.Codes <- c('5112', '5182', '5191', '5241', '5242',  
                 '5412', '5415', '5416', '5419', '5614'  
)
Percent.Health.Related <- c('32%', '52%', '23%','60%','20%', '40%', '48%', '36%', '32%', '56%')
Descriptions <- c('Software Publishers',
                  'Data Processing, Hosting, and Related Services',
                  'All Other Information Services',
                  'Insurance',
                  'Insurance',
                  'Accounting Services',
                  'Computer Systems',
                  'Consulting Services',
                  'Marketing Research',
                  'Business Support Services')

sumstats <- cbind(NAICS.Codes, Percent.Health.Related, Descriptions)
write.csv(sumstats, 'sumstats.csv', row.names=F)

##Merging Final Results##

results <- read.csv('results.csv')
results1 <- read.csv('adhocresults.csv')

names(results1) <- sub('X', 'classifier', names(results1))
results1$kwset <- 'Ad Hoc'
results1$classifier <- sub('knn.table', 'knn', results1$classifier)
results1$classifier <- sub('l.svm.table', 'svm.l', results1$classifier)
results1$classifier <- sub('lda.table', 'lda', results1$classifier)
results1$classifier <- sub('lm.table', 'lpm', results1$classifier)
results1$classifier <- sub('r.svm.table', 'svm.r', results1$classifier)
results1$classifier <- sub('rf.table', 'rf', results1$classifier)

results <- rbind(results, results1)

newdata <- melt(results)
sensitivity <- newdata[newdata$variable=='Sensitivity' | newdata$variable=='Specificity',]

sensitivity$classifier <- sub('knn','KNN',sensitivity$classifier)
sensitivity$classifier <- sub('lda','LDA',sensitivity$classifier)
sensitivity$classifier <- sub('lpm','LPM',sensitivity$classifier)
sensitivity$classifier <- sub('rf','RF',sensitivity$classifier)
sensitivity$classifier <- sub('svm.l','SVM (linear)',sensitivity$classifier)
sensitivity$classifier <- sub('svm.r','SVM (radial)',sensitivity$classifier)

sensitivity$kwset <- sub('lasso','Lasso',sensitivity$kwset)
sensitivity$kwset <- sub('t10','T10',sensitivity$kwset)
sensitivity$kwset <- sub('t100','T100',sensitivity$kwset)
sensitivity$kwset <- sub('t50b50','T50B50',sensitivity$kwset)

names(sensitivity) <- c('classifier','Keywords.Selection', 'Performance', 'Value')

###GGPLOT OF RESULTS###

c <- ggplot(sensitivity, aes(x=Keywords.Selection, y=Value, fill=Performance)) 
c <- c + geom_bar(stat='identity', position=position_dodge())
c <- c + facet_wrap(~classifier)
c <- c + ggtitle('Comparative Model Performance')
c
ggsave('results.png')

###TABLE OF RESULTS###

results$classifier <- sub('knn','KNN',results$classifier)
results$classifier <- sub('lda','LDA',results$classifier)
results$classifier <- sub('lpm','LPM',results$classifier)
results$classifier <- sub('rf','RF',results$classifier)
results$classifier <- sub('svm.l','SVM (linear)',results$classifier)
results$classifier <- sub('svm.r','SVM (radial)',results$classifier)

results$kwset <- sub('lasso','Lasso',results$kwset)
results$kwset <- sub('t10','T10',results$kwset)
results$kwset <- sub('t100','T100',results$kwset)
results$kwset <- sub('t50b50','T50B50',results$kwset)

write.csv(results,'finalresults.csv',row.names=F)
