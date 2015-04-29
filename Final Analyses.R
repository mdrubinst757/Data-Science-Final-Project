setwd('C:\\Users\\mdrub_000\\Desktop\\Github\\Data-Science-Final-Project')

cc1 <- read.csv('cc1.csv')
cc2 <- read.csv('cc2.csv')
cc3 <- read.csv('cc3.csv')
cc4 <- read.csv('cc4.csv')

cc <- rbind(cc1, cc2, cc3, cc4)
write.table(cc, 'Classification Results.csv', row.names=F, sep=',')
