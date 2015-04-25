############################################
###########MERGING DATASETS#################
############################################

##This dataset merges two python generated webscraped datasets
##with a dataframe that contains additional variables on the websites.
##These datasets are then appended at the end are outputted to a csv.

setwd('C:\\Users\\mdrub_000\\Desktop\\Data Science Project')

###FIRST SCRAPE MERGE###

scrape1 <- read.csv('scrape1.csv', na.strings='NA')
scrape1$WebAddress <- scrape1$Website
scrape1$WebAddress <- sub('http://','',scrape1$WebAddress)

scrape1m <- read.csv('training1.csv')

scrape1 <- scrape1[!duplicated(scrape1$Website),]
scrape1m <- scrape1m[!duplicated(scrape1m$WebAddress),]

data1 <- merge(scrape1, scrape1m, by='WebAddress')
data1 <- data1[!is.na(data1[,4]),]
data1$healthcat = ifelse(data1$Health.y==0, 0, 1)

###SECOND SCRAPE MERGE###

scrape2 <- read.csv('scrape2.csv', na.strings='NA')
scrape2$WebAddress <- scrape2$Website
scrape2$WebAddress <- sub('http://','',scrape2$WebAddress)

scrape2m <- read.csv('training2.csv')

scrape2 <- scrape2[!duplicated(scrape2$Website),]
scrape2m <- scrape2m[!duplicated(scrape2m$Website),]

scrape2 <- scrape2[grep("TRUE", scrape2$WebAddress %in% scrape2m$Website),]
scrape2m <- scrape2m[grep("TRUE", scrape2m$Website %in% scrape2$WebAddress),]

data2 <- merge(scrape2, scrape2m, by.x='WebAddress', by.y='Website')
data2 <- data2[!is.na(data2[,4]),]
data2$healthcat = ifelse(data2$HC==0, 0, 1)

###FINAL DATASET MERGE###

names(data1)
names(data2)

data1 <- data1[,c(2:10,12:13,21)]
data2 <- data2[,c(3:13,17)]

names(data1) <- sub('Health.x', 'Health', names(data1))
names(data1)[11] <- 'NAICScode'
names(data2)[11] <- 'NAICScode'
names(data2)[10] <- 'CompanyName'

data <- rbind(data1, data2)
write.table(data, 'scrape.csv', row.names=F, sep=',')