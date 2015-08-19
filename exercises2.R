

## exercises 02 script
set.seed(620)

# flights at ABIA
flights = read.csv('/Users/bengrisz/Documents/UT/Academic/Predictive2/STA380/data/ABIA.csv', header=TRUE)

#head(flights)
#hist(flights$DayOfWeek)

flights$Year <- NULL
flights$FlightNum <- NULL
flights$TailNum <- NULL
flights$UniqueCarrier <- NULL
flights$TaxiIn <- NULL
flights$TaxiOut <- NULL
flights$CancellationCode <- NULL

flights$Month <- factor(flights$Month)
flights$DayofMonth <- factor(flights$DayofMonth)
# flights$CarrierDelay[is.na(flights$CarrierDelay)] <- 0
# flights$WeatherDelay[is.na(flights$WeatherDelay)] <- 0
# flights$NASDelay[is.na(flights$NASDelay)] <- 0
# flights$SecurityDelay[is.na(flights$SecurityDelay)] <- 0
# flights$LateAircraftDelay[is.na(flights$LateAircraftDelay)] <- 0
flights$CRSDepTime[flights$CRSDepTime<300] <- 
      flights$CRSDepTime[flights$CRSDepTime<300] + 2400
flights$CarrierDelay <- NULL
flights$WeatherDelay <- NULL
flights$NASDelay <- NULL
flights$SecurityDelay <- NULL
flights$LateAircraftDelay <- NULL

# flights = data.frame(flights$CRSDepTime, flights$DepDelay)

plot(flights$CRSDepTime, flights$DepDelay)
quad.fit = lm(DepDelay ~ poly(CRSDepTime, 2),data=flights)

minutes = seq(0,2400,1)
quad.pred = predict(quad.fit,data.frame(CRSDepTime=minutes))

plot(flights$CRSDepTime, flights$DepDelay)
lines(minutes, quad.pred, col="red", lwd=3)



library(kknn)

train = data.frame(flights$CRSDepTime, flights$DepDelay)
colnames(train) <- c("DepTime", "Delay")
test = data.frame(flights$CRSDepTime, flights$DepDelay)
colnames(test) <- c("DepTime", "Delay")

ind = order(test[,1])
test = test[ind,]



near = kknn(Delay~DepTime, train, test, k=1000, kernel="rectangular")

plot(flights$CRSDepTime, flights$DepDelay, main=paste("k=5"), 
     pch=19,cex=0.8,col="darkgray")

lines(test[ind,1],near$fitted[ind],col=2,lwd=2)


rm(list=ls())
# author attribution

library(tm)

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }



### TRAIN corpus
author_dirs = Sys.glob('~/Documents/UT/Academic/Predictive2/STA380/data/ReutersC50/C50train/*')

file_list = NULL
labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))

train_corpus = Corpus(VectorSource(all_docs))
names(train_corpus) = file_list


train_corpus = tm_map(train_corpus, content_transformer(tolower))
train_corpus = tm_map(train_corpus, content_transformer(removeNumbers))
train_corpus = tm_map(train_corpus, content_transformer(removePunctuation))
train_corpus = tm_map(train_corpus, content_transformer(stripWhitespace))
train_corpus = tm_map(train_corpus, content_transformer(removeWords), stopwords("en"))

DTM_train = DocumentTermMatrix(train_corpus)
DTM_train = removeSparseTerms(DTM_train, .99)
DTM_train

X_train = as.matrix(DTM_train)

smooth_count = (1/nrow(X_train))
X_train = X_train + smooth_count
w_all = rowsum(X_train, labels)
w_all = w_all/sum(w_all)
w_all = log(w_all)
### ^^^ train corpus

### TEST corpus
author_dirs = Sys.glob('../data/ReutersC50/C50test/*')

file_list = NULL
test_labels = NULL
author_names = NULL
for(author in author_dirs) {
  author_name = substring(author, first=28)
  author_names = append(author_names, author_name)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  test_labels = append(test_labels, rep(author_name, length(files_to_add)))
}

all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))

test_corpus = Corpus(VectorSource(all_docs))
names(test_corpus) = file_list

test_corpus = tm_map(test_corpus, content_transformer(tolower))
test_corpus = tm_map(test_corpus, content_transformer(removeNumbers))
test_corpus = tm_map(test_corpus, content_transformer(removePunctuation))
test_corpus = tm_map(test_corpus, content_transformer(stripWhitespace))
test_corpus = tm_map(test_corpus, content_transformer(removeWords), stopwords("en"))

DTM_test = DocumentTermMatrix(test_corpus, list(dictionary=colnames(DTM_train)))
DTM_test

X_test = as.matrix(DTM_test)

predictions = NULL
for (i in 1:nrow(X_test)) {
  # get maximum Naive Bayes log probabilities
  max = -(Inf)
  author = NULL
  for (j in 1:nrow(w_all)) {
    result = sum(w_all[j,]*X_test[i,])
    if (result > max) {
      max = result
      author = rownames(w_all)[j]
    }
  }
  predictions = append(predictions, author)
}

predict_results = table(test_labels,predictions)
predict_results
correct = NULL
for (i in 1:nrow(predict_results)) {
  correct = append(correct, predict_results[i, i])
}

correct_by_author = data.frame(author_names, correct)
correct_by_author

### ^^^ test corpus


library(randomForest)

rf.fit = randomForest(x = X_train, y = as.factor(labels), mtry=10, ntree=200)
rf.pred = predict(rf.fit, data=X_test)

rf_results = table(test_labels, rf.pred)
rf_correct = NULL
for (i in 1:nrow(rf_results)) {
  rf_correct = append(rf_correct, rf_results[i, i])
}

rf_correct_by_author = data.frame(rf_correct, row.names = author_names)
rf_correct_by_author
sum(rf_correct_by_author)/2500



# practice with association rule mining

groceries <- read.transactions("~/Documents/UT/Academic/Predictive2/STA380/data/groceries.txt", format = 'basket', sep = ',')
grocrules <- apriori(groceries, parameter=list(support=.005, confidence=.5, maxlen=8))
inspect(grocrules)


