library(tm)
library(magrittr)
library(caret)
library(e1071)
#setwd('D:\\Personal\\RCode\\sms-spam-collection-dataset')

inputData <- read.csv("spam.csv" , header = T)
head(inputData)

inputData <- inputData[,c(1,2)]

smsText <- as.vector(inputData$v2 )

head(smsText)

corpus <- VCorpus(VectorSource(smsText)) 
dtm <- corpus  %>% 
  tm_map(tolower) %>%
  tm_map(stemDocument)  %>%
  tm_map(removeWords, stopwords("English")) %>% 
  tm_map(removePunctuation) %>%
  tm_map(PlainTextDocument) %>%
  DocumentTermMatrix(control = list(wordLengths = c(4,10)))



dtmMat <- as.matrix(dtm)
x <- sample(nrow(dtm),0.80 * nrow(dtm) )

trainData <- dtm[c(x),]
testData <-  dtm[-c(x),]

fit <-  svm(x = trainData, y = inputData[x,]$v1 ,kernel = 'linear')

preds <-  predict(fit , newdata = testData)

#predsProb <-  predict(fit , newdata = testData ,probability=TRUE)
#predictiionS <- prediction(predsProb , inputData[-c(x),1])

cmatrix <-confusionMatrix(preds , inputData[-c(x),1])
