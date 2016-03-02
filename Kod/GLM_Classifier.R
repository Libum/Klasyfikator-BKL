library(caTools)
library(e1071)
library(caret)
library(tm)
library(RWeka)
library(glmnet)

x = subset(uza2015, Vulg == FALSE)
index_long_flag = ifelse(x$Long==TRUE & x$Flaga==TRUE, FALSE, TRUE)
x = subset(x, index_long_flag == TRUE)
x = subset(x, (x$Lematy == "")==FALSE)
x = subset(x, Min_One_Lem == TRUE)
rm(index_long_flag)

#Trening na tagach

Corpus_sko2014 = Corpus(VectorSource(x$Tagi))
options(mc.cores=1)
BigramTokenizer = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
DTM_sko2014 = DocumentTermMatrix(Corpus_sko2014, control = list(tokenize = BigramTokenizer))
DTM_sko2014 = removeSparseTerms(DTM_sko2014, sparse = 0.9999)
DTM_sko2014 = as.matrix(DTM_sko2014)
DTM_sko2014 = as.data.frame(DTM_sko2014)
DTM_sko2014$Wynik = x$Wynik
DTM_sko2014$Wynik = as.factor(DTM_sko2014$Wynik)
names(DTM_sko2014) = make.names(names(DTM_sko2014))

index = sample.split(DTM_sko2014$Wynik, SplitRatio = 0.7)
train_tag = DTM_sko2014[index==TRUE,]
test_tag = DTM_sko2014[index==FALSE,]

#test_uza = DTM_sko2014

#Trening na lematach

Corpus_sko2014 = Corpus(VectorSource(x$Lematy))
DTM_sko2014 = DocumentTermMatrix(Corpus_sko2014)
DTM_sko2014 = removeSparseTerms(DTM_sko2014, sparse = 0.9999)
DTM_sko2014 = as.matrix(DTM_sko2014)
DTM_sko2014 = as.data.frame(DTM_sko2014)
DTM_sko2014$Wynik = x$Wynik
DTM_sko2014$Wynik = as.factor(DTM_sko2014$Wynik)
names(DTM_sko2014) = make.names(names(DTM_sko2014))

train_lem = DTM_sko2014[index==TRUE,]
test_lem = DTM_sko2014[index==FALSE,]

#test_uza$Wynik = NULL; test_uza = cbind(test_uza, DTM_sko2014)

#Łączenie zbiorów

train_lem$Wynik = NULL
test_lem$Wynik = NULL

train = cbind(train_lem, train_tag)
test = cbind(test_lem, test_tag)
ImpAttr = c(getSelectedAttributes(boruta.lem), getSelectedAttributes(boruta.tag), "Wynik")
train = train[,ImpAttr]
test = test[,ImpAttr]
train$Wynik = as.factor(train$Wynik)
test$Wynik = as.factor(test$Wynik)

#Budowa modelu

library(glmnet)
model = cv.glmnet(y = train[,ncol(train)], x = as.matrix(train[,-ncol(train)]), family="binomial", nfolds = 10)
test = test[,intersect(names(train), names(test))]
pred = predict.cv.glmnet(model, newx = as.matrix(test[, -ncol(test)]), type = "response")

library(ROCR)
ROCRpred = prediction(predictions = pred, labels = test_uza$Wynik)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, main = "Lasso on uza test")