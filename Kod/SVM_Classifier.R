library(caTools)
library(e1071)
library(caret)
library(tm)
library(RWeka)

x = subset(sko2014, Vulg == FALSE)
index_long_flag = ifelse(x$Long==TRUE & x$Flaga==TRUE, FALSE, TRUE)
x = subset(x, index_long_flag == TRUE)
rm(index_long_flag)

Corpus_sko2014 = Corpus(VectorSource(x$Tagi))
options(mc.cores=1)
BigramTokenizer = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
DTM_sko2014 = DocumentTermMatrix(Corpus_sko2014, control = list(tokenize = BigramTokenizer))
DTM_sko2014 = as.matrix(DTM_sko2014)
DTM_sko2014 = as.data.frame(DTM_sko2014)
DTM_sko2014$Wynik = x$Wynik

index = sample.split(DTM_sko2014$Wynik, SplitRatio = 0.7)
train = DTM_sko2014[index==TRUE,]
test = DTM_sko2014[index==FALSE,]
train_Good = subset(train, Wynik==TRUE)

train_independent = subset(train_Good, select = -Wynik)
train_dependent = subset(train_Good, select = Wynik)

model = svm(x = train_independent, y = train_dependent, type = "one-classification", nu = 0.2, cross = 5, kernel = "linear")
