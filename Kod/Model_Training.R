library(caTools)
library(e1071)
library(caret)
library(tm)
library(RWeka)
library(glmnet)
library(ROCR)
library(Boruta)


#Kod do selekcji istotnych zmiennych - lematów i tagów gramatycznych - w oparciu o algorytm Boruta

create_sets(train = uza2014, test = uza2015, sparsity = 0.999, bigram = c(FALSE,TRUE))

boruta.tag = Boruta(Wynik~., data=train_tag, maxRuns = 20, doTrace = 1)
boruta.tag = TentativeRoughFix(boruta.tag)

boruta.lem = Boruta(Wynik~., data=train_lem, maxRuns = 20, doTrace = 1)
boruta.lem = TentativeRoughFix(boruta.lem)

ImpAttr = c(getSelectedAttributes(boruta.lem), getSelectedAttributes(boruta.tag), "Wynik")
train_both = train_both[,ImpAttr]
test_both = test_both[,ImpAttr]
train_both$Wynik = as.factor(train_both$Wynik)
test_both$Wynik = as.factor(test_both$Wynik)

#Budowa modelu SVM - uczenie w oparciu wyłącznie o "dobre cytaty" oraz w oparciu o obie klasy. 
#Wartość gamma została wybrana w opraciu o wyniki walidacji krzyżowej.

train_Good = subset(train_both, Wynik==TRUE)

train_independent = subset(train_Good, select = -Wynik)
train_dependent = subset(train_Good, select = Wynik)

modelSVM_onlyGood = svm(y = train_dependent, x = train_dependent, data = train,
                    type = "one-classification", gamma = 0.01923077)


modelSVM = svm(Wynik~., data = train_both, probability = TRUE, gamma = 0.01923077)
pred = predict(modelSVM, newdata = test_both, probability = TRUE)
pred = attr(x = pred, which = "probabilities")
pred = pred[,2]

ROCRpred = prediction(predictions = pred, labels = test_both$Wynik)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, main = "SVM on uza test")
performance(ROCRpred, "auc")@y.values

#Budowa modelu regresji logistycznej

modelGLM = glm(Wynik~., data = train_both, family = "binomial")
pred = predict(modelGLM, newdata = test_both, type = "response")

ROCRpred = prediction(predictions = pred, labels = test_both$Wynik)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, main = "GLM on uza test")
performance(ROCRpred, "auc")@y.values

#Budowa modelu regresji logistycznej karanej typu Lasso

create_sets(train = uza2014, test = uza2015, sparsity = 0.999)

library(glmnet)

train_both = train_both[,intersect(names(train_both), names(test_both))]
test_both = test_both[,intersect(names(train_both), names(test_both))]

train_both$Liczba_znakow = uza_all_train[index_train,]$Liczba_znakow
train_both$Liczba_znakow2 = uza_all_train[index_train,]$Liczba_znakow^2
train_both$Liczba_znakow3 = uza_all_train[index_train,]$Liczba_znakow^3
train_both$Jest_Marka = uza_all_train[index_train,]$Jest_Marka

test_both$Liczba_znakow = uza2016[index_test,]$Liczba_znakow
test_both$Liczba_znakow2 = uza2016[index_test,]$Liczba_znakow^2
test_both$Liczba_znakow3 = uza2016[index_test,]$Liczba_znakow^3
test_both$Jest_Marka = uza2016[index_test,]$Jest_Marka

model = cv.glmnet(y = train_both$Wynik, x = as.matrix(train_both[,-grep(pattern = "Wynik", x = names(train_both))]),
                        family="binomial",nfolds = 10)
                  
pred = predict.cv.glmnet(model, newx = as.matrix(test_both[, -grep(pattern = "Wynik", x = names(train_both))]),
                         type = "response")

ROCRpred = prediction(predictions = pred, labels = test_both$Wynik)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize = TRUE, main = "Lasso on uza test")
performance(ROCRpred, "auc")@y.values