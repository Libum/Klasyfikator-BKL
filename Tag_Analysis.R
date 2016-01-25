#Stworzenie n-gramów przy użyciu tagów
library(RWeka)
library(tm)

Corpus_uza2014 = Corpus(VectorSource(uza2014$Tagi))

options(mc.cores=1)
BigramTokenizer = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
DTM_uza2014 = DocumentTermMatrix(Corpus_uza2014, control = list(tokenize = BigramTokenizer))