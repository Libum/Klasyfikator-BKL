#Wczytanie słowników do preprocessingu

stopwords = readLines("Polish_stopwords.txt")
stopwords = tolower(stopwords)

library(data.table)
slownik = fread(input = "polimorf-20151020.tab", select = c(1,2), encoding = "UTF-8")
colnames(slownik) = c("indeks","lemat")
lematy = slownik$lemat
names(lematy) = slownik$indeks
 
#Przygotowanie danych tekstowych do analizy

library(tm)

Corpus_sko2014 = Corpus(VectorSource(x = sko2014$Skojarzenie))
Corpus_sko2015 = Corpus(VectorSource(x = sko2015$skojarzenie))
Corpus_uza2014 = Corpus(VectorSource(x = uza2014$Uzasadnienie))
Corpus_uza2015 = Corpus(VectorSource(x = uza2015$uzasadnienie))

Corpus_sko2014 = tm_map(Corpus_sko2014, content_transformer(tolower))
Corpus_sko2015 = tm_map(Corpus_sko2015, content_transformer(tolower))
Corpus_uza2014 = tm_map(Corpus_uza2014, content_transformer(tolower))
Corpus_uza2015 = tm_map(Corpus_uza2015, content_transformer(tolower))

Corpus_sko2014 = tm_map(Corpus_sko2014, stripWhitespace)
Corpus_sko2015 = tm_map(Corpus_sko2015, stripWhitespace)
Corpus_uza2014 = tm_map(Corpus_uza2014, stripWhitespace)
Corpus_uza2015 = tm_map(Corpus_uza2015, stripWhitespace)

Corpus_sko2014 = tm_map(Corpus_sko2014, removePunctuation)
Corpus_sko2015 = tm_map(Corpus_sko2015, removePunctuation)
Corpus_uza2014 = tm_map(Corpus_uza2014, removePunctuation)
Corpus_uza2015 = tm_map(Corpus_uza2015, removePunctuation)

Corpus_sko2014 = tm_map(Corpus_sko2014, removeWords, stopwords)
Corpus_sko2015 = tm_map(Corpus_sko2015, removeWords, stopwords)
Corpus_uza2014 = tm_map(Corpus_uza2014, removeWords, stopwords)
Corpus_uza2015 = tm_map(Corpus_uza2015, removeWords, stopwords)

#Lematyzacja (funkcja lemmatize) na korpusie stworzonym w tm, jako output daje wektor ze zlematyzowanymi zdaniami
# UWAGA gubi puste wektory

extract_text = function(corpus){
        text = as.vector(sapply(corpus, as.character)) #Wyciąga teksty z corpusu
        final = vector()
        for (i in text){
                test = grepl(x = i, pattern = "^ ") #Usuwa zbędne spacje na początku zdań
                if (test==TRUE){
                      final = cbind(final, substr(x = i, start = 2, stop = nchar(i)))  
                }
                else {
                        final = cbind(final, i)
                }
        }
        as.vector(final)
}

insert_lemats = function(sentence){
        final_sentence = character()
        sentence = strsplit(x = sentence, split = " ") #tu jest problem
        for(word in sentence[[1]]){
                lemat = as.character(lematy[word])
                test = as.vector(is.na(lemat))
                if(test == TRUE){final_sentence = paste(final_sentence, word)}
                else{final_sentence = paste(final_sentence, lemat)}
        }
        substr(x = final_sentence, start = 2, stop = nchar(final_sentence))
}


lemmatize = function(corpus){
        text = extract_text(corpus)
        names(text) = seq(1, length(text), by = 1)
        result = vector()
        for (name in names(text)) {                 
                sentence = text[[name]]
                final_sentence = insert_lemats(sentence)
                result = c(result, final_sentence)
        }
        result
}

#Tworzenie bi-gramów

library(RWeka)
options(mc.cores=1)
BigramTokenizer = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
DTM_uza2014 = DocumentTermMatrix(Corpus_uza2014, control = list(tokenize = BigramTokenizer))
DTM_uza2014 = removeSparseTerms(DTM_uza2014, sparse = 0.99)



