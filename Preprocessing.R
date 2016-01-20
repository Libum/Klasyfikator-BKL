#Wczytanie słowników do preprocessingu

stopwords = readLines("Polish_stopwords.txt", encoding = "UTF-8")
stopwords = tolower(stopwords)
stopwords = stri_trans_general(stopwords, "latin-ascii")

library(data.table)
library(hash)
slownik = fread(input = "polimorf-20151020.tab", select = c(1,2), encoding = "UTF-8", stringsAsFactors = FALSE)
colnames(slownik) = c("indeks","lemat")
slownik$indeks = stri_trans_general(slownik$indeks, "latin-ascii")
lematy = hash(slownik$indeks, slownik$lemat)
rm(slownik)

wulgaryzmy = read.csv(file = "Wulgaryzmy_1by1row.csv", stringsAsFactors = FALSE, header = FALSE, encoding = "UTF-8")
names(wulgaryzmy) = "slowo"
wulgaryzmy = as.vector(wulgaryzmy)
wulgaryzmy = stri_trans_general(wulgaryzmy, "latin-ascii")
 
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

Corpus_sko2014 = tm_map(Corpus_sko2014, removeNumbers)
Corpus_sko2015 = tm_map(Corpus_sko2015, removeNumbers)
Corpus_uza2014 = tm_map(Corpus_uza2014, removeNumbers)
Corpus_uza2015 = tm_map(Corpus_uza2015, removeNumbers)

Corpus_sko2014 = tm_map(Corpus_sko2014, removePunctuation)
Corpus_sko2015 = tm_map(Corpus_sko2015, removePunctuation)
Corpus_uza2014 = tm_map(Corpus_uza2014, removePunctuation)
Corpus_uza2015 = tm_map(Corpus_uza2015, removePunctuation)

Corpus_sko2014 = tm_map(Corpus_sko2014, content_transformer(stri_trans_general), "latin-ascii")
Corpus_sko2015 = tm_map(Corpus_sko2015, content_transformer(stri_trans_general), "latin-ascii")
Corpus_uza2014 = tm_map(Corpus_uza2014, content_transformer(stri_trans_general), "latin-ascii")
Corpus_uza2015 = tm_map(Corpus_uza2015, content_transformer(stri_trans_general), "latin-ascii")

Corpus_sko2014 = tm_map(Corpus_sko2014, removeWords, stopwords)
Corpus_sko2015 = tm_map(Corpus_sko2015, removeWords, stopwords)
Corpus_uza2014 = tm_map(Corpus_uza2014, removeWords, stopwords)
Corpus_uza2015 = tm_map(Corpus_uza2015, removeWords, stopwords)

Corpus_sko2014 = tm_map(Corpus_sko2014, stripWhitespace)
Corpus_sko2015 = tm_map(Corpus_sko2015, stripWhitespace)
Corpus_uza2014 = tm_map(Corpus_uza2014, stripWhitespace)
Corpus_uza2015 = tm_map(Corpus_uza2015, stripWhitespace)

#Lematyzacja (funkcja lemmatize) na korpusie stworzonym w tm, jako output daje wektor ze zlematyzowanymi zdaniami


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
        sentence = strsplit(x = sentence, split = " ")
        for(word in sentence[[1]]){
                lemat = lematy[[word]]
                test = as.vector(is.null(lemat))
                if(test == TRUE){final_sentence = paste(final_sentence, word)}
                else{final_sentence = paste(final_sentence, lemat)}
        }
        result = substr(x = final_sentence, start = 2, stop = nchar(final_sentence))
        if (length(result)==0){
                result = ""
        }
        result
}


lemmatize = function(corpus){
        text = extract_text(corpus)
        index = seq(1, length(text), by = 1)
        result = vector(length = max(index))
        for (i in index) { 
                sentence = text[[i]]
                final_sentence = insert_lemats(sentence)
                result[i] = final_sentence
        }
        result
}

#Tworzenie bi-gramów

library(RWeka)
options(mc.cores=1)
BigramTokenizer = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
DTM_uza2014 = DocumentTermMatrix(Corpus_uza2014, control = list(tokenize = BigramTokenizer)) #przykład zastosowania
DTM_uza2014 = removeSparseTerms(DTM_uza2014, sparse = 0.99)



