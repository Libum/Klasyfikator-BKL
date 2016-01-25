#Wczytanie słowników do preprocessingu

library(stringi)
library(stringr)
library(data.table)
library(hash)

stopwords = readLines("Polish_stopwords.txt", encoding = "UTF-8")
stopwords = tolower(stopwords)
stopwords = stri_trans_general(stopwords, "latin-ascii")

slownik = fread(input = "polimorf-20151020.tab", select = c(1,2,3), encoding = "UTF-8", stringsAsFactors = FALSE, skip = 41)
colnames(slownik) = c("indeks","lemat","tag")
slownik$indeks = stri_trans_general(slownik$indeks, "latin-ascii")
lematy = hash(slownik$indeks, slownik$lemat)
tagi = hash(slownik$indeks, slownik$tag)
rm(slownik)

wulgaryzmy = read.csv(file = "Wulgaryzmy_1by1row.csv", stringsAsFactors = FALSE, header = FALSE)
colnames(wulgaryzmy) = "slowo"
wulgaryzmy = wulgaryzmy$slowo
wulgaryzmy = stri_trans_general(wulgaryzmy, "latin-ascii")
names(wulgaryzmy) = wulgaryzmy
 
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

Corpus_sko2014 = tm_map(Corpus_sko2014, content_transformer(gsub), pattern = "[[:punct:]]", replacement = " ")
Corpus_sko2015 = tm_map(Corpus_sko2015, content_transformer(gsub), pattern = "[[:punct:]]", replacement = " ")
Corpus_uza2014 = tm_map(Corpus_uza2014, content_transformer(gsub), pattern = "[[:punct:]]", replacement = " ")
Corpus_uza2015 = tm_map(Corpus_uza2015, content_transformer(gsub), pattern = "[[:punct:]]", replacement = " ")

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

#Lematyzacja (funkcja lemmatize) na korpusie stworzonym w tm, jako output daje ramkę danych z dwoma zmiennymi:
#' Lematy - zlematyzowane zdania (wektor tekstowy)
#' Flaga - wektor logiczny, okreslający, czy w danym zdaniu znalazły się słowa, które nie istniały w słowniku 
#' lematyzacyjnym (Morfologik).


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
        flag = FALSE
        for(word in sentence[[1]]){
                lemat = lematy[[word]]
                test = as.vector(is.null(lemat))
                if(test == TRUE){
                        final_sentence = paste(final_sentence, word)
                        flag = TRUE
                        }
                else{
                        final_sentence = paste(final_sentence, lemat)
                        }
        }
        result = substr(x = final_sentence, start = 2, stop = nchar(final_sentence))
        if (length(result)==0){
                result = ""
        }
        return(c(result,flag))
}


lemmatize = function(corpus){
        text = extract_text(corpus)
        index = seq(1, length(text), by = 1)
        result = vector(length = max(index))
        flags = vector(length = max(index))
        for (i in index) { 
                sentence = text[[i]]
                final = insert_lemats(sentence)
                result[i] = final[1]
                flags[i] = final[2]
        }
        df = data.frame(Lematy = result, Flaga = as.logical(flags))
        return(df)
}

#Funkcja insert_tags jako input przyjmuje zdanie (wektor tekstowy), a jako output daje wektor tekstowy z tagami 
#gramatycznymi odpowiadającymi poszczególnym słowom. Słowa niemożliwe do otagowania zostają pominięte. W przypadku
#tagowania w preprocessingu pominięto etap usuwania stopwords, gdyż mogą się one okazać przydatne.

insert_tags = function(sentence){
        tags = character()
        sentence = strsplit(x = sentence, split = " ")
        for (word in sentence[[1]]){
                tag = tagi[[word]]
                test = as.vector(is.null(tag))
                if (test == FALSE){
                        end = regexpr(pattern = ":", text =  tag)[1]
                        if (end == -1){
                                tags = paste(tags, tag)
                        } else {
                        tag = substr(x = tag, start = 1, stop = end-1)
                        tags = paste(tags, tag)
                        }
                }
        }
        result = substr(x = tags, start = 2, stop = nchar(tags))
        if (length(result)==0){
                result = ""
        }
        result
}

#Zapisywanie zlematyzowanych zdan, tagów oraz flag do bazy danych

sko2014_lematyzacja = lemmatize(Corpus_sko2014)
sko2015_lematyzacja = lemmatize(Corpus_sko2015)
uza2014_lematyzacja = lemmatize(Corpus_uza2014)
uza2015_lematyzacja = lemmatize(Corpus_uza2015)

sko2014$Lematy = sko2014_lematyzacja$Lematy
sko2015$Lematy = sko2015_lematyzacja$Lematy
uza2014$Lematy = uza2014_lematyzacja$Lematy
uza2015$Lematy = uza2015_lematyzacja$Lematy

sko2014$Lematy = as.character(sko2014$Lematy)
sko2015$Lematy = as.character(sko2015$Lematy)
uza2014$Lematy = as.character(uza2014$Lematy)
uza2015$Lematy = as.character(uza2015$Lematy)

sko2014$Flaga = sko2014_lematyzacja$Flaga
sko2015$Flaga = sko2015_lematyzacja$Flaga
uza2014$Flaga = uza2014_lematyzacja$Flaga
uza2015$Flaga = uza2015_lematyzacja$Flaga

sko2014$Tagi = sapply(X = extract_text(Corpus_sko2014), FUN = insert_tags)
sko2015$Tagi = sapply(X = extract_text(Corpus_sko2015), FUN = insert_tags)
uza2014$Tagi = sapply(X = extract_text(Corpus_uza2014), FUN = insert_tags)
uza2015$Tagi = sapply(X = extract_text(Corpus_uza2015), FUN = insert_tags)

save(sko2014, file = "./Bazy_danych/sko2014.Rda")
save(sko2015, file = "./Bazy_danych/sko2015.Rda")
save(uza2014, file = "./Bazy_danych/uza2014.Rda")
save(uza2015, file = "./Bazy_danych/uza2015.Rda")

rm(sko2014_lematyzacja)
rm(sko2015_lematyzacja)
rm(uza2014_lematyzacja)
rm(uza2015_lematyzacja)

#Tworzenie bi-gramów

#library(RWeka)
#options(mc.cores=1)
#BigramTokenizer = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
#DTM_uza2014 = DocumentTermMatrix(Corpus_uza2014, control = list(tokenize = BigramTokenizer)) #przykład zastosowania
#DTM_uza2014 = removeSparseTerms(DTM_uza2014, sparse = 0.99)



