#Ogólna funkcja "zbiorcza" do preprocessingu - do późniejszego użycia, domyśle nie usuwa najczętszych wyrazów
#i nie przeprowadza lematyzacji

Preprocess = function(text, lemmat = FALSE, stopWords = FALSE, tolower = TRUE, removeNumbers = TRUE,
                      punctuation = TRUE, ASCII = TRUE, stripWhite = TRUE){

        library(tm)
        library(stringi)
        library(stringr)

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

        text = Corpus(VectorSource(x = text))
        
        if(ASCII == TRUE) text = tm_map(text, content_transformer(stringi::stri_trans_general), "latin-ascii")
        if(tolower == TRUE) text = tm_map(text, content_transformer(base::tolower))
        if(punctuation == TRUE) text = tm_map(text, content_transformer(base::gsub), pattern = "[[:punct:]]", replacement = " ")
        if(removeNumbers == TRUE) text = tm_map(text, tm::removeNumbers)
        if(stopWords == TRUE) text = tm_map(text, tm::removeWords, stopwords)
        if(stripWhite == TRUE) text = tm_map(text, tm::stripWhitespace)

        if (lemmat == TRUE){
                text = lemmatize(text)
                text = text$Lematy
                flagi = text$Flaga
                text = as.character(text)
                return(data.frame(Lematy = text, Flagi = flagi))
        }
        extract_text(text)
}