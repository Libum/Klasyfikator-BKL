#Funckcja upraszczająca tagi (sprowadza je do podstawowych klas: rzeczownik, czasownik, przymiotnik itp.)

simplify_tags = function(sentence){
        sentence = gsub(x = sentence, pattern = "depr", replacement = "subst")
        sentence = gsub(x = sentence, pattern = "(adja)|(adjp)", replacement = "adj")
        sentence = gsub(x = sentence,
                        pattern = "(fin)|(nbedzie)|(naglt)|(npraet)|(nimpt)|(nimps)|(ninf)|
                        (npcon)|(npant)|(nger)|(npact)|(nppas)|(winien)",
                        replacement = "verb")
        sentence = gsub(x = sentence, pattern = "(adja)|(adjp)", replacement = "adj")
        sentence = gsub(x = sentence, pattern = "(ppron12)|(ppron3)|(siebie)", replacement = "ppron")
}

sko2014$Tagi = sapply(sko2014$Tagi, FUN = simplify_tags)
sko2015$Tagi = sapply(sko2015$Tagi, FUN = simplify_tags)
uza2014$Tagi = sapply(uza2014$Tagi, FUN = simplify_tags)
uza2015$Tagi = sapply(uza2015$Tagi, FUN = simplify_tags)

#Stworzenie n-gramów przy użyciu tagów

library(RWeka)
library(tm)

Corpus_uza2014 = Corpus(VectorSource(uza2014$Tagi))

options(mc.cores=1)
BigramTokenizer = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
DTM_uza2014 = DocumentTermMatrix(Corpus_uza2014, control = list(tokenize = BigramTokenizer))