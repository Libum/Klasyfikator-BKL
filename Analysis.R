#Zliczanie ilości znaków w zdaniach bazowych + przypisanie tych zliczen do bazy danych

sko2014$Liczba_znakow = as.numeric(sapply(sko2014$Skojarzenie, nchar))
sko2015$Liczba_znakow = as.numeric(sapply(sko2015$skojarzenie, nchar))
uza2014$Liczba_znakow = as.numeric(sapply(uza2014$Uzasadnienie, nchar))
uza2015$Liczba_znakow = as.numeric(sapply(uza2015$uzasadnienie, nchar))

#Funkcja identify_long, identyfikująca długie ciągi znaków bez spacji (jako output daje wektor logiczny)

identify_long = function(sentence, n){
        test_whitespace = grepl(pattern = "[^ ] [^ ]", x = sentence)
        test_length = nchar(sentence) > n
        if ((test_length==TRUE) & (test_whitespace==FALSE)){
                return(TRUE)
        }
        else {
                return(FALSE)
        }
}

#Dodanie do baz danych identyfikatorów wspomnianych wyżej ciągów

sko2014$Long = identify_long(sko2014$Lematy, n = 15)
sko2014$Long = identify_long(sko2014$Lematy, n = 15)
sko2014$Long = identify_long(sko2014$Lematy, n = 15)
sko2014$Long = identify_long(sko2014$Lematy, n = 15)