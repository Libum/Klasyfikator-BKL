#Zliczanie ilości znaków w zdaniach bazowych + przypisanie tych zliczen do bazy danych

sko2014$Liczba_znakow = as.numeric(sapply(sko2014$Skojarzenie, nchar))
sko2015$Liczba_znakow = as.numeric(sapply(sko2015$skojarzenie, nchar))
uza2014$Liczba_znakow = as.numeric(sapply(uza2014$Uzasadnienie, nchar))
uza2015$Liczba_znakow = as.numeric(sapply(uza2015$uzasadnienie, nchar))

#Funkcja, identyfikująca długie ciągi znaków bez spacji (jako output daje wektor logiczny)

exclude = function(sentence, n){
        test_whitespace = grepl(pattern = "[^ ] [^ ]", x = sentence)
        test_length = nchar(sentence) > n
        if ((test_length==TRUE) & (test_whitespace==FALSE)){
                return(TRUE)
        }
        else {
                return(FALSE)
        }
}