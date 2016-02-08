#Kod wczytujący dane z cytatami zakwalifikowanymi w poprzednich edycjach oraz bazy danych

load("~/Klasyfikator/Algorytm i dane/Bazy_danych/uza2015.Rda")
load("~/Klasyfikator/Algorytm i dane/Bazy_danych/uza2014.Rda")
load("~/Klasyfikator/Algorytm i dane/Bazy_danych/sko2015.Rda")
load("~/Klasyfikator/Algorytm i dane/Bazy_danych/sko2014.Rda")

library(stringi)
library(stringr)
library(xlsx)

Final_sko2015 = read.xlsx("KLJ__2015_skojarzenia_raport_FINAL.xlsx", 1, encoding = "UTF-8", header = FALSE)
Final_sko2015 = Final_sko2015[,1]
Final_sko2015 = as.data.frame(Final_sko2015)
colnames(Final_sko2015) = "zdanie"

Final_uza2015 = read.xlsx("KLJ__2015_uzasadnienia_FINAL.xlsx", 1, encoding = "UTF-8", header = FALSE)
Final_uza2015 = Final_uza2015[,1]
Final_uza2015 = as.data.frame(Final_uza2015)
colnames(Final_uza2015) = "zdanie"

Final_sko2014 = read.xlsx("klj2014_zestawienie_raportowe_skojarzenia.xlsx", 2, encoding = "UTF-8", header = FALSE)
Final_sko2014 = Final_sko2014[,1]
Final_sko2014 = as.data.frame(Final_sko2014)
colnames(Final_sko2014) = "zdanie"

Final_uza2014 = read.xlsx("klj2014_zestawienie_raportowe_uzasadnienia.xlsx", 3, encoding = "UTF-8", header = FALSE)
Final_uza2014 = Final_uza2014[,1]
Final_uza2014 = as.data.frame(Final_uza2014)
colnames(Final_uza2014) = "zdanie"

#Usuwanie zbędnej przestrzeni

Final_sko2015 = as.data.frame(Final_sko2015$zdanie[as.logical(as.numeric(is.na(Final_sko2015$zdanie))-1)])
colnames(Final_sko2015) = "zdanie"

Final_sko2014 = as.data.frame(Final_sko2014$zdanie[as.logical(as.numeric(is.na(Final_sko2014$zdanie))-1)])
colnames(Final_sko2014) = "zdanie"

Final_uza2014 = as.data.frame(Final_uza2014$zdanie[as.logical(as.numeric(is.na(Final_uza2014$zdanie))-1)])
colnames(Final_uza2014) = "zdanie"

Final_uza2015 = as.data.frame(Final_uza2015$zdanie[as.logical(as.numeric(is.na(Final_uza2015$zdanie))-1)])
colnames(Final_uza2015) = "zdanie"

#Preprocessing surowych danych

sko2014_raw = Preprocess(sko2014$Skojarzenie)
sko2015_raw = Preprocess(sko2015$skojarzenie)
uza2014_raw = Preprocess(uza2014$Uzasadnienie)
uza2015_raw = Preprocess(uza2015$uzasadnienie)

#Preprocessing danych wynikowych - zaklasyfikowanych zdań

Final_sko2015$zdanie = Preprocess(Final_sko2015$zdanie)
Final_uza2015$zdanie = Preprocess(Final_uza2015$zdanie)
Final_uza2014$zdanie = Preprocess(Final_uza2014$zdanie)
Final_sko2014$zdanie = Preprocess(Final_sko2014$zdanie)

#Usuwanie nazw kategorii - czy to ma sens?
#Final_sko2015$Kategoria = grepl(pattern = "^[A-Z]", x = Final_sko2015$zdanie)
#Final_sko2015 = subset(Final_sko2015, Kategoria == FALSE)
#Final_sko2015$Kategoria = NULL

#Stworzenie tablic hashujących do przeszukiwania
library(hash)
outcomes_sko2015 = hash(Final_sko2015$zdanie, Final_sko2015$zdanie)
outcomes_uza2015 = hash(Final_uza2015$zdanie, Final_uza2015$zdanie)
outcomes_uza2014 = hash(Final_uza2014$zdanie, Final_uza2014$zdanie)
outcomes_sko2014 = hash(Final_sko2014$zdanie, Final_sko2014$zdanie)

#Stworzenie funkcji sprawdzającej, czy dane zdanie zostało zakwalifikowane. Zdania identyczne z tymi, które zostały zakwalifikowane,
#także zostają zaliczone, nawet jeśli należą do innej kategorii.

check_outcome = function(sentence, outcomes){
        if (sentence == ""){
                FALSE
        } else {
                if (is.null(outcomes[[sentence]])){
                        FALSE
                } else {
                        TRUE
                }
        }
}

#Dodanie zmiennych wynikowych do baz danych
sko2015$Wynik = as.logical(sapply(X = sko2015_raw, FUN = check_outcome, outcomes = outcomes_sko2015))
sko2014$Wynik = as.logical(sapply(X = sko2014_raw, FUN = check_outcome, outcomes = outcomes_sko2014))
uza2014$Wynik = as.logical(sapply(X = uza2014_raw, FUN = check_outcome, outcomes = outcomes_uza2014))
uza2015$Wynik = as.logical(sapply(X = uza2015_raw, FUN = check_outcome, outcomes = outcomes_uza2015))


