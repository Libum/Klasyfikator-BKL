#Wczytywanie danych

library(xlsx)
uza2014 = read.xlsx2("KLJ2014_uzasadnieniaALL.xlsx", sheetIndex = 1)
sko2014 = read.xlsx2("KLJ2014_skojarzeniaALL.xlsx", sheetIndex = 1)
uza2015 = read.xlsx2("KLJ__2015_uzasadnienia_ALL.xlsx", sheetIndex = 1)
sko2015 = read.xlsx2("KLJ__2015_skojarzenia_ALL.xlsx", sheetIndex = 1)

#Formatowanie zmiennych
uza2014$Uzasadnienie = as.character(uza2014$Uzasadnienie)
sko2014$Skojarzenie = as.character(sko2014$Skojarzenie)
uza2015$uzasadnienie = as.character(uza2015$uzasadnienie)
sko2015$skojarzenie = as.character(sko2015$skojarzenie)

#Usuwanie pustych zmiennych powstałych przy wczytywaniu
sko2015 = sko2015[,c(1:3,5)]
uza2015 = uza2015[,c(1:3,5)]

