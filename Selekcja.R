#Kod do selekcji istotnych zmiennych - tagi

library(Boruta)

boruta.tag = Boruta(Wynik~., data=train_tag, maxRuns = 20, doTrace = 1)
boruta.tag = TentativeRoughFix(boruta.tag)

boruta.lem = Boruta(Wynik~., data=train_lem, maxRuns = 20, doTrace = 1)
boruta.lem = TentativeRoughFix(boruta.lem)