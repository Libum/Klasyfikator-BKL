#Celem tej części kodu jest dobór odpowiednich cytatów w oparciu o wyniki klasyfikatora oraz stopień podobieństwa między nimi
library(proxy)

rank = function(kat, mar, x){
        data = subset(x = x, Kategoria == kat & Marka_name == mar)
        data = data[order(data$Predykcja, decreasing = TRUE),]
        data = unique(data)
        data
}

compute_distance = function(x, type = "cosine"){
        DTM = as.matrix(DocumentTermMatrix(Corpus(VectorSource(x))))
        d = dist(DTM, method = type)
        as.vector(d)
}

exclude_similar = function(ranked, distance = 0.4){
        x = ranked
        repeat{
                test = FALSE
                for (i in 1:3){
                        for (j in setdiff(c(1,2,3), i)){
                               dist = compute_distance(x$Zdanie[c(i,j)]) 
                               if (dist < distance){
                                       x = x[-min(c(i,j)),]
                                       test = TRUE
                               }
                        }
                }
                if (test == FALSE){break}
        }
        x
}

write_results = function(final_rank){
        result = character(length = 6)
        result[1] = paste0(final_rank$Kategoria_name[1], ": ", final_rank$Marka_name[1])
        result[2] = ""
        result[3] = final_rank$Zdanie[1]
        result[4] = final_rank$Zdanie[2]
        result[5] = final_rank$Zdanie[3]
        result[6] = ""
        result
}

final_ranking = function(data, kategoria, marka){
        if (length(kategoria) != length(marka)){
                stop("Liczba kategorii i marek nie jest równa")
        }
        final = character()
        for (i in 1:length(kategoria)){
                x = rank(x = data, kat = kategoria[i], mar = marka[i])
                x = exclude_similar(x)
                x = write_results(x)
                final = c(final, x)
        }
        data.frame(X1 = final)
}