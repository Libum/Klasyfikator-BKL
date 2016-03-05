#' Funkcja create_sets służy do wygodnego tworzenia zbiorów testowych i treningowych dla algorytmów opartych na 
#' lematach, tagach gramatycznych lub zawierających oba. 


create_sets = function(train, test, sparsity = 0.99, type = "both", all = TRUE, bigram = c(FALSE,FALSE)){
        library(tm)
        library(RWeka)
        train = subset(train, Vulg == FALSE)
        index_long_flag = ifelse(train$Long==TRUE & train$Flaga==TRUE, FALSE, TRUE)
        train = subset(train, index_long_flag == TRUE)
        train = subset(train, (train$Lematy == "")==FALSE)
        train = subset(train, Min_One_Lem == TRUE)
        rm(index_long_flag)
        .GlobalEnv$index_train = row.names(train)
        
        test = subset(test, Vulg == FALSE)
        index_long_flag = ifelse(test$Long==TRUE & test$Flaga==TRUE, FALSE, TRUE)
        test = subset(test, index_long_flag == TRUE)
        test = subset(test, (test$Lematy == "")==FALSE)
        test = subset(test, Min_One_Lem == TRUE)
        rm(index_long_flag)
        .GlobalEnv$index_test = row.names(test)

        options(mc.cores=1)
        BigramTokenizer = function(x) {NGramTokenizer(x, Weka_control(min = 2, max = 2))}
        control_lem = ifelse(bigram[1] == TRUE, parse(text = "list(tokenize = BigramTokenizer)"), parse(text = "list()"))
        control_tag = ifelse(bigram[2] == TRUE, parse(text = "list(tokenize = BigramTokenizer)"), parse(text = "list()"))
        
        if (type == "lem" | type == "both"){
                Corpus_train_lem = Corpus(VectorSource(train$Lematy))
                DTM_train_lem = DocumentTermMatrix(Corpus_train_lem, control = eval(control_lem))
                DTM_train_lem = removeSparseTerms(DTM_train_lem, sparse = sparsity)
                DTM_train_lem = as.matrix(DTM_train_lem)
                DTM_train_lem = as.data.frame(DTM_train_lem)
                DTM_train_lem$Wynik = train$Wynik
                DTM_train_lem$Wynik = as.factor(DTM_train_lem$Wynik)
                names(DTM_train_lem) = make.names(names(DTM_train_lem))
                
                Corpus_test_lem = Corpus(VectorSource(test$Lematy))
                DTM_test_lem = DocumentTermMatrix(Corpus_test_lem, control = eval(control_lem))
                DTM_test_lem = removeSparseTerms(DTM_test_lem, sparse = sparsity)
                DTM_test_lem = as.matrix(DTM_test_lem)
                DTM_test_lem = as.data.frame(DTM_test_lem)
                DTM_test_lem$Wynik = test$Wynik
                DTM_test_lem$Wynik = as.factor(DTM_test_lem$Wynik)
                names(DTM_test_lem) = make.names(names(DTM_test_lem))
        }
        if (type == "tag" | type == "both"){
                Corpus_train_tag = Corpus(VectorSource(train$Tagi))
                DTM_train_tag = DocumentTermMatrix(Corpus_train_tag, control = eval(control_tag))
                DTM_train_tag = removeSparseTerms(DTM_train_tag, sparse = sparsity)
                DTM_train_tag = as.matrix(DTM_train_tag)
                DTM_train_tag = as.data.frame(DTM_train_tag)
                DTM_train_tag$Wynik = train$Wynik
                DTM_train_tag$Wynik = as.factor(DTM_train_tag$Wynik)
                names(DTM_train_tag) = make.names(names(DTM_train_tag))
                
                Corpus_test_tag = Corpus(VectorSource(test$Tagi))
                DTM_test_tag = DocumentTermMatrix(Corpus_test_tag, control = eval(control_tag))
                DTM_test_tag = removeSparseTerms(DTM_test_tag, sparse = sparsity)
                DTM_test_tag = as.matrix(DTM_test_tag)
                DTM_test_tag = as.data.frame(DTM_test_tag)
                DTM_test_tag$Wynik = test$Wynik
                DTM_test_tag$Wynik = as.factor(DTM_test_tag$Wynik)
                names(DTM_test_tag) = make.names(names(DTM_test_tag)) 
        }
        if (type == "tag"){
                .GlobalEnv$train_tag = DTM_train_tag
                .GlobalEnv$test_tag = DTM_test_tag
                rm(list = c("DTM_train_tag", "DTM_test_tag", "Corpus_train_tag", "Corpus_test_tag"))
        }
        if (type == "lem"){
                .GlobalEnv$train_lem = DTM_train_lem
                .GlobalEnv$test_lem = DTM_test_lem
                rm(list = c("DTM_train_lem", "DTM_test_lem", "Corpus_train_lem", "Corpus_test_lem"))
        }
        if (type == "both"){
                .GlobalEnv$train_tag = DTM_train_tag
                .GlobalEnv$test_tag = DTM_test_tag
                .GlobalEnv$train_lem = DTM_train_lem
                .GlobalEnv$test_lem = DTM_test_lem
                train_lem$Wynik = NULL
                test_lem$Wynik = NULL
                .GlobalEnv$train_both = cbind(train_lem, train_tag)
                .GlobalEnv$test_both = cbind(test_lem, test_tag)
                .GlobalEnv$train_lem$Wynik = train_tag$Wynik
                .GlobalEnv$test_lem$Wynik = test_tag$Wynik
                rm(list = c("DTM_train_tag", "DTM_test_tag", "Corpus_train_tag", "Corpus_test_tag"))
                rm(list = c("DTM_train_lem", "DTM_test_lem", "Corpus_train_lem", "Corpus_test_lem"))
                if (all == FALSE){
                        rm(list = c("train_lem", "test_lem", "train_tag", "test_tag"))
                }
        }
}