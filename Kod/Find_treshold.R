find_treshold = function(class_prediction, true_classes, start, stop, measure = "Kappa"){
        library(caret)
        treshold = NULL
        result = 0
        for (i in seq(start, stop, by = 0.01)){
                x = confusionMatrix(data = class_prediction > i, true_classes)
                kappa = as.vector(x$overall[measure])
                if (kappa > result){
                        treshold = i
                        result = kappa
                }
        }
        z = c(treshold, result)
        names(z) = c("treshold", measure)
        z
}