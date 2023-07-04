#
# Copyright (C) 2013-2021 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 


.auditClassificationSetFormula <- function(options, jaspResults) {
  features <- options[["featPred"]]
  target <- options[["target"]]
  formula <- formula(paste(target, "~", paste(features, collapse = " + ")))
  jaspResults[["formula"]] <- createJaspState(formula)
  jaspResults[["formula"]]$dependOn(options = c("featPred", "target"))
}

.auditPerfMeasures <- function(testPredictions, testReal){
  #cf <- caret::confusionMatrix(data = as.factor(dataPredictions), reference=as.factor(dataset[, options[["target"]]]))$table
  #counts<- .auditFMGetCounts(cf)
    
  pred <- factor(testPredictions)
  real <- factor(testReal)
  lvls <- levels(as.factor(real))
  support <- rep(NA, length(lvls))
  accuracy <- rep(NA, length(lvls))
  precision <- rep(NA, length(lvls))
  recall <- rep(NA, length(lvls))
  f1 <- rep(NA, length(lvls))
  mcc <- rep(NA, length(lvls))
  #auc <- classificationResult[["auc"]]
  for (i in seq_along(lvls)) {
    TP <- as.numeric(length(which(pred == lvls[i] & real == lvls[i])))
    TN <- as.numeric(length(which(pred != lvls[i] & real != lvls[i])))
    FN <- as.numeric(length(which(pred != lvls[i] & real == lvls[i])))
    FP <- as.numeric(length(which(pred == lvls[i] & real != lvls[i])))
    support[i]  <- length(which(real == lvls[i]))
    accuracy[i] <- (TP + TN) / (TP + FN + FP + TN)
    precision[i] <- TP / (TP + FP)
    recall[i] <- TP / (TP + FN)
    f1[i] <- 2 * ((precision[i] * recall[i]) / (precision[i] + recall[i]))
    mcc[i] <- ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
  }
  support[length(support) + 1] <- sum(support, na.rm = TRUE)
  accuracy[length(accuracy) + 1] <- mean(accuracy, na.rm = TRUE)
  precision[length(precision) + 1] <- sum(precision * support[seq_along(lvls)], na.rm = TRUE) / sum(support[seq_along(lvls)], na.rm = TRUE)
  recall[length(recall) + 1] <- sum(recall * support[seq_along(lvls)], na.rm = TRUE) / sum(support[seq_along(lvls)], na.rm = TRUE)
  f1[length(f1) + 1] <- sum(f1 * support[seq_along(lvls)], na.rm = TRUE) / sum(support[seq_along(lvls)], na.rm = TRUE)
  mcc[length(mcc) + 1] <- mean(mcc, na.rm = TRUE)
  
  resultsDf<- data.frame(support, accuracy, precision, recall, f1, mcc)
  
  return(resultsDf)
}

.auditPredResult <- function(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name, dataset){
  result <- list()
  result[["formula"]] <- formula
  result[["model"]] <- fit
  result[["confTable"]] <- table("Pred" = testPredictions, "Real" = testSet[, options[["target"]]])
  result[["testAcc"]] <- sum(diag(prop.table(result[["confTable"]])))
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["classes"]] <- dataPredictions
  result[["testReal"]] <- testSet[, options[["target"]]]
  result[["testPred"]] <- testPredictions
  result[["test"]] <- testSet
  result[["perfMeasures"]] <- .auditPerfMeasures(result[["testReal"]],result[["testPred"]])

  jaspResults[[name]] <- createJaspState(result)
  jaspResults[[name]]$dependOn(options = c("target","featPred","group","svm","lr","rf", "predictBool", "testDataManual"))
}

.auditML<- function(dataset, jaspResults, options, trainingIndex, name){
  if (!is.null(jaspResults[[name]])) {return ()}
  formula <- jaspResults[["formula"]]$object
  trainingSet <- dataset[trainingIndex, ]
  
  # Create the generated test set indicator
  testIndicatorColumn <- rep(1, nrow(dataset))
  testIndicatorColumn[trainingIndex] <- 0
  
  # Just create a train and a test set (no optimization)
  testSet <- dataset[-trainingIndex, ]
  
  #check if name contains SVM
  if (grepl("svm", name, fixed = TRUE))
  {
    fit <- e1071::svm(formula, data = trainingSet, type = "C-classification",kernel = options[["weights"]], cost = options[["cost"]], tolerance = options[["tolerance"]],
                      epsilon = options[["epsilon"]], scale = FALSE, degree = options[["degree"]], gamma = options[["gamma"]], coef0 = options[["complexityParameter"]])
    
    # Use the specified model to make predictions for dataset
    testPredictions <- predict(fit, newdata = testSet, type="response")
    dataPredictions <- predict(fit, newdata = dataset, type="response")
    .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name, dataset)
  }
  if (grepl("rf", name, fixed = TRUE))
  {
    #trainingSet[[options[["target"]]]] <- as.factor(trainingSet[[options[["target"]]]])
    fit <- randomForest::randomForest(formula, data=trainingSet, ntree = options[["noOfTrees"]], mtry = options[["numberOfPredictors"]],
                                      sampsize = ceiling(options[["baggingFraction"]] * nrow(trainingSet)),
                                      importance = TRUE, keep.forest = TRUE)
    
    # Use the specified model to make predictions for dataset
    testPredictions <- predict(fit, newdata = testSet, type="response")
    dataPredictions <- predict(fit, newdata = dataset, type="response")
    .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name, dataset)
  }  
  if (grepl("lr", name, fixed = TRUE))
  {
    fit <- stats::glm(formula, family=binomial(link='logit'),data=trainingSet)
    
    testPredictions <- predict(fit, newdata = testSet, type="response")
    dataPredictions <- predict(fit, newdata = dataset, type="response")
    testPredictions <- ifelse(testPredictions > 0.5,1,0)
    dataPredictions <- ifelse(dataPredictions > 0.5,1,0)
    .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name, dataset)
  }
  # if (grepl("boost", name, fixed = TRUE))
  # {
  #   #if sensitive attribute is not a feature, delete it so we can rearrange the dataset
  #   trainingSet <- trainingSet[, names(trainingSet) != options[["group"]]]
  #   
  #   # gbm expects the columns in the data to be in the same order as the variables...
  #   trainingSet<- trainingSet[, match(names(trainingSet), all.vars(formula))]
  #   fit <- gbm::gbm(
  #     formula = formula, data = trainingSet, distribution = "multinomial", n.cores = 1, keep.data = TRUE,n.trees = options[["noOfTrees"]],
  #     shrinkage = options[["shrinkage"]], interaction.depth = options[["interactionDepth"]],
  #     bag.fraction = options[["baggingFraction"]], n.minobsinnode = options[["minObservationsInNode"]]
  #   )
  #   
  #   dataProbs <- gbm::predict.gbm(fit, newdata = dataset, type = "response")
  #   dataPredictions <- colnames(dataProbs)[apply(dataProbs, 1, which.max)]
  #   testPredictions <- dataPredictions[-trainingIndex]
  #   
  #   .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name, dataset)
  # }
}



.mlClassificationTableMetrics <- function(dataset, options, jaspResults, ready, name) {
  if (!is.null(jaspResults[["performanceMeasuresAll"]]) || !options[["performanceMeasuresAll"]]) {
    return()
  }

  if (grepl("svm", name, fixed = TRUE))
  {bestModel <- "SVM)"}
  if (grepl("lr", name, fixed = TRUE))
  {bestModel <- "LR)"}
  if (grepl("rf", name, fixed = TRUE))
  {bestModel <- "RF)"}
  
  table <- createJaspTable(title = gettext(paste("Evaluation Metrics of Best Model (", bestModel, sep="")))
  table$transpose <- TRUE
  #table$dependOn(options = c(.mlClassificationDependencies(options), "validationMeasures"))
  table$addColumnInfo(name = "group", title = "", type = "string")
  table$addColumnInfo(name = "support", title = gettext("Support"), type = "integer")
  table$addColumnInfo(name = "accuracy", title = gettext("Accuracy"), type = "number")
  table$addColumnInfo(name = "precision", title = gettext("Precision (Positive Predictive Value)"), type = "number")
  table$addColumnInfo(name = "recall", title = gettext("Recall (True Positive Rate)"), type = "number")
  table$addColumnInfo(name = "f1", title = gettext("F1 Score"), type = "number")
  table$addColumnInfo(name = "mcc", title = gettext("Matthews Correlation Coefficient"), type = "number")
  #table$addColumnInfo(name = "auc", title = gettext("Area Under Curve (AUC)"), type = "number")
  table$addFootnote(gettext("All metrics are calculated for every class against all other classes."))
  if (options[["target"]] != "") {
    table[["group"]] <- c(levels(factor(dataset[, options[["target"]]])), gettext("Average / Total"))
  }
  jaspResults[["performanceMeasuresAll"]] <- table
  if (!ready) {
    return()
  }
  
  classificationResult<- jaspResults[[name]]$object
  classificationResult <- classificationResult[["perfMeasures"]]
  print("hierzo")
  print(name)
  print(jaspResults[[name]]$obj)
  #auc[length(auc) + 1] <- mean(auc, na.rm = TRUE)
  table[["group"]] <- c(levels(factor(classificationResult[["test"]][, options[["target"]]])), "Average / Total") # fill again to adjust for missing categories
  table[["accuracy"]] <- classificationResult[["accuracy"]]
  table[["precision"]] <- classificationResult[["precision"]]
  table[["recall"]] <- classificationResult[["recall"]]
  table[["f1"]] <- classificationResult[["f1"]]
  table[["mcc"]] <- classificationResult[["mcc"]]
  table[["support"]] <- classificationResult[["support"]]
  table$dependOn(options = c("performanceMeasuresAll","svm","lr","rf","featPred","target","group", "genPredictions"))
  #table[["auc"]] <- auc
}

# these could also extend the S3 method scale although that could be somewhat unexpected
.scaleNumericData <- function(x, ...) {
  UseMethod(".scaleNumericData", x)
}

.scaleNumericData.data.frame <- function(x, center = TRUE, scale = TRUE) {
  if (nrow(x) == 0) {
    return(x)
  }
  idx <- sapply(x, function(x) is.numeric(x) && length(unique(x)) > 1)
  x[, idx] <- scale(x[, idx, drop = FALSE], center, scale)
  attr(x, which = "scaled:center") <- NULL
  attr(x, which = "scaled:scale") <- NULL
  return(x)
}

.scaleNumericData.matrix <- function(x, center = TRUE, scale = TRUE) {
  if (!is.numeric(x)) {
    warning(sprintf("Object passed to .scaleNumericData.matrix was not numeric!"))
    return(x)
  }
  x <- scale(x, center, scale)
  attr(x, which = "scaled:center") <- NULL
  attr(x, which = "scaled:scale") <- NULL
  return(x)
}

.scaleNumericData.numeric <- function(x, center = TRUE, scale = TRUE) {
  if (center) {
    x <- x - mean(x)
  }
  if (scale && length(unique(x)) > 1) {
    x <- x / sd(x)
  }
  return(x)
}

# fallback when .scaleNumericData is called with factor/ character data
.scaleNumericData.default <- function(x, center = TRUE, scale = TRUE) {
  return(x)
}
