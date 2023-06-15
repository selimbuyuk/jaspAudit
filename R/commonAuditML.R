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
  features <- options[["features"]]
  target <- options[["target"]]
  formula <- formula(paste(target, "~", paste(features, collapse = " + ")))
  jaspResults[["formula"]] <- createJaspState(formula)
  jaspResults[["formula"]]$dependOn(options = c("features", "target"))
}

.auditPerfMeasures <- function(dataPredictions, dataset, options){
  cf <- caret::confusionMatrix(data = as.factor(dataPredictions), reference=as.factor(dataset[, options[["target"]]]))$table
  counts<- .auditFMGetCounts(cf)
    
  accuracy <-  (counts$tp + counts$tn) / (counts$tp + counts$fn + counts$fp + counts$tn)
  precision <- counts$tp / (counts$tp + counts$fp)
  recall <- counts$tp / (counts$tp + counts$fn)
  f1 <- 2 * ((precision * recall) / (precision + recall))
  mcc <- (counts$tp * counts$tn - counts$fp * counts$fn)/
    sqrt(as.numeric(counts$tp + counts$fp) * as.numeric(counts$tp + counts$fn) *
           as.numeric(counts$tn + counts$fp)*as.numeric(counts$tn +counts$fn))
  
  resultsDf<- data.frame(accuracy, precision, recall, f1, mcc)
  
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
  result[["perfMeasures"]] <- .auditPerfMeasures(dataPredictions, dataset, options)

  jaspResults[[name]] <- createJaspState(result)
  jaspResults[[name]]$dependOn(options = c("target","features","group"))
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
  if (grepl("SVM", name, fixed = TRUE))
  {
    fit <- e1071::svm(formula, data = trainingSet, type = "C-classification")
    
    # Use the specified model to make predictions for dataset
    testPredictions <- predict(fit, newdata = testSet, type="response")
    dataPredictions <- predict(fit, newdata = dataset, type="response")
    .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name, dataset)
  }
  if (grepl("RF", name, fixed = TRUE))
  {
    trainingSet[[options[["target"]]]] <- as.factor(trainingSet[[options[["target"]]]])
    fit <- randomForest::randomForest(formula, data=trainingSet)
    
    # Use the specified model to make predictions for dataset
    testPredictions <- predict(fit, newdata = testSet, type="response")
    dataPredictions <- predict(fit, newdata = dataset, type="response")
    .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name, dataset)
  }
  if (grepl("ADA", name, fixed = TRUE))
  {
    fit <- gbm::gbm(
      formula = formula, data = trainingSet, distribution = "multinomial"
    )
    
    dataProbs <- gbm::predict.gbm(fit, newdata = dataset, type = "response")
    dataPredictions <- colnames(dataProbs)[apply(dataProbs, 1, which.max)]
    testPredictions <- dataPredictions[-trainingIndex]
    
    .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name, dataset)
  }
}


