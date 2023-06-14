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

.auditPredResult <- function(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name){
  result <- list()
  result[["formula"]] <- formula
  result[["model"]] <- fit
  result[["confTable"]] <- table("Pred" = testPredictions, "Real" = testSet[, options[["target"]]])
  result[["testAcc"]] <- sum(diag(prop.table(result[["confTable"]])))
  result[["ntrain"]] <- nrow(trainingSet)
  result[["ntest"]] <- nrow(testSet)
  result[["classes"]] <- dataPredictions
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
  
  if (name == "classificationResultSVM")
  {
    fit <- e1071::svm(formula, data = trainingSet, type = "C-classification")
    # Use the specified model to make predictions for dataset
    testPredictions <- predict(fit, newdata = testSet, type="response")
    dataPredictions <- predict(fit, newdata = dataset, type="response")
    .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name)
  }
  else if (name == "classificationResultRF")
  {
    trainingSet[[options[["target"]]]] <- as.factor(trainingSet[[options[["target"]]]])
    fit <- randomForest::randomForest(formula, data=trainingSet)
    
    # Use the specified model to make predictions for dataset
    testPredictions <- predict(fit, newdata = testSet, type="response")
    dataPredictions <- predict(fit, newdata = dataset, type="response")
    .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name)
  }
  else if (name == "classificationResultADA")
  {
    fit <- gbm::gbm(
      formula = formula, data = trainingSet, distribution = "multinomial"
    )
    
    dataProbs <- gbm::predict.gbm(fit, newdata = dataset, type = "response")
    dataPredictions <- colnames(dataProbs)[apply(dataProbs, 1, which.max)]
    testPredictions <- dataPredictions[-trainingIndex]
    
    .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name)
  }
  


}


