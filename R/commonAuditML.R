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
  print('haha')
  print(features)
  formula <- formula(paste(target, "~", paste(features, collapse = " + ")))
  jaspResults[["formula"]] <- createJaspState(formula)
  jaspResults[["formula"]]$dependOn(options = c("featPred", "target"))
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
  jaspResults[[name]]$dependOn(options = c("target","featPred","group"))
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
    trainingSet[[options[["target"]]]] <- as.factor(trainingSet[[options[["target"]]]])
    fit <- randomForest::randomForest(formula, data=trainingSet, ntree = options[["noOfTrees"]], mtry = options[["numberOfPredictors"]],
                                      sampsize = ceiling(options[["baggingFraction"]] * nrow(trainingSet)),
                                      importance = TRUE, keep.forest = TRUE)
    
    # Use the specified model to make predictions for dataset
    testPredictions <- predict(fit, newdata = testSet, type="response")
    dataPredictions <- predict(fit, newdata = dataset, type="response")
    .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name, dataset)
  }
  if (grepl("boost", name, fixed = TRUE))
  {
    #if sensitive attribute is not a feature, delete it so we can rearrange the dataset
    trainingSet <- trainingSet[, names(trainingSet) != options[["group"]]]
    
    # gbm expects the columns in the data to be in the same order as the variables...
    trainingSet<- trainingSet[, match(names(trainingSet), all.vars(formula))]
    fit <- gbm::gbm(
      formula = formula, data = trainingSet, distribution = "multinomial", n.cores = 1, keep.data = TRUE,n.trees = options[["noOfTrees"]],
      shrinkage = options[["shrinkage"]], interaction.depth = options[["interactionDepth"]],
      bag.fraction = options[["baggingFraction"]], n.minobsinnode = options[["minObservationsInNode"]]
    )
    
    dataProbs <- gbm::predict.gbm(fit, newdata = dataset, type = "response")
    dataPredictions <- colnames(dataProbs)[apply(dataProbs, 1, which.max)]
    testPredictions <- dataPredictions[-trainingIndex]
    
    .auditPredResult(formula, fit, testPredictions, dataPredictions, trainingSet, testSet, options, jaspResults, name, dataset)
  }
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
