#
# Copyright (C) 2013-2018 University of Amsterdam
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
#
# When making changes to this file always mention @koenderks as a
# reviewer in the pull Request.


#DOE ALLES ONDER AUDIT OF ALGORITHMS
#AUDIT OF FINANCIAL STATEMENS
#AUDIT OF DATA

auditFairnessCriteriaAndSelection <- function(jaspResults, dataset, options, ...) {
  
  #x <- jfa::fairness()
  
  
  ready <- (  options[["group"]]      != "" &&
                options[["target"]]     != "" &&
                options[["features"]]   != "" &&
                options[["selectedRow"]] != -1 )
  
  dataset <- .auditFMReadData(dataset, options, ready)
  
  #results <- .auditFMComputeResults(dataset, options, ready)
  results<- .auditFMPred(dataset, options, ready,jaspResults)
  bestModelName <- .auditFMPickModel(dataset, options, ready, jaspResults, results)
  # 
  .auditFMCreateTable(jaspResults, options, dataset, ready, bestModelName)
  
  # .auditFMCreateTable(jaspResults, options, dataset, ready, "")
  #.auditFMCreateTable(jaspResults, options, dataset, ready, results[, -c(1:4)])
  
}

.auditFMReadData<- function(dataset, options, ready){
  if (!is.null(dataset)) {
    return(dataset)
  }
  
  if (ready) {
    dataset <- .readDataSetToEnd(columns.as.numeric = c(options[["target"]]),
                                 columns.as.factor= c(options[["group"]], options[["features"]]))
    return(dataset)
  }
}

.auditFMPickModel <- function(dataset, options, ready, jaspResults, results){
  if (!ready){return ()}
  namesModels <- jaspResults[["namesModels"]]$object

  fairnessFrame <- data.frame()
  performanceFrame <- c()
  for (name in namesModels)
  {
    obj <- jaspResults[[name]]$object
    
    fairnessFrame <- rbind(fairnessFrame, obj[["subsetValues"]])
    
    perfMeasure <- obj[["perfMeasures"]][[options[["perfFocus"]]]]
    performanceFrame <- append(performanceFrame, perfMeasure)

  }
  
  bestModelIndicesPerf <- which(performanceFrame == max(performanceFrame))
  if (length(bestModelIndicesPerf) > 1)
  {
    fairnessFrame<- fairnessFrame[bestModelIndicesPerf,]
    
    #measures closer to 1 are considered better/more fair 
    distance <- abs(fairnessFrame - 1)
    bestModelIndicesFair <- which(distance == min(distance))
    #again more than one bestmodel index
    if (length(bestModelIndicesFair) > 1)
    {
      if (!is.data.frame(distance))
      {
        index <- which.max(distance)
        return (namesModels[index])
      }
      #normalise all columns and sum to get best model index
      process<-caret::preProcess(distance, method ="range")
      normalized <- predict(process, distance)
      index<- which.max(apply(normalized, 1, sum))
      
      return (namesModels[index])
    }
    return (namesModels[bestModelIndicesFair]) 
  }
  else {
    return (namesModels[bestModelIndicesPerf])
  }
  
  # #best case: model is best in fairness and best in performance
  # if (bestFairName == bestPerfName){
  #   return (bestFairName)
  # }
  # #else: discrepancy between fairness and performance, preference is performance for better predictions
  # return (bestPerfName)
  
}

.auditFMPred<-function(dataset, options, ready, jaspResults){
  if (!ready){return ()}
  
  #set the formula for every classifier
  .auditClassificationSetFormula(options,jaspResults)
  trainingIndex <- sample.int(nrow(dataset), size = ceiling((0.8 * nrow(dataset))))
  
  #make index for amount of models created so we can distinguish multiple models of same type
  i<- 1
  
  #create list such that we can append all names of models into it
  
  namesModels <- c()
  
  #randomly get k models from the algorithm types 
  for (algo in sample(c("svm","rf","boost"), options[["kmodels"]], replace=TRUE))
  {
    if (options[["svm"]] && algo == "svm")
    {
      name <- paste("classificationResultSVM",as.character(i),sep="")
    }
    if (options[["rf"]] && algo == "rf")
    {
      name <- paste("classificationResultRF",as.character(i),sep="")
    }
    if (options[["boost"]] && algo == "boost")
    {
      name <- paste("classificationResultADA",as.character(i),sep="")
    }
    .auditML(dataset, jaspResults, options, trainingIndex, name)
    
    #look into getting this in the .auditPredResult function 
    dataPredictions <- jaspResults[[name]]$object[["classes"]]
    .auditFMComputeResults(cbind(dataset,dataPredictions), options, ready, "dataPredictions", jaspResults, name) 
    
    #jaspResults[["namesModels"]] <- append(jaspResults[["namesModels"]], name)
    namesModels <- append(namesModels, name)
    i <- i + 1
  }
  jaspResults[["namesModels"]] <- createJaspState(namesModels)
  
}


#maak functie waarin results <- .auditFMComputeResults
.auditFMCreateTable <- function(jaspResults, options, dataset, ready, bestModelName){
  if (!ready){
    return ()
  }
  
  bestModel <- jaspResults[[bestModelName]]$object
  
  tbBest <- createJaspTable("Best Model")
  tbBest$dependOn(optionsFromObject = bestModel)
  jaspResults[["bestModel"]] <- tbBest
  
  row <- data.frame(
    bestmodel <- bestModelName,
    bestmodelAcc = bestModel[["testAcc"]],
    bestmodelFocus = bestModel[["perfMeasures"]][[options[["perfFocus"]]]]
  )
  tbBest$addRows(row)
  #tbBest$addColumns(bestModel[["perfMeasures"]][[options[["perfFocus"]]]])
  
  
  tb <- createJaspTable("Comparison of Fairness Measures Between Groups")
  tb$dependOn(optionsFromObject = bestModel)
  
  jaspResults[["FMtable"]] <- tb
  fairnessResult <- bestModel[["fairnessMetrics"]]
  
  subset<- jaspResults[["subsetMeasures"]]$object #.auditGetSubset(options)
  
  
  if (length(subset) != 0){
    fairnessResult<- fairnessResult[c("levelsGroup", subset)]
  }
  
  #tb$addRows(fairnessResult)
  tb$setData(fairnessResult)
  
}

.auditGetSubset <- function(options){
  df<- data.frame(id = c("demPar", "propPar", "eqOdds", "prPar", "accPar",
                         "fnrPar", "fprPar", "npvPar",
                         "specPar", "mcc"),
                  "q1" = c("no","no","yes","yes","yes","yes","yes","yes","yes","yes"),
                  "q2" = c("abs","prop","yes","no","yes","no","no","no","no","yes"),
                  "q3" = c("","","","corr","","incorr","incorr","corr","corr",""),
                  "q4" = c("","","","tp","","fn","fp","tn","tn",""))
  
  subset <- subset(df, (q1 == options[["q1"]] & q2 == options[["q2"]] & q3 == options[["q3"]] & q4 == options[["q4"]]))[["id"]]
  return (subset)
}

.auditFMCreateCM <- function(df, target_col, pred_col){
  cf <- caret::confusionMatrix(data = as.factor(df[[pred_col]]),reference=as.factor(df[[target_col]]))$table
  return(cf)
  #table(factor(act), factor(pred))
}

.auditFMGetCounts <- function(confusion_matrix){
  tp  <- confusion_matrix[2,2]
  fp <- confusion_matrix[2,1]
  fn <- confusion_matrix[1,2]
  tn  <- confusion_matrix[1,1]
  counts <- data.frame(tp, fp, tn, fn)
  return(counts)
}

.auditFMCalcFairMeasures <- function(counts){
  #equal amount of positive predictions in each group
  #therefore, just add up TP + FP
  demPar <- counts$tp + counts$fp
  #demPar <- counts$tn/(counts$tp + counts$fp + counts$tn + counts$fn)
  #counts$tp for the positive class
  #counts$tn / all counts (total number of samples that belong to this sample group)
  #this is in case the negative class corresponds to the favorable outcome
  
  propPar <- (counts$tp + counts$fp) / (counts$tp + counts$fp + counts$tn + counts$fn)
  
  #original was wrong so we adapted it 
  #eqOdds <- counts$tp / (counts$tp + counts$fn)
  #tpr/fpr should be 1 if equalized odds. this has to be 
  eqOdds <- (counts$tp / (counts$tp + counts$fn)) / ((counts$fp / (counts$fp + counts$tn)))
  
  prPar <- counts$tp / (counts$tp + counts$fp)
  
  accPar <- (counts$tp + counts$tn) / (counts$tp + counts$fp + counts$tn + counts$fn)
  
  fnrPar <- counts$fn / (counts$tp + counts$fn)
  
  fprPar <- counts$fp / (counts$tn + counts$fp)
  
  npvPar <- counts$tn / (counts$tn + counts$fn)
  
  specPar <- counts$tn / (counts$tn + counts$fp)
  
  mcc <- (counts$tp * counts$tn - counts$fp * counts$fn)/
    sqrt(as.numeric(counts$tp + counts$fp) * as.numeric(counts$tp + counts$fn) *
           as.numeric(counts$tn + counts$fp)*as.numeric(counts$tn +counts$fn))
  
  resultsDf <- data.frame(demPar, propPar, eqOdds, prPar, accPar,
                          fnrPar, fprPar, npvPar,
                          specPar, mcc)
  
  
  return (resultsDf)
}

.auditFMCompareGroups <- function(all_groups_df, reference){
  return (as.numeric(all_groups_df)/as.numeric(all_groups_df[reference]))
}

.auditFMCalculateRatios<- function(data, privilegedIndex, unprivilegedIndex){
  
  return (data[privilegedIndex]/ data[unprivilegedIndex])
  #return (max(data)/min(data))
}

.auditFMComputeResults <- function(data, options, ready, predCol, jaspResults, name)
{
  group<- options[["group"]]
  targetCol<- options[["target"]]
  ref<- options[["selectedRow"]]
  
  if (!ready) {
    ref = 0
  }
  
  allMeasures <- data.frame()
  privilegeFrame <- data.frame()
  privilegeFrame <- c()
  #evalMeasures <- data.frame()
  #for each group in the dataset, calculate the confusion matrix, counts and fairness measures
  for (level in levels(factor(data[[group]]))){
    #group the data based on the specified group (e.g race)
    oneGroup <- subset(data, data[[group]] == level)
    
    #create confusion matrix and get counts
    cf <- .auditFMCreateCM(oneGroup, predCol, targetCol)
    counts <- .auditFMGetCounts(cf)
    
    privilegeRatio <- nrow(subset(oneGroup, oneGroup[[targetCol]] == 1)) / nrow(oneGroup) 
    privilegeFrame<- append(privilegeFrame, privilegeRatio)
    
    #calculate the fairness measures per group
    oneGroupMeasures <- .auditFMCalcFairMeasures(counts)
    oneGroupEval<- .auditFMEvalMeasures(counts)
    
    #combine every measure from a single group to one dataframe
    allMeasures<- rbind(allMeasures, oneGroupMeasures)
    #evalMeasures<- rbind(evalMeasures, oneGroupEval)
  }
  
  # get all the levels for the specified group
  levelsGroup <- levels(as.factor(data[[group]]))
  
  privilegedIndex <- which.max(privilegeFrame)
  unprivilegedIndex <- which.min(privilegeFrame)
  
  #get the index of the referenced level
  lev_index <- ref + 1L
  
  #get a subset of the fairness measures, generated from the question flowchart
  subset<- .auditGetSubset(options)
  jaspResults[["subsetMeasures"]] <- createJaspState(subset)
  
  #get ratios from the subset of fmeasures 
  subsetValues <- apply(allMeasures[subset], 2, .auditFMCalculateRatios,
                        privilegedIndex = privilegedIndex, unprivilegedIndex = unprivilegedIndex)
  
  results <- jaspResults[[name]]$object
  
  results[["subsetValues"]] <- subsetValues
  
  #compare the fairness measures of the referenced group with other groups
  fairnessResults <- apply(allMeasures[, names(allMeasures) != "groups"], 2, .auditFMCompareGroups, reference = lev_index)
  
  if (ready) {
    fairnessResults <- data.frame(apply(fairnessResults, 2, signif, digits = 2))
    fairnessResults <- cbind(data.frame(levelsGroup), fairnessResults)
    #fairnessResults <- cbind(evalMeasures, fairnessResults)
  }
  # result[["fairMetrics"]] <- fairnessResults[, -c(1:4)]
  # result[["perfMetrics"]] <- cbind(Subgroup = fairnessResults[["levelsGroup"]], Metrics = fairnessResults[, c(1:4)])
  
  results[["fairnessMetrics"]] <- fairnessResults
  jaspResults[[name]] <- createJaspState(results)
  
  #jaspResults[["performanceMetrics"]] <- createJaspState(cbind(fairnessResults[["levelsGroup"]], fairnessResults[, c(1:4)]))
  
  
}

.auditFMEvaluationMatrixGroup <- function(jaspResults, options, dataset, ready, results){
  if (!is.null(jaspResults[["performanceMeasuresGroup"]]) || !options[["performanceMeasuresGroup"]]) {
    return()
  }
  
  tableGroup <- createJaspTable(title = "Performance Metrics per Group")
  tableGroup$dependOn(c("performanceMeasuresGroup",.auditFMCommonOptions()))
  
  # Bind table to jaspResults
  jaspResults[["performanceMeasuresGroup"]] <- tableGroup
  
  if (!ready){
    return ()
  }
  
  tableGroup$addColumns(results)
}

.auditFMCommonOptions <- function(){
  opt <- c("group", "predicted", "")
  return (opt)
}

#jfa::functienaam
