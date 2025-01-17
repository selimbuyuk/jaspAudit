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


auditFairnessCriteriaAndSelection <- function(jaspResults, dataset, options, ...) {
  ready <- (  options[["group"]]      != "" &&
                options[["target"]]     != "" &&
                options[["featPred"]]   != "" &&
                options[["selectedRow"]] != -1 )
  
  dataset <- .auditFMReadData(dataset, options, ready)
  if (options[["predictBool"]] == "ownPrediction")
  {
    .auditFMComputeResults(dataset, options, ready, options[["featPred"]], jaspResults, "onlyModel")
    bestModelName <- "onlyModel"
  }
  else 
  {
    .auditFMPred(dataset, options, ready, jaspResults)
    bestModelName <- .auditFMPickModel(dataset, options, ready, jaspResults)
  }
  print("SS")
  print(bestModelName)
  .auditFMCreateTable(jaspResults, options, dataset, ready, bestModelName)
}

.auditFMReadData<- function(dataset, options, ready){
  if (!is.null(dataset)) {
    return(dataset)
  }
  
  if (ready) {
    # dataset <- .readDataSetToEnd(columns.as.factor = c(options[["target"]]),
    #                              columns.as.categorical= c(options[["group"]], options[["featPred"]]))}
    # 
    
    dataset <- .readDataSetToEnd(#columns.as.factor = c(options[["target"]]),
      columns.as.factor= c(options[["group"]], options[["featPred"]],options[["target"]]))
    
    #dataset<- .readDataSetToEnd(columns.as.factor= c(options[["group"]]),columns.as.numerical=options[["target"]], columns.as.categorical= c( options[["featPred"]]))
    
    # if (options[["predictBool"]] != "ownPrediction"){
    #   library(caret)
    #   dummy <- caret::dummyVars(" ~ .", data=dataset[, options[["featPred"]]])
    #   #perform one-hot encoding on data frame
    #   d <- data.frame(predict(dummy, newdata=dataset[, options[["featPred"]]]))
    #   
    for (algo in c("scaleVariablesRF", "scaleVariablesSVM", "scaleVariablesBoost"))
    {
      if (length(unlist(options[["featPred"]]) > 0) && (options[[algo]])) {
        dataset[, options[["featPred"]]] <- .scaleNumericData(dataset[, options[["featPred"]], drop = FALSE])
      }
    }
    return(dataset)
  }
}


.auditFMPred<-function(dataset, options, ready, jaspResults){
  if (!ready){return ()}
  
  #set the formula for every classifier
  .auditClassificationSetFormula(options, jaspResults)
  trainingIndex <- sample.int(nrow(dataset), size = ceiling((1 - options[["testDataManual"]]) * nrow(dataset)))
  
  if (options[["svm"]])
  {
    name <- "classificationResultsvm"
    .auditML(dataset, jaspResults, options, trainingIndex, name)
    dataPredictions <- jaspResults[[name]]$object[["classes"]]
    .auditFMComputeResults(cbind(dataset,dataPredictions), options, ready, "dataPredictions", jaspResults, name) 
  }
  if (options[["rf"]])
  {
    name <- "classificationResultrf"
    .auditML(dataset, jaspResults, options, trainingIndex, name)
    dataPredictions <- jaspResults[[name]]$object[["classes"]]
    .auditFMComputeResults(cbind(dataset,dataPredictions), options, ready, "dataPredictions", jaspResults, name) 
  }
  if (options[["lr"]])
  {
    name <- "classificationResultlr"
    .auditML(dataset, jaspResults, options, trainingIndex, name)
    dataPredictions <- jaspResults[[name]]$object[["classes"]]
    .auditFMComputeResults(cbind(dataset,dataPredictions), options, ready, "dataPredictions", jaspResults, name) 
  }
}

.auditFMComputeResults <- function(data, options, ready, predCol, jaspResults, name)
{
  group<- options[["group"]]
  targetCol<- options[["target"]]
  ref<- options[["selectedRow"]]
  
  if (!ready) {
    #ref = 0
    return ()
  }
  
  allMeasures <- data.frame()
  privilegeFrame <- data.frame()
  privilegeFrame <- c()
  #for each group in the dataset, calculate the confusion matrix, counts and fairness measures
  for (level in levels(factor(data[[group]]))){
    #group the data based on the specified group (e.g race)
    oneGroup <- subset(data, data[[group]] == level)
    
    #create confusion matrix and get counts
    cf <- .auditFMCreateCM(oneGroup, targetCol, predCol)
    counts <- .auditFMGetCounts(cf)
    
    privilegeRatio <- nrow(subset(oneGroup, oneGroup[[targetCol]] == 1)) / nrow(oneGroup) 
    privilegeFrame<- append(privilegeFrame, privilegeRatio)
    
    #calculate the fairness measures per group
    oneGroupMeasures <- .auditFMCalcFairMeasures(counts)
    oneGroupEval<- .auditFMEvalMeasures(counts)
    
    #combine every measure from a single group to one dataframe
    allMeasures<- rbind(allMeasures, oneGroupMeasures)
  }
  
  # get all the levels for the specified group
  levelsGroup <- levels(as.factor(data[[group]]))
  
  privilegedIndex <- which.max(privilegeFrame)
  unprivilegedIndex <- which.min(privilegeFrame)
  
  #get the index of the referenced level
  lev_index <- ref + 1L
  
  #get a subset of the fairness measures, generated from the question flowchart
  subset<- .auditGetSubset(options)
  jaspResults[["subsetNames"]] <- createJaspState(subset)
  
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
  }
  
  results[["fairnessMetrics"]] <- fairnessResults
  jaspResults[[name]] <- createJaspState(results)
}

.auditFMPickModel <- function(dataset, options, ready, jaspResults){
  if (!ready){return ()}
  
  namesModels <- c()
  fairnessFrame <- data.frame()
  performanceFrame <- c()
  for (algo in c("svm", "rf", "lr"))
  {
    if (options[[algo]])
    {
      name <- paste("classificationResult", algo, sep="")
      namesModels <- append(namesModels, name)
      
      obj <- jaspResults[[name]]$object 
      
      fairnessFrame <- rbind(fairnessFrame, obj[["subsetValues"]])
      
      perfMeasure <- obj[["perfMeasures"]][[options[["perfFocus"]]]]
      print(perfMeasure)
      performanceFrame <- append(performanceFrame, perfMeasure[length(perfMeasure)])
    }
  }
  
  bestModelIndicesPerf <- which(performanceFrame == max(performanceFrame))
  
  
  if (length(bestModelIndicesPerf) > 1)
  {
    if (options[["tiebreaker"]] == "randomTie"){index <- sample(bestModelIndicesPerf,1)} 
    
    fairnessFrame<- fairnessFrame[bestModelIndicesPerf,]
    #measures closer to 1 are considered better/more fair 
    distance <- abs(fairnessFrame - 1)
    
    if (options[["tiebreaker"]] =="bestTie"){index <-  which(distance == min(distance))[1]} 
    
    if (options[["tiebreaker"]] =="worseTie"){index <- which.max(distance)[1]} 

    return (namesModels[index])
  }
  
  else {
    return (namesModels[bestModelIndicesPerf])
  }
}

#maak functie waarin results <- .auditFMComputeResults
.auditFMCreateTable <- function(jaspResults, options, dataset, ready, bestModelName){
  
  containerProcedure <- createJaspContainer(title = gettext("<u>Procedure</u>"))
  paragraphProcedure<- createJaspHtml(text = gettext("The goal of this procedure is to determine whether a dataset is fair for all subgroups in a sensitive attribute. We characterise an attribute as sensitive if it can lead to a specific person's legal, ethical, social, or personal beliefs. Examples of this are race, religion and gender. Since we are interested in the relative unfairness between subgroups, called disparity, we want to evaluate them with fairness measures. In order to do this, we have to provide a group that acts as a reference point. We consider a disparity tolerance, where values in this tolerance range are considered to be fair. This disparity tolerance is provided by the user, but generally a tolerance of 80% is used. This means that subgroups with disparities between 0.8 and 1.25 are considered fair.

  This procedure is designed in such a way that the user is guided through the selection of the best models and fairness measures suited for the given situation."))
  
  containerProcedure[["paragraph"]] <- paragraphProcedure
  jaspResults[["containerProcedure"]] <- containerProcedure
  
  if (!ready){
    return ()
  }
  
  bestModel <- jaspResults[[bestModelName]]$object
  
  fairnessResult <- bestModel[["fairnessMetrics"]]
  
  subset<- jaspResults[["subsetNames"]]$object 
  if (length(subset) != 0){
    fairnessResult<- fairnessResult[c("levelsGroup", subset)]
  }
  
  fairnessMask <- .auditFMFairnessCheck(fairnessResult, options,jaspResults)
  if (length(subset) > 1){
    #check per measure what the overall consensus is of disparity
    fairnessMask <- apply(fairnessMask, 2, all)
  }
  else {fairnessMask <- all(fairnessMask)}
  
  # if all of the fairness measures are true, audit is succesful
  if (all(fairnessMask))
  {
    paragraphEval<- createJaspHtml(text = gettextf("Audit Succesful!"))
  } # if only one measure is succesful, than partially succesful
  else if (any(fairnessMask))
  {
    paragraphEval<- createJaspHtml(text = gettextf("Audit Partially Succesful!"))
  } # if none, audit failed
  else {paragraphEval<- createJaspHtml(text = gettextf("Audit failed!"))}
  
  containerEval <- createJaspContainer(title = gettext("<u>Audit Evaluation</u>"))
  containerEval[["paragraph"]] <- paragraphEval
  jaspResults[["containerEval"]] <- containerEval
  
  
  tb <- createJaspTable("Comparison of Fairness Measures Between Groups")
  tb$dependOn(options = c("svm","lr","rf","featPred","target","group", "selectedRow","q1","q2","q3"))
  
  jaspResults[["FMtable"]] <- tb
  tableFairness <- fairnessResult
  colnames(tableFairness)[colnames(tableFairness) == 'levelsGroup'] <- 'Sensitive Groups'
  tb$setData(tableFairness)
  
  if (options[["enableGroup"]] == "groupPlot")
  {
    plotFrame <- tidyr::pivot_longer(fairnessResult,
                                     cols = !levelsGroup, 
                                     names_to = "fairness_measure", 
                                     values_to = "measure_value")
    
    p <- .auditFMCreatePlot(plotFrame, TRUE, "fairness_measure", "levelsGroup","measure_value", options )
    plot <- createJaspPlot(title="Fairness Measures Comparison Plot Grouped", width = 500)
    plot$plotObject <- p
    plot$dependOn(c("enableGroup","enableThreshold",.auditFMCommonOptions(),"svm", "lr","rf"))
    jaspResults[["FMplot"]] <- plot
  }  
  else
  {
    for (fm in subset)
    {
      p <- .auditFMCreatePlot(fairnessResult, FALSE, "", "levelsGroup", fm, options)
      plot <- createJaspPlot(title=paste("Fairness Measures Comparison Plot", fm), width = 500)
      plot$plotObject <- p
      plot$dependOn(c("enableGroup","enableThreshold", .auditFMCommonOptions(), "q1","q2","q3","svm", "lr","rf"))
      jaspResults[[paste("FMplot",fm)]] <- plot
    }
  }
  
  if (options[["performanceMeasuresAll"]] & options[["predictBool"]] == "genPredictions"){
    .mlClassificationTableMetrics(dataset, options, jaspResults, ready, bestModelName)
  }
}

.auditFMCreatePlot <- function(plotFrame, fillBool, fill, x, y, options){
  if (fillBool){p<- ggplot2::ggplot(plotFrame, ggplot2::aes(fill=.data[[fill]], x=.data[[x]], y=.data[[y]]))+ ggplot2::theme_classic()}
  else{p<- ggplot2::ggplot(plotFrame, ggplot2::aes(fill=.data[[x]], y=.data[[y]], x=.data[[x]])) + ggplot2::theme_classic()+
    ggplot2::theme(legend.position="none")}
  p<- p + 
    ggplot2::geom_bar(position="dodge", stat="identity")+
    ggplot2::coord_cartesian(ylim=c(0, 2)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))+ ggplot2::labs(x = "Sensitive Groups", y = "Measure Value", fill = "Fairness Measure")
  if (options[["enableThreshold"]])
  {
    rect <- data.frame(ymin=options[["fthreshold"]], ymax=1/options[["fthreshold"]], xmin=-Inf, xmax=Inf)
    p <- p + ggplot2::geom_rect(data=rect, mapping = ggplot2::aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                                alpha=0.3,
                                inherit.aes = FALSE)
  }
  
  return (p)
}

.auditFMFairnessCheck <- function(fairnessResult, options, jaspResults){
  fairnessResult<- fairnessResult[,names(fairnessResult) != "levelsGroup"]
  lowerLimit <- options[["fthreshold"]]
  upperLimit <- 1/options[["fthreshold"]]
  #lambda function so that we dont have to name a function
  fairnessMask <- (lowerLimit< fairnessResult & fairnessResult < upperLimit)
  
  jaspResults[["fairnessMask"]] <- createJaspState(fairnessMask)
  jaspResults[["fairnessMask"]]$dependOn(c("fthresh","selectedRow"))
  return (fairnessMask)
  
}

.auditGetSubset <- function(options){
  #special case
  if (options[["q1"]] == "no" & options[["q2"]]== "yes" & options[["q3"]] == ""){return (c("FPRP","TPRP"))}
  
  df<- data.frame(id = c("DP", "PP", "PRP", "AP",
                         "FNRP", "FPRP", "TPRP", "NPVP",
                         "SP"),
                  "q1" = c("no" ,"no" ,"yes" ,"no" ,"yes"   ,"yes"   , "yes" , "yes","yes"),
                  "q2" = c("no" ,"no" ,"corr","no" ,"incorr","incorr","corr" ,"corr","corr"),
                  "q3" = c("no" ,"no" ,"tp"  ,"yes",""      ,""      , "tp"  ,"tn"  ,"tn"))
  
  subset <- subset(df, (q1 == options[["q1"]] & q2 == options[["q2"]] & q3 == options[["q3"]]))[["id"]]
  return (subset)
}

.auditFMCreateCM <- function(df, target_col, pred_col){
  cf <- caret::confusionMatrix(data = as.factor(df[[pred_col]]),reference=as.factor(df[[target_col]]))$table
  return(cf)
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
  DP <- counts$tp + counts$fp
  
  PP <- (counts$tp + counts$fp) / (counts$tp + counts$fp + counts$tn + counts$fn)
  
  PRP <- counts$tp / (counts$tp + counts$fp)
  
  AP <- (counts$tp + counts$tn) / (counts$tp + counts$fp + counts$tn + counts$fn)
  
  FNRP <- counts$fn / (counts$tp + counts$fn)
  
  FPRP <- counts$fp / (counts$tn + counts$fp)
  
  #when eqOdds is selected, show TPRP and FPRP
  TPRP <- (counts$tp / (counts$tp + counts$fn)) 
  
  NPVP <- counts$tn / (counts$tn + counts$fn)
  
  SP <- counts$tn / (counts$tn + counts$fp)
  
  resultsDf <- data.frame(DP, PP, PRP, AP,
                          FNRP, FPRP, TPRP, NPVP,
                          SP)
  
  return (resultsDf)
}

.auditFMCompareGroups <- function(all_groups_df, reference){
  return (as.numeric(all_groups_df)/as.numeric(all_groups_df[reference]))
}

.auditFMCalculateRatios<- function(data, privilegedIndex, unprivilegedIndex){
  return (data[privilegedIndex]/ data[unprivilegedIndex])
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

