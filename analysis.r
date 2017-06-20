#
# Creator: Pavel Valov (University of Waterloo)
# Date: Jan 2015
# 
# Modifiers: Dingyu Yang (Shanghai Dianji University) & Jianmei Guo (East China University of Science and Technology)
# Date: Jun 2016
#

library(dplyr)
library(ggplot2)
library(randtoolbox)
library(sensitivity)
library(stringr)
library(sqldf)
library(tidyr)
library(xtable)
library(rBayesianOptimization)

source("init.r")
source("data.r")
source("monitor.r")
source("regmodels.r")

Analyse <- function() {
  
  # Analyse systems using regression methods
  for (m in 1:length(methodNames)) {
    for (s in 1:length(systemNames)) {
      for(e in 1:length(experParams$sampleMethod)){
        for(p in 1:length(experParams$paraSearchMethod)){
          # remove other configuration
          # return the default configuration
          experParams$sampleSizes <<- experParams$defalutSampleSizes
          experParams$currentSampleMethod <<- experParams$sampleMethod[e]
          experParams$currentSearchMethod <<- experParams$paraSearchMethod[p]
          
          # Perform data analysis
          sysDataInfo <- GetSysDataInfo(s)
          methodInfo <- GetMethodInfo(m)
          accuracyData <- AnalysePredError(sysDataInfo, methodInfo)
          
          # Export analysis results
          accuracyFile <- file.path(outputFolder,
                                    paste(methodNames[m], "_",
                                          systemNames[s], "_Details_",experParams$currentSampleMethod,"_",
                                          experParams$currentSearchMethod,".csv", sep = ""))
          write.csv(accuracyData, file = accuracyFile, row.names = FALSE)
          
          
          # statis the result
          statResult <- statAccuracyData(accuracyData, sysDataInfo$sampleSizes)
          
          # Export analysis results
          statResultFile <- file.path(outputFolder,
                                      paste(methodNames[m], "_",
                                            systemNames[s], "_Stat_",experParams$currentSampleMethod,"_",
                                            experParams$currentSearchMethod,".csv", sep = ""))
          write.csv(statResult, file = statResultFile, row.names = FALSE)
        }
      }
    }
  }
  
}

AnalysePredError <- function(sysDataInfo, methodInfo) {
  # Performs regression analysis of the given configurable software systems
  # using specified regression method
  # 
  # Args:
  #   sysDataInfo: system data information (sample size, samples, etc)
  #   methodInfo: method information (sobol sequence, parameter sequence, etc)
  Monitor()
  Monitor(sysDataInfo, 3)
  Monitor(methodInfo, 3)
  
  # Initialize dataset for storing experiment results
  resultData <- data.frame()
  resultFormat <- c("SamplingSize", "SampleID")
  resultFormat <- c(resultFormat, methodsParams[[methodInfo$methodID]]$names)
  resultFormat <- c(resultFormat,"AvgSampleDistance","ValidError", "GeneralizedError")
  if(experParams$previousCartMethod){
    resultFormat <- c(resultFormat, "ASEMeanError")
  }
  resultFormat <- c(resultFormat, "ModelTime")
  
  resultData <- rbind(resultData, 1:length(resultFormat))
  colnames(resultData) <- resultFormat

  resultData <- tbl_df(resultData)

  Monitor(resultData, 3)

  # the prediction error for each sample iteration
  sampleSummaryError <- list()
  sampleSummaryError <- c(sampleSummaryError, 1:experParams$sampleSizes)
  
  # Iterate over all different sampling sizes (N, 2N, 3N...)
  for (sampleSizeID in 1:experParams$sampleSizes) {
    
      minError <- 10^6
    
      errorData <- NULL
      
      # Iterate over all different samples of the same size
      for (sampleID in 1:experParams$sampleRep) {

        Monitor("Starting regression model training", 3)
        
        modelParamsResult <- GetPredError(methodInfo, sysDataInfo, sampleSizeID, sampleID)

        Monitor("Regression model is trained", 3)
        
        resultRow <- c(sysDataInfo$sampleSizes[sampleSizeID], sampleID)
        resultRow <- c(resultRow, modelParamsResult$methodParams)
        resultRow <- c(resultRow, sysDataInfo$sampleSummaryDistance[[sampleSizeID]][[sampleID]])
        resultRow <- c(resultRow, modelParamsResult$validError)
        resultRow <- c(resultRow, modelParamsResult$error,modelParamsResult$aseTestError, modelParamsResult$modelTime)
        if(experParams$previousCartMethod){
          resultRow <- c(resultRow, modelParamsResult$aseTestError)
        }
        resultRow <- c(resultRow, modelParamsResult$modelTime)

        # Add to other results
        resultData <- rbind(resultData, resultRow)
        
      } # for (sample in 1:experParams$sampleRep)
  } # for (sampleSize in experParams$sampleSizes)

  # Remove pseudo-row from the beginning
  resultData <- resultData[-1, ]

  Monitor("Results", 3)
  Monitor(resultData, 3)

  return (resultData)
}

GetPredError <- function(methodInfo, sysDataInfo, sampleSizeID, sampleID) {
  # Generates regression model using specified method (CART, Bagging, etc)
  # 
  # Args:
  #   methodID: ID of the method to be used
  #   methodParams: method paramters to be used in training
  #   sysDataInfo: data info of the target configurable system
  #   sampleSizeID: ID of the sample size to be used from data info
  #   sampleID: ID of the particular sample to be used
  Monitor("Methods Parameters:", 2)
  Monitor(methodsParams, 2)

  # Build the training/test datasets   # get the row id
  set.seed(1)
  trainObs <- sysDataInfo$samples[[sampleSizeID]][[sampleID]]
  validObs <- sysDataInfo$validSamples[[sampleSizeID]][[sampleID]]
  testObs <- setdiff(setdiff(seq_len(nrow(sysDataInfo$sysData)), trainObs), validObs)

  Monitor("Training ,Validation and testing samples", 2)
  Monitor(trainObs, 2)
  Monitor(validObs, 2)
  Monitor(testObs, 2)

  # Select input and target system features
  input <- setdiff(colnames(sysDataInfo$sysData), "PERF")
  target <- "PERF"

  Monitor("Input and target system features:", 2)
  Monitor(input, 2)
  Monitor(target, 2)

  # Train regression model
  trainData <- sysDataInfo$sysData[trainObs, ]
  ValidData <- subset(sysDataInfo$sysData[validObs, ], select = c("PERF"))
  testData <- subset(sysDataInfo$sysData[testObs, ], select = c("PERF"))
  
  splitNum <- length(methodsParams$cart$minsplit)
  complexityNum <- length(methodsParams$cart$complexity)
  
  minError <- 10^6
  bestMethodParams <- NULL
  bestModel <- NULL
  
  # runtime of the construction model within training and validation
  modelTime <- 0

  startTime <- Sys.time()
  
  aseTestError <- 0
  # for ASE 2013 original cart method
  if(experParams$previousCartMethod){
    aseTrainObs <- unique(c(trainObs,validObs))
    aseTrainData <- sysDataInfo$sysData[aseTrainObs, ]
    S <- length(rownames(aseTrainData))
    aseMinbucket <- 0
    aseMinSplt <- 0
    aseMaxDepth <- 30
    aseComplexity <- 0.01
    if(S > 100){
      aseMinSplt <- floor(S/10 + 0.5)
      aseMinbucket <- aseMinSplt/2
    }else{
      aseMinbucket <- floor(S/10 + 0.5)
      aseMinSplt <- 2*aseMinbucket
    }
    aseMethodParams <- c(aseMinSplt, aseMinbucket)
    aseMethodParams <- c(aseMethodParams, aseMaxDepth, aseComplexity)
    
    asemodel <- Trainer(methodInfo$methodName, aseMethodParams, aseTrainData)
    # Predict system performance and get actual performance values
    asepredicted <- predict(asemodel, newdata = sysDataInfo$sysData[testObs, input])
    # Calculate relative error
    aseTestError <- colMeans(abs(testData - asepredicted) / testData * 100)
  }
  
  if(experParams$currentSearchMethod=="randomsearch"){
    splitId = sample(splitNum,1)
    minSplitValue <- methodsParams$cart$minsplit[splitId]
    trainSize <- length(rownames(trainData))
    cpId = sample(complexityNum,1)
    methodParams <- c(minSplitValue, methodsParams$cart$minbucket[splitId]);
    methodParams <- c(methodParams, methodsParams$cart$maxdepth, methodsParams$cart$complexity[cpId]);
    
    model <- Trainer(methodInfo$methodName, methodParams, trainData)
    
    # valid system performance and get valid prediction
    predicted <- predict(model, newdata = sysDataInfo$sysData[validObs, input])
    
    # Calculate relative error
    validError <- colMeans(abs(ValidData - predicted) / ValidData * 100) 
    
    if(minError > validError){
      minError <- validError
      bestMethodParams <- methodParams
      bestModel <- model
    }
  }
  # grid search method
  else if(experParams$currentSearchMethod=="gridsearch"){
    for(splitId in 1:splitNum){
      minSplitValue <- methodsParams$cart$minsplit[splitId]
      trainSize <- length(rownames(trainData))
      if(minSplitValue<=trainSize){
        for(cpId in 1:complexityNum){
          methodParams <- c(minSplitValue, methodsParams$cart$minbucket[splitId]);
          methodParams <- c(methodParams, methodsParams$cart$maxdepth, methodsParams$cart$complexity[cpId]);
          model <- Trainer(methodInfo$methodName, methodParams, trainData)
          # valid system performance and get valid prediction
          predicted <- predict(model, newdata = sysDataInfo$sysData[validObs, input])
          # Calculate relative error
          validError <- colMeans(abs(ValidData - predicted) / ValidData * 100) 
          if(minError > validError){
            minError <- validError
            bestMethodParams <- methodParams
            bestModel <- model
          }
        } # complexity  1:complexityNum 
      } # Search Space : minSplitValue<=trainSize
    }  # minSplit 1:splitNum
  } 
  else if(experParams$currentSearchMethod=="bayesian"){
    
    BayesianParams <<- list()
    
    # trainData, validObs, input, ValidData
    BayesianParams$trainData <<- trainData
    BayesianParams$newdata <<- sysDataInfo$sysData[validObs, input]
    BayesianParams$ValidData <<- ValidData
    BayesianParams$methodName <<- methodInfo$methodName
    
    init_grid_dt<-data.frame(splitId=c(4L,20L,32L),cpId=c(2L,3L,4L))

    OPT_Model <- BayesianOptimization(BayesianOptimizationFun,
                                      bounds = list(splitId = c(1L,splitNum),
                                                    cpId = c(1L, complexityNum)),
                                      init_grid_dt = init_grid_dt, init_points = 1, n_iter = 1,
                                      acq = "ucb", kappa = 2.576, eps = 0.0,
                                      verbose = TRUE)
    
    # get best parameters
    splitId <- OPT_Model$Best_Par["splitId"]
    cpId <-  OPT_Model$Best_Par["cpId"]
    
    minSplitValue <- methodsParams$cart$minsplit[splitId]
    trainSize <- length(rownames(trainData))
    methodParams <- c(minSplitValue, methodsParams$cart$minbucket[splitId]);
    methodParams <- c(methodParams, methodsParams$cart$maxdepth, methodsParams$cart$complexity[cpId]);
    
    model <- Trainer(methodInfo$methodName, methodParams, trainData)
    
    
    # valid system performance and get valid prediction
    predicted <- predict(model, newdata = sysDataInfo$sysData[validObs, input])
    
    # Calculate relative error
    validError <- colMeans(abs(ValidData - predicted) / ValidData * 100) 
    
    if(minError > validError){
      minError <- validError
      bestMethodParams <- methodParams
      bestModel <- model
    }
    
  }
    
  

  modelTime <- difftime(Sys.time(), startTime)

  # Predict system performance and get actual performance values
  predicted <- predict(bestModel, newdata = sysDataInfo$sysData[testObs, input])
  
  
  Monitor("Predicted and testData system performance from training sample", 2)
  Monitor(predicted, 2)
  Monitor(t(testData), 2)

  # Calculate relative error
  testError <- colMeans(abs(testData - predicted) / testData * 100)

  Monitor("Relative Error")
  # print(bestMethodParams)

  # methodInfo$methodParams  <- bestMethodParams
  
  modelParamsResult <- list(methodParams = bestMethodParams, validError = minError, error = testError)
  # for ASE 2013 original cart method
  if(experParams$previousCartMethod){
    modelParamsResult$aseTestError <- aseTestError
  }
  modelParamsResult$modelTime <- modelTime

  return(modelParamsResult)
}


BayesianOptimizationFun <- function(splitId, cpId){
  

  minSplitValue <- methodsParams$cart$minsplit[splitId]
  trainSize <- length(rownames(BayesianParams$trainData))

  methodParams <- c(minSplitValue, methodsParams$cart$minbucket[splitId])
  methodParams <- c(methodParams, methodsParams$cart$maxdepth, methodsParams$cart$complexity[cpId])
  
  model <- Trainer(BayesianParams$methodName, methodParams, BayesianParams$trainData)
  
  # valid system performance and get valid prediction
  predicted <- predict(model, newdata = BayesianParams$newdata)
  
  # score = 100-error (maximal is best)
  score <- 100-colMeans(abs(BayesianParams$ValidData - predicted) / BayesianParams$ValidData * 100) 
  
  list(Score = score, Pred = 0)
}


statAccuracyData <- function(accuracyData, sampleSizes){
  
  # Initialize dataset for storing experiment results
  resultData <- data.frame()
  resultFormat <- c("SamplingSize")
  resultFormat <- c(resultFormat, "AvgSampleDistance", "Dis_ConfInteval_95%")
  resultFormat <- c(resultFormat, "ValidError(%)", "ValidError_ConfInteval_95%")
  resultFormat <- c(resultFormat, "GeneralizedError(%)", "Error_ConfInteval_95%")
  # for ASE 2013 original cart method
  if(experParams$previousCartMethod){
    resultFormat <- c(resultFormat, "ASEMeanError(%)", "ASEError_ConfInteval_95%")
  }
  resultFormat <- c(resultFormat, "ModelTime(S)", "MTime_ConfInteval_95%")
  
  resultData <- rbind(resultData, 1:length(resultFormat))
  colnames(resultData) <- resultFormat
  
  resultData <- tbl_df(resultData)
  

  # Iterate over all different sampling sizes (N, 2N, 3N...)
  for (sampleSizeID in 1:experParams$sampleSizes) {
    
    SamplingSizeValue <- sampleSizes[sampleSizeID]
    
    # get the distance data by sampleSize and distance column
    tempDistance <- subset(subset(accuracyData, SamplingSize==SamplingSizeValue), select=c("AvgSampleDistance"))
    # remove max value
    tempDistance <- setdiff(tempDistance, subset(tempDistance, AvgSampleDistance==max(tempDistance)))
    # remove min value
    tempDistance <- setdiff(tempDistance, subset(tempDistance, AvgSampleDistance==min(tempDistance)))
    
    tempAvgDistance <- colMeans(tempDistance)
    tempSdDistance <- sqrt(var(tempDistance))
    tempDisConfiIntervalIn95 <- 1.96*tempSdDistance/sqrt(length(rownames(tempDistance)))
    # tempDisConfiIntervalIn99 <- 2.576*tempSdDistance/sqrt(length(rownames(tempDistance)))
    
    
    # get the validation error data by sampleSize 
    tempValidError <- subset(subset(accuracyData, SamplingSize==SamplingSizeValue),select=c("ValidError"))
    # remove max value
    tempValidError <- setdiff(tempValidError, subset(tempValidError, ValidError==max(tempValidError)))
    # remove min value
    tempValidError <- setdiff(tempValidError, subset(tempValidError, ValidError==min(tempValidError)))
    
    tempValidAvgError <- colMeans(tempValidError)
    tempValidSdError <- sqrt(var(tempValidError))
    tempValidErrorConfiIntervalIn95 <- 1.96*tempValidSdError/sqrt(length(rownames(tempValidError)))
    # tempValidErrorConfiIntervalIn99 <- 2.576*tempValidSdError/sqrt(length(rownames(tempValidError)))
    
    
    # get the error data by sampleSize  
    tempError <- subset(subset(accuracyData, SamplingSize==SamplingSizeValue),select=c("GeneralizedError"))
    # remove max value
    tempError <- setdiff(tempError, subset(tempError, GeneralizedError==max(tempError)))
    # remove min value
    tempError <- setdiff(tempError, subset(tempError, GeneralizedError==min(tempError)))
    
    tempAvgError <- colMeans(tempError)
    tempSdError <- sqrt(var(tempError))
    tempErrorConfiIntervalIn95 <- 1.96*tempSdError/sqrt(length(rownames(tempError)))
    # tempErrorConfiIntervalIn99 <- 2.576*tempSdError/sqrt(length(rownames(tempError)))
    
    # for ASE 2013 original cart method
    if(experParams$previousCartMethod){
      tempASEError <- subset(subset(accuracyData, SamplingSize==SamplingSizeValue),select=c("ASEMeanError"))
      # remove max value
      tempASEError <- setdiff(tempASEError, subset(tempASEError, ASEMeanError==max(tempASEError)))
      # remove min value
      tempASEError <- setdiff(tempASEError, subset(tempASEError, ASEMeanError==min(tempASEError)))
      tempASEAvgError <- colMeans(tempASEError)
      tempASESdError <- sqrt(var(tempASEError))
      tempASEErrorConfiIntervalIn95 <- 1.96*tempASESdError/sqrt(length(rownames(tempASEError)))
      # tempErrorConfiIntervalIn99 <- 2.576*tempSdError/sqrt(length(rownames(tempError)))
    }
    
    # get the model time by sampleSize  
    tempModelTime <- subset(subset(accuracyData, SamplingSize==SamplingSizeValue),select=c("ModelTime"))
    # remove max value
    tempModelTime <- setdiff(tempModelTime, subset(tempModelTime, ModelTime==max(tempModelTime)))
    # remove min value
    tempModelTime <- setdiff(tempModelTime, subset(tempModelTime, ModelTime==min(tempModelTime)))
    tempAvgModelTime <- colMeans(tempModelTime)
    tempSdModelTime <- sqrt(var(tempModelTime))
    tempMTimeConfiIntervalIn95 <- 1.96*tempSdModelTime/sqrt(length(rownames(tempModelTime)))
    # tempMTimeConfiIntervalIn99 <- 2.576*tempSdModelTime/sqrt(length(rownames(tempModelTime)))
    
    
    resultRow <- c(SamplingSizeValue)
    resultRow <- c(resultRow, tempAvgDistance, tempDisConfiIntervalIn95) 
    resultRow <- c(resultRow, tempValidAvgError, tempValidErrorConfiIntervalIn95)
    resultRow <- c(resultRow, tempAvgError, tempErrorConfiIntervalIn95)
    # for ASE 2013 original cart method
    if(experParams$previousCartMethod){
      resultRow <- c(resultRow, tempASEAvgError, tempASEErrorConfiIntervalIn95)
    }
    resultRow <- c(resultRow, tempAvgModelTime, tempMTimeConfiIntervalIn95)
    
    resultData <- rbind(resultData, resultRow)
    
  }
  # Remove pseudo-row from the beginning
  resultData <- resultData[-1, ]
  
  # calculate the Pearson's correlation between AvgSampleDistance and GeneralizedError
  AvgSampleDistance <- subset(resultData,select=c("AvgSampleDistance"))
  GeneralizedError <- subset(resultData,select=c("GeneralizedError(%)"))
  
  ValidError <- subset(resultData,select=c("ValidError(%)"))
  
  corDisWithError <- cor(AvgSampleDistance$AvgSampleDistance, GeneralizedError$`GeneralizedError(%)`)
  SpearMancorDisWithError <- cor(AvgSampleDistance$AvgSampleDistance, GeneralizedError$`GeneralizedError(%)`, method = "spearman")
  
  corValidationWithError <- cor(ValidError$'ValidError(%)', GeneralizedError$`GeneralizedError(%)`)
  
  SpearMancorValidationWithError <- cor(ValidError$'ValidError(%)', GeneralizedError$`GeneralizedError(%)`, method = "spearman")
  
  # blank space area
  resultRow <- c("","","","","","","","","","","","","")
  resultData <- rbind(resultData, resultRow)
  
  # blank space area
  resultRow <- c("Distance with Testing Error","","","","","","","","","","","","")
  resultData <- rbind(resultData, resultRow)
  
  
  # Pearson's correlation
  resultRow <- c("Pearson's correlation", corDisWithError,"","","","","","","","","","","")
  resultData <- rbind(resultData, resultRow)
  
  # SpearMan's correlation
  resultRow <- c("SpearMan's correlation", SpearMancorDisWithError,"","","","","","","","","","","")
  resultData <- rbind(resultData, resultRow)
  
  
  # blank space area
  resultRow <- c("Validation with Testing Error","","","","","","","","","","","","")
  resultData <- rbind(resultData, resultRow)
  
  
  # Pearson's correlation
  resultRow <- c("Pearson's correlation", corValidationWithError,"","","","","","","","","","","")
  resultData <- rbind(resultData, resultRow)
  
  # SpearMan's correlation
  resultRow <- c("SpearMan's correlation", SpearMancorValidationWithError,"","","","","","","","","","","")
  resultData <- rbind(resultData, resultRow)
  
  
  return(resultData)

}


