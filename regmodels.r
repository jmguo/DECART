#
# Creator: Pavel Valov (University of Waterloo)
# Date: Jan 2015
# 
# Modifiers: Dingyu Yang (Shanghai Dianji University) & Jianmei Guo (East China University of Science and Technology)
# Date: Jun 2016
#

library(rpart)
library(randomForest)
library(gbm)
library(kernlab)

source("monitor.r")

GetMethodInfo <- function(methodID) {


  # Populate methodInfo data structure
  methodInfo <- list()
  methodInfo$methodID <- methodID
  methodInfo$methodName <- methodNames[[methodID]]

  Monitor(paste("Method ID:",   methodInfo$methodID))
  Monitor(paste("Method Name:", methodInfo$methodName))

  return(methodInfo)
}

GetMethodParams <- function(methodInfo, sobolIter, sysDataInfo, sampleSizeID, sampleID) {
  # Sobol parameters to convert into method parameters
  sobolParams <- methodInfo$sobolSeq[sobolIter, ]

  # Data properties
  obsNum <- sysDataInfo$sampleSizes[sampleSizeID] 
  ftrNum <- sysDataInfo$featureNum

  # Output method parameters
  methodParams <- NULL

  if (methodInfo$methodName == "CART") {
    minSplit   <- ConvertSobol(2, min(floor(obsNum / 2), 10), sobolParams[1], 0)
    minBucket  <- ConvertSobol(1, min(floor(obsNum / 2), 10), sobolParams[2], 0)
    maxDepth   <- ConvertSobol(2, 10, sobolParams[3], 0) # 30
    complexity <- ConvertSobol(0, 0.01, sobolParams[4], 4) # 0

    methodParams <- c(minSplit, minBucket, maxDepth, complexity)
  }

  return(methodParams)
}

# Regression models training methods ##########################################
Trainer <- function(methodName, params, data) {
  #
  #
  # Args:
  #   methodName:
  #   params:
  #   data:

  model <- NULL

  # Select regression method implementation
  regMethod <<- switch(methodName,
                       "CART" = TrainCart,
                       "BAGGING" = TrainBag,
                       "FOREST" = TrainFrst,
                       "ESVR" = TrainEsvr)
  
  # Train regression model
  model <- regMethod(params, data)
  return(model)
}

TrainCart <- function(params, data) {
  #
  #
  # Args:
  #   methodID:
  #   params:
  #   data:

  Monitor()
  Monitor("CART Training params:", 2)
  Monitor(params, 2)

  minSplit   <- params[1]
  minBucket  <- params[2]
  maxDepth   <- params[3]
  complexity <- params[4]

  Monitor(paste("CART. ",
                "minsplit: ", minSplit, "; ",
                "minBucket: ", minBucket, "; ",
                "maxDepth: ", maxDepth, "; ",
                "complexity: ", complexity, "; ",
                sep = ""))

  # Train regression model
  Monitor("Starting CART model training...", 3)
  require(rpart, quietly = TRUE)
    set.seed(1)
    model <-
      rpart(PERF ~ .,
            data = data,
            method = "anova",
            parms = list(split = "information"),
            control = rpart.control(minsplit = minSplit,
                                    minbucket = minBucket,
                                    maxdepth = maxDepth,
                                    cp = complexity,
                                    usesurrogate = 0,
                                    maxsurrogate = 0))
  Monitor("Finished CART model training", 3)

  return(model)
}

