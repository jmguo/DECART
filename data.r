#
# Creator: Pavel Valov (University of Waterloo)
# Date: Jan 2015
# 
# Modifiers: Dingyu Yang (Shanghai Dianji University) & Jianmei Guo (East China University of Science and Technology)
# Date: Jun 2016
#

library(dplyr)
library(tidyr)

source("monitor.r")

GetSysDataInfo <- function(sysID) {
  # Loads data for specified configurable software system
  # 
  # Args:
  #   sysID: ID of the system to be loaded

  # Load system data
  sysDataFile <- file.path("data", "systems",
                           paste(systemNames[sysID], ".csv", sep = ""))

  sysData <- read.csv(sysDataFile,
                      na.strings = c(".", "NA", "", "?"),
                      strip.white = TRUE, encoding = "UTF-8")

  # original Non-feature columns: PERF
  originFeatures <- colnames(sysData)
  nonFeat <- c("PERF")
  originFeatures <- originFeatures[!(originFeatures %in% nonFeat)]
  originFeatureNum <- length(originFeatures)
  
  # remove Mandatory Feature
  sysData <- removeMandatoryFeature(sysData)
  
  # remove Mandatory Non-feature columns: PERF
  features <- colnames(sysData)
  nonFeat <- c("PERF")
  features <- features[!(features %in% nonFeat)]
  featureNum <- length(features)
  
  rowNum <- length(rownames(sysData))
  
  # sample size limit for whole row number limitation
  sampleSizeLimit <- rowNum/originFeatureNum
  
  if(sampleSizeLimit < experParams$sampleSizes){
    experParams$sampleSizes <<- floor(sampleSizeLimit)
  }
  
  
  # 'samples' is a list of 'sampleNum' lists of length 'sampleRep'
  # 'samples' contains all necessary data samples for analysing the specified
  # system
  samples <- list()
  samples <- c(samples, 1:experParams$sampleSizes)

  # validation data list for (N,2N,...,5N)
  validSamples <- list()
  validSamples <- c(validSamples, 1:experParams$sampleSizes)
  
  
  # 'sampleFeatherDistance' 'sampleNonFeatherDistance' 'sampleSummaryDistance'
  # store the similarity between samples and whole dataset 
  sampleSummaryDistance <- list()
  sampleSummaryDistance <- c(sampleSummaryDistance, 1:experParams$sampleSizes)
  
  
  sampleSizes <- 1:experParams$sampleSizes
  sampleSizes <- sampleSizes * originFeatureNum

  # Fill 'samples' with newly generated data samples
  # 'i' iterates over lists
  # 'j' iterates over individual samples in lists
  for (i in 1:experParams$sampleSizes) {   #(N,2N,3N...5N)
    equisizedSamples <- list()
    tmpValidSamples <- list()
    
    tempSimilarityDistance <- list()
    tempsimilarity <- list()

    # Populate new list with 'sampleRep' samples of the same size
    # TODO: check randomization seed
    for (j in 1:experParams$sampleRep) {
      
      # init sample data
      initsample <- sample(nrow(sysData), sampleSizes[i])
      
      # training sample data
      smpl <- sample(initsample, sampleSizes[i], replace = TRUE)
      
      # valid sample data
      validsample <- setdiff(initsample, smpl)
      
      equisizedSamples <- c(equisizedSamples, list(smpl))
      
      tmpValidSamples <- c(tmpValidSamples, list(validsample))
      
      # tempDistance <- GetSampleFeatureDistance(sysData, smpl, featureNum) 
      # tempDistance <- GetPerfValue(sysData, smpl) 
      tempDistance <- mean(c(GetPerfValue(sysData, smpl), GetSampleFeatureDistance(sysData, smpl, featureNum)))
      
      #tempSimilarityDistance <- c(tempSimilarityDistance, list(tempDistance))  
      tempsimilarity <- c(tempsimilarity,tempDistance)
      
    }
    # training samples
    samples[[i]] <- equisizedSamples
    
    # validation samples
    validSamples[[i]] <- tmpValidSamples
    
    # the average of the distance   
    sampleSummaryDistance[[i]] <- tempsimilarity
    
  }

  # Initialize list that will contain all system data information
  dataInfo <- list()
  dataInfo$sysID <- sysID                 # Configurable system ID
  dataInfo$sysName <- systemNames[sysID]  # Configurable system name
  dataInfo$sysData <- sysData             # Configurable system data
  
  dataInfo$originFeatures <- originFeatures           # Configuration origin features
  dataInfo$originFeatureNum <- originFeatureNum       # Number of configuration origin features

  dataInfo$features <- features           # Configuration features
  dataInfo$featureNum <- featureNum       # Number of configuration features
  
  dataInfo$sampleSizes <- sampleSizes     # List of sample sizes
  dataInfo$samples <- samples             # List (for each machine) of
                                          #   Lists (for each sample size) of
                                          #     Samples

  dataInfo$validSamples <- validSamples   # list of validation samples
  
  dataInfo$sampleSummaryDistance <- sampleSummaryDistance  # the list of feature distance by each sample 
  
  Monitor(paste("System ID:", dataInfo$sysID))
  Monitor(paste("System Name:", dataInfo$sysName))
  Monitor(paste("Features:", dataInfo$featureNum))
  Monitor(dataInfo$features)
  Monitor("Sampling sizes:")
  Monitor(dataInfo$sampleSizes)
  Monitor("Samples:")
  Monitor(dataInfo$samples)

  return(dataInfo)
}

# get the similarity by the 1:featureNum between whole data and sample data  
GetSampleFeatureDistance <- function(sysData, sampleId, featureNum){
  
  # get sample data from original data 
  sampleData <- sysData[sampleId, ]
  sampleNum <- length(rownames(sampleData))
  
  # get the feature disance between sample  and whole dataset
  FeatureDistance  <- 0
  
  for(i in 1:featureNum){
    # selected frequecy
    RFSi <- GetRFValue(sampleData, i)  
    # selected expected frequecy
    RFWi <- GetRFValue(sysData, i)*length(rownames(sampleData))/length(rownames(sysData))
    #FeatureDistance <- sum(FeatureDistance,(RFSi-RFWi)^2/RFWi)
    
    FeatureDistance <- sum(FeatureDistance, (RFSi-RFWi)^2/RFWi)
  }
  
  # FeatureDistance <- FeatureDistance/featureNum
  # get the performance distance between sample  and whole dataset
  
  return(FeatureDistance)
}


#calculate the RF value for data by featureId  
GetRFValue <- function(data, featureId){
  # dataNum = length(rownames(data))
  selectedNum = sum(data[,featureId])
  
  #return(sum(data[,featureId])/length(rownames(data)))
  return (selectedNum)
  
}


#calculate the RF value for  each performance value  
GetPerfValue <- function(sysData, sampleId ){
  sampleData <- sysData[sampleId, ]
  #get the different performance values
  uniqueSysData <- unique(subset(sysData, select = c("PERF")))
  uniqueSampleData <- unique(subset(sampleData, select = c("PERF")))
  
  # the performance value numbers of whole data set 
  perfSysDataNum = length(rownames(uniqueSysData))
  
  # the performance value numbers of sample data set 
  perfSampleDataNum = length(rownames(uniqueSampleData))
  
  # the similarity of performance values
  perfValue <- 0
  
  # calculate the similarity for each performance value
  for(i in 1:perfSysDataNum){
    tempwholedata <- subset(uniqueSysData, PERF==uniqueSysData[i,])
    tempsampledata <- subset(uniqueSampleData, PERF==uniqueSampleData[i,])
    # real frequency
    tempsampleRF <- length(rownames(tempsampledata))
    # this sample data expected frequency
    tempExpectRF <- length(rownames(tempwholedata))*perfSampleDataNum/perfSysDataNum
    
    
    # tempwholeRF <- length(rownames(tempwholedata))/perfSysDataNum
    # tempsampleRF <- length(rownames(tempsampledata))/perfSampleDataNum
    perfValue <- sum(perfValue,(tempsampleRF-tempExpectRF)^2/tempExpectRF)

  }
  
  # perfValue <- perfValue/perfSysDataNum
  
  return(perfValue)
}


# remove the Mandatory Feature
removeMandatoryFeature <-function(sysData){
  features <- colnames(sysData)
  nonFeat <- c("PERF")
  features <- features[!(features %in% nonFeat)]
  featureNum <- length(features)
  
  MandatoryFeatureId <- NULL

  for(i in 1:featureNum){
    selectdata <- unique(subset(sysData, select = features[i]))
    if(length(rownames(selectdata)) == 1){
      MandatoryFeatureId <- c(MandatoryFeatureId, i)
    }
  }
  
  #remove the specified featureId
  if( !is.null(MandatoryFeatureId)){
    sysData <- sysData[,-MandatoryFeatureId]
  }

  return(sysData)
}



