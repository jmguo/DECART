#
# Creator: Pavel Valov (University of Waterloo)
# Date: Jan 2015
# 
# Modifiers: Dingyu Yang (Shanghai Dianji University) & Jianmei Guo (East China University of Science and Technology)
# Date: Jun 2016
#

source("monitor.r")

Init <- function() {
  # Aggregated function for initializing all parameters
  InitMonitorParams()
  InitMethodsParams()
  InitParamsExperiment()
  Monitor("Initialization is complete")
}

InitMonitorParams <- function() {
  # Specifies global parameters used by debugging functions

  # List specifies debugging levels for functions
  # Messages from functions not in the list will not be printed
  # Messages from functions that have negative level will not be printed
  # Messages with level exceeding specified in the list for function will not
  # be printed
  funLevels <<- list(# analysis.r
                     AnalysePredError = 2,
                     GetPredError = -2,

                     # data.r
                     GetSysDataInfo = -2,

                     # init.r
                     Init = 2,

                     # regmodels.r
                     GetMethodInfo = -2,
                     Trainer = 2,
                     TrainCart = 2)
}

InitMethodsParams <- function() {
  # Initializes parameter ranges for CART regression method
  methodNames <<- list(cart = "CART")
  
  # Initialize CART parameters
  # minsplit, minbucket - depend on number of observations
  cartNames   <- c("minsplit", "minbucket", "maxdepth", "complexity")
  cartMinSplit <- c(seq(4,52,4))
  cartMinBucket <-  round(1/3 * cartMinSplit)
  cartmaxdepth    <- 30
  cartComplexity <- c(0.01, 0.001, 0.0001, 0.00001, 0.000001)
  
  cartSobol   <- 1
  cart <- list(names = cartNames, minsplit = cartMinSplit, minbucket = cartMinBucket,
               maxdepth = cartmaxdepth, complexity = cartComplexity, sobolLen = cartSobol)

  methodsParams <<- list(cart = cart)
}

InitParamsExperiment <- function() {
  # Initializes parameters relevant to the whole experiment
  # input the data name
#  systemNames <<- c( "AJStat","Apache","BerkeleyC", "BerkeleyJ","clasp","LLVM","lrzip","SQLite_all4653","x264_158")
  systemNames <<- c("AJStat")
  
    # systemNames <<- c( "Hipacc")


  experParams <<- list()
  # for dataset "Hipacc" , the experParams$sampleSizes applies the number 20 for 20*N
  experParams$sampleSizes <<- 9   # Sizes to generate (1*N, 2*N, ..., 9*N)
  experParams$sampleRep <<- 30    # Experiment repetitions
  experParams$defalutSampleSizes <<- experParams$sampleSizes # default Sample Sizes 
  
  # for ASE 2013 cart method
  experParams$previousCartMethod <<- TRUE

  outputFolder <<- file.path("data", "results")
}


