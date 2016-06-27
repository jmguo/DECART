#
# Creator: Pavel Valov (University of Waterloo)
# Date: Jan 2015
# 

Monitor <- function(obj = NULL, level = 1) {
  # Prints debugging messages from functions
  #
  # Args:
  #   obj: actual debugging message to be printed
  #   level: specifies debugging level of the message

  # Get function name that called this function by:
  #   1) Get system function call
  #   2) Find where function name ends by searching for left bracket '('
  #   3) Extract calling function name

  # sysCall <- sys.calls()[[sys.nframe() - 1]]
  # print(sysCall)

  sysCallDep <- deparse(sys.calls()[[sys.nframe() - 1]])[1]
  brktInd <- which(strsplit(sysCallDep, '')[[1]] == '(')[1]
  funName <- substr(sysCallDep, 1, brktInd - 1)

  # IF level is specified for the calling function AND
  # this level is non-negative AND
  # message level doesn't exceed function level THEN
  # print the message from the calling function
  if ((funName %in% names(funLevels)) &&
      (get(funName, funLevels) > 0) &&
      (get(funName, funLevels) >= level)) {

    if (is.null(obj)) {
      print(paste("FUNCTION:", funName))
    }
    else {
      print(obj)
    }
  }
}


