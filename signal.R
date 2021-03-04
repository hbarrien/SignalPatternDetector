# signal.R
#
# Date: 
# 2021-03-02
#
# Author:
# Herbert Barrientos
#
# Description:
# Signal pattern detection functionality
#
# Note:
# This functionality is just a functional prototype. Thus, more
# work needs to be included, such as precondition checking, error
# handling, etc.


# ################# LIBRARIES ##################
library(data.table)


# ############ EXTERNAL SOURCE CODE ############
source(file="constants.R")


# ################# VARIABLES ##################
WD <- paste0(getwd(), "/SIGNAL/")


# ################# FUNCTIONS ##################

# readSignalData
# Reads a signal CSV file from local disk
readSignalData <- function() {
  
  SIGNAL <- as.data.table(read.csv(paste0(WD, "SIGNAL.csv", quote=EMPTY_STRING)))
  return(SIGNAL)
  
}  # END readSignalData


# getSignalPattern
#
# Description:
# Extracts a sequence of rows from a signal data table
#
# Arguments:
# signal    : a data table containing signal data
# signalType: a character indicating the desired signal component to process
# fromT     : an integer indicating the starting position within argument signal
# toT       : an integer indicating the end position position within argument signal
#
# Precondition:
# (|signal| > 0) && 
# ((signalType == SIGNAL_X) || (signalType == SIGNAL_Y) || (signalType == SIGNAL_Z)) &&  
# (fromT <= toT))
#
# Postcondition:
# A subset of signal has been returned
getSignalPattern <- function(signal, signalType, fromT, toT) {
  
  pattern <- signal[(signal$t >= fromT) & (signal$t <= toT),]
  return(do.call(  '$', list(get('pattern'), signalType)))
  
}  # END getSignalPattern


# writeSignalPattern.csv
# Writes the contents of argument "pattern" to disk
writeSignalPattern.csv <- function(pattern, fName, localSave = TRUE) {
  
  fPath <- ifelse(localSave, paste0(WD, fName, ".csv"), fName)
  
  write(pattern, fPath, ncolumns = 1, sep = ",")
  
  return(fPath)
  
}  # END writeSignalPatternwriteSignalPattern.csv


# readSignalPattern.csv
# Reads a csv file from disk, containing pattern ddata
readSignalPattern.csv <- function(fName) {
  
  fPath <- paste0(WD, fName, ".csv")
  pattern <- as.numeric(strsplit(readChar(fPath, nchars = (file.size(fPath)-2)), ",")[[1]])
  return(pattern)
  
}  # END readSignalPattern


# findRepeatingSignalPattern
# Searches for repetitions of argument pattern within argument signal
# 
# Arguments:
# signal   : a numeric vector containing signal data
# pattern  : a numeric vector containing a sequence defining a signal pattern
# startPos : an integer indicating the starting position within signal to start searching
# simDegree: an integer indicating the similarity degree to use during the search process
#
# Precondition:
# (|signal| > 0) && (|pattern| > 0) && (|signal| >= |pattern|) &&
# ((simDegree == SIM_0) || (simDegree == SIM_1) || (simDegree == SIM_2) || (simDegree == SIM_3) ||
#  (simDegree == SIM_4) || (simDegree == SIM_5) || (simDegree == SIM_6))
#
# Returns:
# matchIndices: an integer vector containing the starting positions (within signal) of all 
# pattern repetitions
#
# Postcondition:
# (|matchIndices| >= 0)
findRepeatingSignalPattern <- function(signal, pattern, startPos = 1, simDegree = SIM_0) {

  lenSignal  <- length(signal)
  lenPattern <- length(pattern)
  
  matchIndices <- c()
  
  sidx <- startPos
  pidx <- 1
  nidx <- 0
  
  # reevaluate: a toggle variable between 0 and 1. It is used to consider 
  # the case when the current value of sidx, after a no-match, might be 
  # the beginning of a matching pattern, but blindly increasing its value 
  # would miss it. So, when (reevaluate == 0) sidx is not increased, thus
  # giving the process the chance to start a new search at that index; 
  # otherwise, sidx is increased and reevaluate is again set to 0
  reevaluate <- 0

  # Loop invariant: (sidx <= |signal| && (|matchIndices >= 0|))
  while (sidx <= lenSignal) {
    
    if ((pattern[pidx] >= (signal[sidx] - (simDegree/100))) &&
        (pattern[pidx] <= (signal[sidx] + (simDegree/100)))) {
      
      if (nidx == 0) nidx <- sidx

      if ((sidx-length(pattern)+1) == nidx) {
        
        matchIndices <- c(matchIndices, nidx)
        sidx <- (sidx+1)
        pidx <- 1
        nidx <- 0
        next()
        
      }  # END if
      
      pidx <- (pidx+1)
      sidx <- (sidx+1)
      next()
      
    }  # END if
    
    if (reevaluate > 0) {
      sidx <- (sidx+1)
      reevaluate <- 0
    } else {
      reevaluate <- (reevaluate+1)
    }

    if (sidx > (lenSignal-lenPattern+1))
      break()
    
    pidx <- 1
    nidx <- 0
    
  }  # END while
  
  return(matchIndices)
  
}  # END findRepeatingSignalPattern


# scanSignal
# According to argument signalType, this program calls findRepeatingSignalPattern(...)
# and processes the corresponding signal column in argument signalData. If the return 
# value (i.e., the integer vector containing the start positions of pattern repetitions) 
# is neither NULL nor empty, the corresponding "match" column is updated, that is, its 
# rows beginning at a start position and spanning the pattern's length, are updated with 
# the HIT value
#
# Arguments:
# signalData: a data table with three columns (i.e., X, Y, and Z), each containing signal 
#             data, and three "match" columns, each having all rows initialized with NO_HIT
# pattern   : a numeric vector containing a sequence defining a signal pattern
# signalType: a character indicating the desired signal column to process 
# simDegree : an integer indicating the similarity degree to use during the search process
#
# Precondition:
# (|signalData| > 0) && (|pattern| > 0) && (|signalData| >= |pattern|) &&
# ((signalType == SIGNAL_X) || (signalType == SIGNAL_Y) || (signalType == SIGNAL_Z)) &&  
# ((simDegree == SIM_0) || (simDegree == SIM_1) || (simDegree == SIM_2) || (simDegree == SIM_3) ||
#  (simDegree == SIM_4) || (simDegree == SIM_5) || (simDegree == SIM_6))
#
# Returns:
# NULL      : if signalType is unknown
# signalData: argument signalData updated with HIT values, per signalType
#
#Postcondition:
#  (For signalType SIGNAL_X: (|startPositions| > 0) --> "match" column "is_pattern_x" is updated) &&
#  (For signalType SIGNAL_Y: (|startPositions| > 0) --> "match" column "is_pattern_y" is updated) &&
#  (For signalType SIGNAL_Z: (|startPositions| > 0) --> "match" column "is_pattern_z" is updated)
scanSignal <- function(signalData, pattern, signalType, simDegree = SIM_0) {
  
  signalData$rowNbr <- 1:nrow(signalData)
  startPositions <- NULL
  
  if (signalType == SIGNAL_X) {
    
    # As long as startPositions is null or empty, start a new search by incrementing
    # the initial index of signalData$x by 1
    lastPos <- (length(signalData$x) - length(pattern) + 1)
    for (i in 1:lastPos) {
      
      startPositions <- findRepeatingSignalPattern(signalData$x, pattern, i, simDegree)
      
      if (!is.null(startPositions) && (length(startPositions) > 0))
        break()
      
    }  # END for

    if (!is.null(startPositions) && (length(startPositions) > 0)) 
      lapply(startPositions, function(x) {r <- ((signalData$rowNbr >= x) & (signalData$rowNbr <= (x+length(pattern)-1)))
                                          signalData[r,"is_pattern_x"] <<- HIT})
    
  } else if (signalType == SIGNAL_Y) {
    
    # As long as startPositions is null or empty, start a new search by incrementing
    # the initial index of signalData$y by 1
    lastPos <- (length(signalData$y) - length(pattern) + 1)
    for (i in 1:lastPos) {
      
      startPositions <- findRepeatingSignalPattern(signalData$y, pattern, i, simDegree)
      
      if (!is.null(startPositions) && (length(startPositions) > 0))
        break()
      
    }  # END for
    
    if (!is.null(startPositions) && (length(startPositions) > 0)) 
      lapply(startPositions, function(x) {r <- ((signalData$rowNbr >= x) & (signalData$rowNbr <= (x+length(pattern)-1)))
                                          signalData[r,"is_pattern_y"] <<- HIT})
    
  } else if (signalType == SIGNAL_Z) {
    
    # As long as startPositions is null or empty, start a new search by incrementing
    # the initial index of signalData$z by 1
    lastPos <- (length(signalData$z) - length(pattern) + 1)
    for (i in 1:lastPos) {
      
      startPositions <- findRepeatingSignalPattern(signalData$z, pattern, i, simDegree)
    
      if (!is.null(startPositions) && (length(startPositions) > 0))
        break()
      
    }  # END for  
      
    if (!is.null(startPositions) && (length(startPositions) > 0)) 
      lapply(startPositions, function(x) {r <- ((signalData$rowNbr >= x) & (signalData$rowNbr <= (x+length(pattern)-1)))
                                          signalData[r,"is_pattern_z"] <<- HIT})
    
  } else {
    
    return(NULL)
    
  }  # END if
  
  signalData <- signalData[,rowNbr := NULL]
  return(signalData)
  
}  # END scanSignal


# findSignalPatterns
# Driver function
#
# Arguments:
# signal    : a data table containing signal data
# px, py, pz: numeric vectors, each containing a sequence defining a signal pattern
# simDegree : an integer indicating the similarity degree to use during the search process
# localProc : TRUE  - this program is invoked from a local machine
#             FALSE - this program is invoked remotely, e.g., a server
#
# Precondition:
# (!localProc --> (signal != NULL) && (|signal| > 0)) &&
# ((px != NULL) --> (|px| > 0)) && ((py != NULL) --> (|py| > 0)) && ((pz != NULL) --> (|pz| > 0)) &&
# (|signal| >= |px|) && (|signal| >= |py|) && (|signal| >= |pz|) &&
# ((simDegree == SIM_0) || (simDegree == SIM_1) || (simDegree == SIM_2) || (simDegree == SIM_3) ||
#  (simDegree == SIM_4) || (simDegree == SIM_5) || (simDegree == SIM_6))
#
# Returns:
# Argument signal
#
# Postcondition:
# ((state at process start: signal) == (state at process completion: signal)) ||
# ((state at process start: signal) != (state at process completion: signal)) 
findSignalPatterns <- function(signal, px, py, pz, simDegree = SIM_0, localProc = TRUE) {
  
  if (localProc)
    signal <- as.data.table(read.csv(paste0(getwd(), "/ANNACISION/SIGNAL.csv", quote=EMPTY_STRING)))
  
  if (!is.null(px)) signal <- scanSignal(signal, px, SIGNAL_X, simDegree)
  if (!is.null(py)) signal <- scanSignal(signal, py, SIGNAL_Y, simDegree)
  if (!is.null(pz)) signal <- scanSignal(signal, pz, SIGNAL_Z, simDegree)
  
  if (localProc)
    write.csv(signal, paste0(getwd(), "/ANNACISION/SIGNAL_PROC.csv"), row.names = FALSE)
    
  return(signal)
  
}  # END findSignalPatterns
