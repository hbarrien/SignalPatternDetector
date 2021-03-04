# server.R
#
# Date: 
# 2021-03-02
#
# Author:
# Herbert Barrientos
#
# Description:
# Server-side definitions for the Shiny application
#
# Note:
# This functionality is just a functional prototype. Thus, more
# work needs to be included, such as precondition checking, error
# handling, etc.


# ################# LIBRARIES ##################
library(shiny)
library(shinybusy)
library(dygraphs)
library(data.table)
library(DT)


# ############ EXTERNAL SOURCE CODE ############
source(file="constants.R")
source(file="signal.R")


# ################# VARIABLES ##################
SIGNAL <- NULL
PX     <- NULL
PY     <- NULL
PZ     <- NULL


# ################# FUNCTIONS ##################

# By default, the file size limit is 5MB. It can be changed
# by setting this option. Here we'll raise limit to 9MB
# Source: web lookup
options(shiny.maxRequestSize = 9*1024^2)


# resetAll
# Sets global variables to NULL, and sets all ui widgets to
# their original state. Garbage collection is invoked to
# ensure a "clean slate"... as much as possible
resetAll <- function(input, output, session) {
  
  SIGNAL <<- NULL
  PX     <<- NULL
  PY     <<- NULL
  PZ     <<- NULL
  
  inFile <- input$signal
  
  # This action is necessary because a temporary copy of the uploaded 
  # file is kept on disk, causing the widgets to behave unpredictably
  if (!is.null(inFile$datapath) && file.exists(inFile$datapath))
    file.remove(inFile$datapath)
  
  reset("signal", asis = FALSE)
  reset("choosePattern", asis = FALSE)
  
  reset("upLoadedSignal", asis = FALSE)
  output$upLoadedSignal <- DT::renderDataTable({return(data.table())})
  updateTabsetPanel(session, TABSET_SIGNAL, TAB_SIGNAL_DATA);
  
  output$px <- output$px <- renderPrint(NULL)
  reset("px", asis = FALSE)
  
  output$py <- output$px <- renderPrint(NULL)
  reset("py", asis = FALSE)
  
  output$pz <- output$px <- renderPrint(NULL)
  reset("pz", asis = FALSE)
  
  reset("pxFromT", asis = FALSE)
  reset("pxToT", asis = FALSE)
  reset("setPx", asis = FALSE)
  
  reset("pyFromT", asis = FALSE)
  reset("pyToT", asis = FALSE)
  reset("setPy", asis = FALSE)
  
  reset("pzFromT", asis = FALSE)
  reset("pzToT", asis = FALSE)
  reset("setPz", asis = FALSE)
  
  reset("chooseSim", asis = FALSE)
  
  reset("viz", asis = FALSE)
  reset("vizx", asis = FALSE)
  reset("vizy", asis = FALSE)
  reset("vizz", asis = FALSE)
  
  output$viz  <- renderDygraph(getMainViz())
  output$vizx <- renderDygraph(getVizX())
  output$vizy <- renderDygraph(getVizY())
  output$vizz <- renderDygraph(getVizZ())
  
  gc()
  
}  # END resetAll


# uploadSignalData
# Uploads a raw signal dataset, and prepares it as a data table
# ready for processing. Once the table is ready, it is assigned
# to the global variable SIGNAL.
uploadSignalData <- function(input, output) {
  
  inFile <- input$signal
  
  if (is.null(inFile))
    return(NULL)

  SIGNAL <<- as.data.table(read.csv(inFile$datapath, header = TRUE, sep = ","))
  colnames(SIGNAL) <- c("x", "y", "z")
  
  SIGNAL$x <- as.numeric(SIGNAL$x)
  SIGNAL$y <- as.numeric(SIGNAL$y)
  SIGNAL$z <- as.numeric(SIGNAL$z)
  
  SIGNAL$t <- 1:nrow(SIGNAL)
  SIGNAL$is_pattern_x <- as.integer(0)
  SIGNAL$is_pattern_y <- as.integer(0)
  SIGNAL$is_pattern_z <- as.integer(0)
  
  SIGNAL_PROC <- data.table(t = SIGNAL$t, x = SIGNAL$x, y = SIGNAL$y, z = SIGNAL$z, is_pattern_x = SIGNAL$is_pattern_x, 
                            is_pattern_y = SIGNAL$is_pattern_y, is_pattern_z = SIGNAL$is_pattern_z)
  
  SIGNAL <<- SIGNAL_PROC
  
  return(SIGNAL)

}  # END uploadSignalData


# uploadSignalPattern
# Uploads a pattern dataset from a local disk and returns a numeric vector
uploadSignalPattern <- function(input, output) {
  
  inFile <- input$patternFile
  
  if (is.null(inFile) || is.null(inFile$datapath) || (inFile$datapath == EMPTY_STRING) || !file.exists(inFile$datapath))
    return(NULL)
  
  pattern <- read.csv(inFile$datapath, header = FALSE, quote="\"", comment.char="", stringsAsFactors=FALSE)
  pattern <- pattern[,1]
  
  # These actions are necessary because a temporary copy of the uploaded file 
  # is kept on disk, causing the widgets to behave unpredictably
  file.remove(inFile$datapath)
  reset("patternFile", asis = FALSE)
  
  return(pattern)

}  # END uploadSignalPattern


# uploadSignalPatternX
# Uploads a pattern dataset for processing against the X signal column in global variable SIGNAL.
# The retrieved pattern is assigned to global variable PX
uploadSignalPatternX <- function(input, output, selectedPattern) {
  
  if (selectedPattern != SIGNAL_X)
    return(PX)
  
  p <- uploadSignalPattern(input, output)
  if (!is.null(p)) PX <<- p
  
  return(PX)
  
}  # END uploadSignalPatternX


# uploadSignalPatternY
# Uploads a pattern dataset for processing against the Y signal column in global variable SIGNAL.
# The retrieved pattern is assigned to global variable PY
uploadSignalPatternY <- function(input, output, selectedPattern) {
  
  if (selectedPattern != SIGNAL_Y)
    return(PY)
  
  p <- uploadSignalPattern(input, output)
  if (!is.null(p)) PY <<- p
  
  return(PY)
  
}  # END uploadSignalPatternY


# uploadSignalPatternZ
# Uploads a pattern dataset for processing against the Z signal column in global variable SIGNAL.
# The retrieved pattern is assigned to global variable PZ
uploadSignalPatternZ <- function(input, output, selectedPattern) {
  
  if (selectedPattern != SIGNAL_Z)
    return(PZ)
  
  p <- uploadSignalPattern(input, output)
  if (!is.null(p)) PZ <<- p
  
  return(PZ)
  
}  # END uploadSignalPatternZ


# setPatternX
# Obtains a pattern vector from the X signal column in global variable SIGNAL.
# The retrieved pattern is assigned to global variable PX
setPatternX <- function(signal, fromT, toT) {
  
  if (is.null(signal) || is.null(fromT) || is.null(toT) || 
      is.na(fromT)    || is.na(toT)     || (fromT > toT)) {
    PX <<- NULL
    return(PX)
  }
  
  p <- getSignalPattern(signal, SIGNAL_X, fromT, toT)
  
  if (is.null(p) || (length(p) < 1)) {
    PX <<- NULL
    return(PX)
  }
  
  PX <<- p
  
  return(PX)
  
}  # END setPatternX


# setPatternY
# Obtains a pattern vector from the Y signal column in global variable SIGNAL.
# The retrieved pattern is assigned to global variable PY
setPatternY <- function(signal, fromT, toT) {
  
  if (is.null(signal) || is.null(fromT) || is.null(toT) || 
      is.na(fromT)    || is.na(toT)     || (fromT > toT)) {
    PY <<- NULL
    return(PY)
  }
  
  p <- getSignalPattern(signal, SIGNAL_Y, fromT, toT)
  
  if (is.null(p) || (length(p) < 1)) {
    PY <<- NULL
    return(PY)
  }
  
  PY <<- p
  
  return(PY)
  
}  # END setPatternY


# setPatternZ
# Obtains a pattern vector from the Z signal column in global variable SIGNAL.
# The retrieved pattern is assigned to global variable PZ
setPatternZ <- function(signal, fromT, toT) {
  
  if (is.null(signal) || is.null(fromT) || is.null(toT) || 
      is.na(fromT)    || is.na(toT)     || (fromT > toT)) {
    PZ <<- NULL
    return(PZ)
  }
  
  p <- getSignalPattern(signal, SIGNAL_Z, fromT, toT)
  
  if (is.null(p) || (length(p) < 1)) {
    PZ <<- NULL
    return(PZ)
  }
  
  PZ <<- p
  
  return(PZ)
  
}  # END setPatternY


# getDownloadFileName
# Returns a file name used to save a pattern to disk
getDownloadFileName <- function(pattern) {
  
  t  <- Sys.time()
  m  <- month(t)
  d  <- mday(t)
  h  <- hour(t)
  mm <- minute(t)
  ss <- second(t)
  
  dt <- paste0(year(t), "-", 
               ifelse(m  < 10, paste0(ZERO, m), m), "-", 
               ifelse(d  < 10, paste0(ZERO, d), d), "-", 
               ifelse(h  < 10, paste0(ZERO, h), h), "-", 
               ifelse(mm < 10, paste0(ZERO, mm), mm), "-", 
               ifelse(ss < 10, paste0(ZERO, ss), ss))
  
  if (pattern == SIGNAL_X)
    return(paste0("PX_", dt, CSV_EXT))
  
  if (pattern == SIGNAL_Y)
    return(paste0("PY_", dt, CSV_EXT))
  
  if (pattern == SIGNAL_Z)
    return(paste0("PZ_", dt, CSV_EXT))
  
  return(EMPTY_STRING)
  
}  # END getDownloadFileName


# searchPatterns
# Searches for pattern repetitions within signal columns. Patterns
# are given by global variables PX, PY, PZ
searchPatterns <- function(simDegree) {
  
  if (!is.null(SIGNAL)) {
    
    SIGNAL$is_pattern_x <<- 0
    SIGNAL$is_pattern_y <<- 0
    SIGNAL$is_pattern_z <<- 0
    
    SIGNAL <<- findSignalPatterns(SIGNAL, PX, PY, PZ, simDegree, FALSE)
  }
  
  return(SIGNAL)
  
}  # searchPatterns


# getMainViz
# Creates a graph object for "the Signal XYZ" tab
getMainViz <- function() {
  
  if (is.null(SIGNAL))
    return(NULL)
  
  vizData <- as.data.table(cbind(T = SIGNAL$t, X = SIGNAL$x, Y = SIGNAL$y, Z = SIGNAL$z))
  mainViz <- dygraph(vizData, xlab = "T")
  
  return(mainViz)
  
}  # END


# getVizX
# Creates a graph object for the "Signal X" tab
getVizX <- function() {
  
  if (is.null(SIGNAL))
    return(NULL)
  
  vizData <- as.data.table(cbind(T = SIGNAL$t, X = SIGNAL$x, PX = SIGNAL$is_pattern_x))
  viz     <- dygraph(vizData, xlab = "T")
  
  return(viz)
  
}  # getVizX


# getVizY
# Creates a graph object for the "Signal Y" tab
getVizY <- function() {
  
  if (is.null(SIGNAL))
    return(NULL)
  
  vizData <- as.data.table(cbind(T = SIGNAL$t, Y = SIGNAL$y, PY = SIGNAL$is_pattern_y))
  viz     <- dygraph(vizData, xlab = "T")
  
  return(viz)
  
}  # getVizY


# getVizZ
# Creates a graph object for the "Signal Z" tab
getVizZ <- function() {
  
  if (is.null(SIGNAL))
    return(NULL)
  
  vizData <- as.data.table(cbind(T = SIGNAL$t, Z = SIGNAL$z, PZ = SIGNAL$is_pattern_z))
  viz     <- dygraph(vizData, xlab = "T")
  
  return(viz)
  
}  # getVizZ


# Server event handler
shinyServer(
  
  function(input, output, session) {
    
    observeEvent(input$signal, 
                 {updateTabsetPanel(session, TABSET_SIGNAL, TAB_SIGNAL_DATA);
                  output$upLoadedSignal <- DT::renderDataTable(uploadSignalData(input, output));
                  output$viz  <- renderDygraph(getMainViz());
                  output$vizx <- renderDygraph(getVizX());
                  output$vizy <- renderDygraph(getVizY());
                  output$vizz <- renderDygraph(getVizZ());
                 })
    
    observeEvent(input$patternFile, 
                 {output$px <- renderPrint(uploadSignalPatternX(input, output, input$choosePattern));
                 if (input$choosePattern == SIGNAL_X) updateTabsetPanel(session, TABSET_SIGNAL, TAB_PATTERN_X);
                 })
    
    observeEvent(input$patternFile, 
                 {output$py <- renderPrint(uploadSignalPatternY(input, output, input$choosePattern));
                 if (input$choosePattern == SIGNAL_Y) updateTabsetPanel(session, TABSET_SIGNAL, TAB_PATTERN_Y);
                 })
    
    observeEvent(input$patternFile, 
                 {output$pz <- renderPrint(uploadSignalPatternZ(input, output, input$choosePattern));
                 if (input$choosePattern == SIGNAL_Z) updateTabsetPanel(session, TABSET_SIGNAL, TAB_PATTERN_Z);
                 })
    
    observeEvent(input$setPx, 
                 {output$px <- renderPrint(setPatternX(SIGNAL, isolate(input$pxFromT), isolate(input$pxToT)));
                  updateTabsetPanel(session, TABSET_SIGNAL, TAB_PATTERN_X);
                 })
    
    
    observeEvent(input$setPy, 
                 {output$py <- renderPrint(setPatternY(SIGNAL, isolate(input$pyFromT), isolate(input$pyToT)));
                  updateTabsetPanel(session, TABSET_SIGNAL, TAB_PATTERN_Y);
                 })
    
    
    observeEvent(input$setPz, 
                 {output$pz <- renderPrint(setPatternZ(SIGNAL, isolate(input$pzFromT), isolate(input$pzToT)));
                  updateTabsetPanel(session, TABSET_SIGNAL, TAB_PATTERN_Z);
                 })
    
    observeEvent(input$resetAll, resetAll(input, output, session))
    
    observeEvent(input$searchPatterns, 
                 {updateTabsetPanel(session, TABSET_SIGNAL, TAB_SIGNAL_DATA);
                  show_modal_spinner(text = "Please wait...");
                  simDegree <- as.integer(input$chooseSim);
                  p <- searchPatterns(simDegree);
                  output$upLoadedSignal <- DT::renderDataTable(p);
                  remove_modal_spinner();
                 })
    
    observeEvent(input$refreshSignalXYZ, output$viz  <- renderDygraph(getMainViz()))
    observeEvent(input$refreshSignalX,   output$vizx <- renderDygraph(getVizX()))
    observeEvent(input$refreshSignalY,   output$vizy <- renderDygraph(getVizY()))
    observeEvent(input$refreshSignalZ,   output$vizz <- renderDygraph(getVizZ()))
    
    output$downloadPx <- downloadHandler(
                           filename = function() {getDownloadFileName(SIGNAL_X)},
                           content  = function(file) {writeSignalPattern.csv(PX, file, FALSE)})
    
    output$downloadPY <- downloadHandler(
                           filename = function() {getDownloadFileName(SIGNAL_Y)},
                           content  = function(file) {writeSignalPattern.csv(PY, file, FALSE)})
    
    output$downloadPZ <- downloadHandler(
                           filename = function() {getDownloadFileName(SIGNAL_Z)},
                           content  = function(file) {writeSignalPattern.csv(PZ, file, FALSE)})

    gc()
    
  }  # END function
  
)  # END shinyServer
