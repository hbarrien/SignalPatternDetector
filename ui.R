# ui.R
#
# Date: 
# 2021-03-02
#
# Author:
# Herbert Barrientos
#
# Description:
# User interface definitions for the Shiny application


# ################# LIBRARIES ##################
library(shinyjs)
library(dygraphs)


# ############ EXTERNAL SOURCE CODE ############
source(file="constants.R")


# ################# VARIABLES ##################


# ################# FUNCTIONS ##################
ui <- fluidPage(
        
        # Used for resetting main panel elements
        useShinyjs(),
        
        titlePanel("Signal Pattern Detector"),
        h5("Searches for pattern repetitions within a signal. Proof of concept developed by Herbert Barrientos (herbert.barrientos@exxeta.com)"),
        
        HTML("<a href='SignalInstructions.html' target=_blank>Instructions For Use</a>"),

        sidebarLayout(
          
          sidebarPanel(width=2,
                       h3("Data Upload"),
                       
                       fileInput('signal', 'Signal Data File', accept = c('text/csv', 'text/comma-separated-values', '.csv')),
                       
                       radioButtons("choosePattern", "First, Select a Pattern", c("X" = SIGNAL_X, "Y" = SIGNAL_Y, "Z" = SIGNAL_Z), inline = TRUE),
                       fileInput('patternFile', 'Then a Pattern Data File', accept = c('text/csv', 'text/comma-separated-values', '.csv')),
                       
                       h3("T Range Selection"),

                       div(style="display:inline-block;", numericInput("pxFromT", "Pattern X", 0, min = 0, max = 1000, step = NA, width = "95px")),
                       div(style="display:inline-block;", numericInput("pxToT",   EMPTY_STRING, 0, min = 0, max = 1000, step = NA, width = "95px")),
                       div(style="display:inline-block;", actionButton("setPx", "OK")),
                       
                       div(style="display:inline-block;", numericInput("pyFromT", "Pattern Y", 0, min = 0, max = 1000, step = NA, width = "95px")),
                       div(style="display:inline-block;", numericInput("pyToT",   EMPTY_STRING, 0, min = 0, max = 1000, step = NA, width = "95px")),
                       div(style="display:inline-block;", actionButton("setPy", "OK")),
                       
                       div(style="display:inline-block;", numericInput("pzFromT", "Pattern Z", 0, min = 0, max = 1000, step = NA, width = "95px")),
                       div(style="display:inline-block;", numericInput("pzToT",   EMPTY_STRING, 0, min = 0, max = 1000, step = NA, width = "95px")),
                       div(style="display:inline-block;", actionButton("setPz", "OK")),
                       
                       div(style="text-align:center", actionButton("resetAll", "Reset All")),

                       h3("Search Patterns"),
                       
                       sliderInput("chooseSim", "Similarity Degree", min = 0, max = 6, value = 0),
                       
                       div(style="text-align:center", actionButton("searchPatterns", "Search"))), 
          
          mainPanel(
            
            # Message bar, in case it's needed
            verbatimTextOutput("msg"),
            
            tabsetPanel(
              id = TABSET_SIGNAL,
              tabPanel("Signal Data", DT::dataTableOutput("upLoadedSignal"), value = TAB_SIGNAL_DATA),
              tabPanel("Pattern X",   verbatimTextOutput("px"), br(), downloadButton("downloadPx", "Download"), value = TAB_PATTERN_X), 
              tabPanel("Pattern Y",   verbatimTextOutput("py"), br(), downloadButton("downloadPY", "Download"), value = TAB_PATTERN_Y), 
              tabPanel("Pattern Z",   verbatimTextOutput("pz"), br(), downloadButton("downloadPZ", "Download"), value = TAB_PATTERN_Z),
              
              tabPanel("Signal XYZ",  dygraphOutput("viz"),  actionButton("refreshSignalXYZ", "Refresh"), value = TAB_SIGNAL_XYZ),
              tabPanel("Signal X",    dygraphOutput("vizx"), actionButton("refreshSignalX",  "Refresh"),  value = TAB_SIGNALX),
              tabPanel("Signal Y",    dygraphOutput("vizy"), actionButton("refreshSignalY",  "Refresh"),  value = TAB_SIGNALY),
              tabPanel("Signal Z",    dygraphOutput("vizz"), actionButton("refreshSignalZ",  "Refresh"),  value = TAB_SIGNALZ)
          ))))
