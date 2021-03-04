# constants.R
# Date:
# 2021-0-02
#
# Author: 
# Herbert Barrientos
#
# Description:
# Definition of constants for the signal application 


# Similarity Degree
SIM_0 <- 0
SIM_1 <- 1
SIM_2 <- 2
SIM_3 <- 3
SIM_4 <- 4
SIM_5 <- 5
SIM_6 <- 6

# Signal names
SIGNAL_X <- "x"
SIGNAL_Y <- "y"
SIGNAL_Z <- "z"

# Pattern detection
NO_HIT <- 0
HIT    <- 1

# Definitions for ui.R
TABSET_SIGNAL    <- "signalTabs"
TAB_SIGNAL_DATA  <- "signalData"
TAB_PATTERN_X    <- "patternX"
TAB_PATTERN_Y    <- "patternY"
TAB_PATTERN_Z    <- "patternZ"
TAB_SIGNAL_XYZ   <- "signalXYZ"
TAB_SIGNALX      <- "signalX"
TAB_SIGNALY      <- "signalY"
TAB_SIGNALZ      <- "signalZ"
TAB_INSTRUCTIONS <- "instructions"

# Formatting
ZERO         <- "0"
EMPTY_STRING <- ""
CSV_EXT   <- ".csv"

# User messages
MSG_TITLE <- "Msg: "
MSG_WAIT  <- paste0(MSG_TITLE, "Please wait...")
MSG_DONE  <- paste0(MSG_TITLE, "Done.")
