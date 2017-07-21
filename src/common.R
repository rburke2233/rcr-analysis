# common.R
# Robin Burke, Reading Chicago Reading
# DePaul University
# Establishes constants used throughout the RCR codebase

RCR_HOME <- "/home/rburke/oboc/src/rcr-analysis/"
RCR_SRC <- paste(RCR_HOME, "src/", sep="")
RCR_DATA <- paste(RCR_HOME, "data/", sep="")
RCR_RESULTS <- paste(RCR_HOME, "results/", sep="")

 # Versions of the data are determined by date 
  
DATA_VERSION <- "20170108"

# Some branches are ignored for the purposes for analysis. These are either
# 'artificial' branches created for bookkeeping purposes or those that we
# treat differently in the analysis.

BRANCHES_IGNORABLE = c("AD", # Adult book discussion
                       "BK", # Bookmobile 1
                       "BO", # Bookmobile 2
                       "H0-LOCAL", # Local polygon data for Harold Washington. Use H0 instead.
                       "JY", # Junior and young adult services
                       "OM", # Not sure what this is but it isn't a branch
                       "S1-LOCAL", # Local polygon data for Sulzer Regional. Use S1 instead
                       "VR", # Virtual branch
                       "W1-LOCAL" # Local polygon data for Woodson Regional. Use W1 instead
                       )


