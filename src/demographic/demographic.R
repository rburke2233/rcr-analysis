### demographic.R

# This code can be run on its own to create the core demographic data set for 
# the Reading Chicago Reading project.

# See demographic.Rmd for full documentation. 

# Load project constants
# ---- C1 -----
setwd("/home/rburke/oboc/src/rcr-analysis/src")
source("common.R")

# Read the data table

# ---- C2 ----
path.in <- paste(RCR_DATA, "branch/branch-data-full-", 
                 DATA_VERSION, ".csv", sep="")
branch.demo <- read.csv(path.in, stringsAsFactors = F)
dim(branch.demo)

# Create a new column for the percentage of the population that is registered to vote.

# ---- C3 ----
branch.demo$PCTvoter <- branch.demo$RegVoter / branch.demo$TotPop

# Drop the columns that we do not use. 

# ---- C4 ----
col.code <- c("Code")
columns <- colnames(branch.demo)
col.circ <- columns[grep("CR_\\d{4}\\Z", columns, perl=T)]
col.vis <- columns[grep("VC_\\d{4}\\Z", columns, perl=T)]
col.pv <- c("MedPV")
col.pct <- columns[grep("PCT", columns, perl=T)]
col.keep <- c(col.code, col.circ, col.vis, col.pv, col.pct)
branch.demo2 <- branch.demo[,col.keep]
dim(branch.demo2) # Should be 88 x 95

# Drop the rows that we do not use.
  
# ---- C5 ----
branch.demo3 <- branch.demo2[!(branch.demo2$Code %in% BRANCHES_IGNORABLE),]
dim(branch.demo3) # Should be 80 x 95


# Save the file in branch-data-reduced-yyyymmdd.csv

# ---- C6 ----
path <- paste(RCR_DATA, "branch/branch-data-reduced-", 
              DATA_VERSION, ".csv", sep="")
write.csv(branch.demo3, path)

