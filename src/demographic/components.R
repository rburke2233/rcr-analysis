### components.R

# This code can be run on its own to create the principal components of the
# demographic data for the Reading Chicago Reading project.

# Creates a principal components representation of the demographic data.

# See components.Rmd for full documentation. 

# ---- C1 --------
# Load project constants
setwd("/home/rburke/oboc/src/rcr-analysis/src")
source("common.R")

# ---- C2 -------
# Load libraries
library(caret)
library(ggplot2)

# ---- C3 -------
# Load demographic data
path <- paste(RCR_DATA, "branch/branch-data-reduced-", 
              DATA_VERSION, ".csv", sep="")
branch.demo <- read.csv(path)

# ---- C4 -------
# Extract demographic features
features <- branch.demo[,12:95]
features.names <- colnames(features)
branch.remain <- branch.demo[,1:11] # Save for later output

# ---- C5 ----
# Remove features with high correlations
set.seed(1218)
features.cor <- cor(features)
highcorr <- findCorrelation(features.cor, cutoff=0.90, verbose=FALSE, 
                            names=FALSE, exact=TRUE)
features.filtered <- features[,-highcorr]

# ---- C6 ------
# Remove columns with low incidence
lowcols <- apply(features.filtered, 2, max)>=1
features.filtered <- features.filtered[,lowcols]

# ---- C7 ------
# Compute principal components
set.seed(1218)
features.components <- prcomp(features.filtered, center = TRUE, scale.=TRUE)
summary(features.components)

# ---- C8 ------
# Create a data frame with the transformed features
branch.transform <- cbind(branch.remain, features.components$x[,1:16])

# ---- C9 -----
# Save the data
path <- paste(RCR_DATA, "branch/branch-data-prcomp-", 
              DATA_VERSION, ".csv", sep="")
write.csv(branch.transform, path)

# ---- C10 ----
# Save the principal components computation
path <- paste(RCR_RESULTS, "rdata/branch-data-prcomp-", 
              DATA_VERSION, ".RData", sep="")
save(features.components, file=path)

