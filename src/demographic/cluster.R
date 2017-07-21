# cluster.R

# ---- C1 --------
# Load project constants
setwd("/home/rburke/oboc/src/rcr-analysis/src")
source("common.R")

library(caret)
library(stats)
library(gplots)
library(ggplot2)
library(GGally)
library(RColorBrewer)
library(plyr)
library(cluster)
library(reshape2)

# ---- C3 ----
# Read the principal components of the demographic data
path.demo <- paste(RCR_DATA, "branch/branch-data-prcomp-", 
                   DATA_VERSION, ".csv", sep="")
demo <- read.csv(path.demo)

# ---- C4 ----
# Drop the regional libraries and extraneous columns
code.drop <- c("H0", "S1", "W1")

demo.noH0 <- demo[demo$Code!="H0",]
demo.noRG <- demo[!(demo$Code %in% code.drop),]

branchPC16 <- demo.noRG[,c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8",
                      "PC9", "PC10", "PC11", "PC12", "PC13", "PC14", "PC15", "PC16")]


branchPC8 <- demo.noRG[,c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")]


# ---- C5 ----
# A function to explore different clustering

try_pam <- function(data,k) {
  set.seed(327)
  branch.pam <- pam(data, k)
  diss <- daisy(data)
  sil <- silhouette(branch.pam$cluster, diss)
  plot(sil)
  return (branch.pam)
}

# ---- C6 ----
# k = 3
cl16_3 <- try_pam(branchPC16, 3)

# ---- C7 ----
# k = 4
cl16_4 <- try_pam(branchPC16, 4)

# ---- C8 ----
# k = 5
cl16_5 <- try_pam(branchPC16, 5)

# ---- C9 ----
# k = 6
cl16_6 <- try_pam(branchPC16, 6)

# ---- C10 ----
# k = 7
cl16_7 <- try_pam(branchPC16, 7)

# ---- C11 ----
# k = 4
cl8_4 <- try_pam(branchPC8, 4)

# ---- C12 ----
# k = 5
cl8_5 <- try_pam(branchPC8, 5)

# ---- C13 ----
# k = 6
cl8_6 <- try_pam(branchPC8, 6)

# ---- C14 ----
# Slight improvement for PC8, k=5
branch.clust <- data.frame(Code=demo.noRG$Code, Cluster=cl8_5$clustering)
omitted <- data.frame(Code=code.drop, Cluster=6)
full.clust <- rbind(branch.clust, omitted)


# ---- C15 ----
path.clust <- paste(RCR_DATA, "branch/branch-cluster-", 
                   DATA_VERSION, ".csv", sep="")
write.csv(branch.clust, file=path.clust, row.names=FALSE)

# ---- C16 ----
medoids <- cl8_5$medoids

clust.info <- data.frame(Cluster=c("C1", "C2", "C3", "C4", "C5"), PC1=medoids[,1], 
                         PC2X=medoids[,2], PC3=medoids[,3], PC4=medoids[,4])

clust.melt <- melt(clust.info, id.vars=c("Cluster"))
colnames(clust.melt) <- c("Cluster", "Component", "Weight")


# ---- C17 ----
p <- ggplot(data=clust.melt, aes(x=Cluster, y=Weight, fill=Component))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + scale_fill_discrete()
print (p)

# ---- C18 ----
library(RMySQL)
library(getPass)

# ---- C19 ----
con <- dbConnect(MySQL(),
                 user=params$username, password=params$password,
                 dbname="oboc", host="localhost")

# ---- C20 ----
dbSendQuery(con, "DROP TABLE IF EXISTS oboc.temp_cluster")
dbWriteTable(con, "temp_cluster", full.clust)

# ---- C21 ----
update_query <- paste("update CPL_branch B, temp_cluster T ",
    "set B.cluster_branch = T.Cluster where B.code_branch = T.Code",
    sep=' ')
dbSendQuery(con, update_query)

# ---- C22 ----
dbSendQuery(con, "DROP TABLE IF EXISTS oboc.temp_cluster")
dbDisconnect(con)


