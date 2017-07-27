# circ-strip.R
# Visualization of circulation by book and cluster
# See circ-strip.Rmd for more details

# ---- C1 --------
# Load project constants
setwd("/home/rburke/oboc/src/rcr-analysis/src/")
source("common.R")

# ---- C2 ----
### Load packages
library(caret)
library(stats)
library(gplots)
library(ggplot2)
library(GGally)
library(RColorBrewer)
library(plyr)
library(dplyr)
library(RMySQL)

# ---- C3 ----
# Read the cluster table
path.cluster <- paste(RCR_DATA, "branch/branch-cluster-", 
                      DATA_VERSION, ".csv", sep="")
cluster <- read.csv(path.cluster)
cluster$Cluster <- as.factor(cluster$Cluster)

# ---- C4a ----
# Database connection
con <- dbConnect(MySQL(),
                 user=params$username, password=params$password,
                 dbname="oboc", host="localhost")

# ---- C4b ----
# Circulation query
# Note that the normalized table has only the selected books and only the checkouts.
circ_query <- paste("select count(T.id_trans), T.abbrev_season, T.code_branch ",
                    "from V_norm_sel_transaction T ",
                    "where day_season >= 0 and day_season < 365 ",
                    "group by T.abbrev_season, T.code_branch", sep="")
rs1 <- dbSendQuery(con, circ_query)
circ <- dbFetch(rs1)
colnames(circ) <- c("Freq", "Book", "Code")

# ---- C5 ----
circ.clust <- join(circ, cluster, by="Code", type="left")
# Drop zeros
circ.clust <- circ.clust[circ.clust$Freq>0,]

# ---- C6 ----
p <- ggplot(data=circ.clust, aes(x=Book, y=Freq, label=Code, color=Cluster))
p <- p + geom_text(position=position_jitter(width=0.3))
p <- p + scale_y_log10()
print(p)

# ---- C7a ----
# Read the visitor data
path.visit <- paste(RCR_DATA, "branch/visitor-count.csv", sep="")
visit <- read.csv(path.visit)
# This replaces NA values with the mean over the non-NA years
visit.noNA <- visit %>% dplyr::group_by(Code) %>% 
  dplyr::mutate(YTD = replace(YTD, is.na(YTD), mean(YTD, na.rm=TRUE)))

# ---- C7b ----
year_query = "select O.abbrev_season, extract(year from O.launch_date_season) from OBOC_season O"
rs3 <- dbSendQuery(con, year_query)
book.year <- dbFetch(rs3)
colnames(book.year) <- c("Book", "Year")

# ---- C7c ----
visit.year <- join(as.data.frame(visit.noNA), book.year, by="Year", type="left")
# Drop Year column
visit.year <- visit.year[,c("Code", "Book", "YTD")]

# ---- C8 ----
circ.join <- join(circ.clust, visit.year, by=c("Code", "Book"), type="left")
circ.join$ChPer1K <- circ.join$Freq / circ.join$YTD * 1000
# No visitor data for PW
circ.join <- circ.join[circ.join$Code!="PW",]

# ---- C9 ----
p <- ggplot(data=circ.join, aes(x=Book, y=ChPer1K, label=Code, color=Cluster))
p <- p + geom_text(position=position_jitter(width=0.3))
p <- p + scale_y_log10("Checkouts per 1K Visitors")
print(p)
