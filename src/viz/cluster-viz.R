# cluster-viz.R
# Various visualizations of the clusters
# See cluster-viz.Rmd for more details

# ---- C1 --------
# Load project constants
setwd("/home/rburke/oboc/src/rcr-analysis/src/")
source("common.R")

# ---- C2 ----
### Load packages
library(ggplot2)
library(GGally)
library(RColorBrewer)
library(plyr)
library(reshape2)
library(maptools)
library(plotrix)
library(classInt)
library(maps)
library(ggmap)
library(rgdal)
library(mapproj)
library(RMySQL)

# ---- C3 ----
# Read the principal components of the demographic data
path.demo <- paste(RCR_DATA, "branch/branch-data-prcomp-", 
                   DATA_VERSION, ".csv", sep="")
demo <- read.csv(path.demo)

# ---- C4 ----
# Read the cluster table
path.cluster <- paste(RCR_DATA, "branch/branch-cluster-", 
                      DATA_VERSION, ".csv", sep="")
cluster <- read.csv(path.cluster)
cluster$Cluster <- as.factor(cluster$Cluster)

# ---- C5 ----
# Database connection
con <- dbConnect(MySQL(),
                 user=params$username, password=params$password,
                 dbname="oboc", host="localhost")
location_query <- paste("select code_branch, lat_branch, long_branch ",
  " from CPL_branch where lat_branch is not NULL")
rs1 <- dbSendQuery(con, location_query)
branchpt <- dbFetch(rs1)
colnames(branchpt) <- c("Code", "Latitude", "Longitude")

# ---- C6 ----
# Read in the chicago community areas map
path.map <- paste(RCR_DATA, "aux/CommAreas.shp", sep="")

comm <- readOGR(dsn=path.map, layer="CommAreas")

# ---- C7 ----
branches <- join(branchpt, cluster, by="Code", type="left")

# ---- C8 ----
# Transform to Lat,Long coordinate system
comm.proj <- spTransform(comm, CRS("+proj=longlat +datum=WGS84"))
# Turn into a data frame for ggplot
comm.proj = fortify(comm.proj, region="COMMUNITY")

# ---- C9 ----

pl <- ggplot() + 
    # Community areas outlined in white
    geom_polygon(data=comm.proj, aes(x=long, y=lat, group=group),
                 color="white", show.legend=FALSE)
pl <- pl +
    # Branches colored by cluster
    geom_point(data=branches, aes(x=Longitude, y=Latitude, 
                                  fill=Cluster, 
                                  color="black",
                                  size=5),
               pch=21)
pl <- pl + scale_fill_brewer("Clusters", palette="Dark2",
                                breaks=seq(1,6),
                                labels=seq(1,6))
pl <- pl + guides(fill=guide_legend(override.aes=list(size=5)),
                    color="none", size="none")
pl <- pl + coord_map() + theme_nothing(legend=TRUE)
print(pl)

# ---- C10 ----
code.drop <- c("H0", "S1", "W1")
demo.noRG <- demo[!(demo$Code %in% code.drop),]
demo.clust <- join(demo, cluster, by="Code", type="left")
branchPC8 <- demo.clust[,c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")]

# ---- C11 ----
d <- dist(branchPC8) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim

# ---- C12 ----
mds <- data.frame(Coord1=fit$points[,1], Coord2=fit$points[,2],
                  Code=demo.clust$Code, 
                  Cluster=as.factor(demo.clust$Cluster))

p <- ggplot(data=mds, aes(x=Coord1, y=Coord2, color=Cluster, label=Code))
p <- p + geom_text(fontface="bold")
p <- p + scale_color_brewer("Clusters", palette="Dark2",
                                      breaks=seq(1,5),
                                      labels=seq(1,5))
p <- p + guides(color=guide_legend(override.aes=list(size=5)), size="none")
print (p)

# ---- C13 ----
branch.pc <- demo.noRG[,c("Code", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8")]
branch.clust <- join(cluster, branch.pc, by="Code", type="left")

clust.info <- ddply(branch.clust, .(Cluster), summarize, 
      PC1=mean(PC1), PC2=mean(PC2), PC3=mean(PC3), PC4=mean(PC4))

clust.info$Cluster=c("C1", "C2", "C3", "C4", "C5")

clust.melt<- melt(clust.info, id.vars=c("Cluster"))
colnames(clust.melt) <- c("Cluster", "Component", "Weight")

# ---- C14 ----
p <- ggplot(data=clust.melt, aes(x=Cluster, y=Weight, fill=Component))
p <- p + geom_bar(stat="identity", position="dodge")
p <- p + scale_fill_discrete()
print (p)


