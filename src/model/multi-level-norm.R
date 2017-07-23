# multi-level-model.R
# Computes multi-level regression model for circulation per 1k visitors.

# See multi-level-norm.Rmd for more information

# ---- C1 --------
# Load project constants
setwd("/home/rburke/oboc/src/rcr-analysis/src")
source("common.R")

# ---- C2 -------
# Load libraries
library(plyr)
library(dplyr)
library(reshape2)
library(multilevel)
library(caret)
library(lme4)
library(cvTools)
library(merTools)
library(RMySQL)

# ---- C3 ----
# Read the principal components of the demographic data
path.demo <- paste(RCR_DATA, "branch/branch-data-prcomp-", 
              DATA_VERSION, ".csv", sep="")
demo <- read.csv(path.demo)
# Drop the parts that we don't use
demo <- demo[,c("Code", "CR_2011", "CR_2012", "CR_2013", "CR_2014", "CR_2015",
                "PC1", "PC2", "PC3", "PC4", "PC5", "PC6")]

# ---- C4 ----
# Database connection
con <- dbConnect(MySQL(),
                 user=params$username, password=params$password,
                 dbname="oboc", host="localhost")

# ---- C5 ----
# Circulation query
# Note that the normalized table has only the selected books and only the checkouts.
circ_query <- paste("select count(T.id_trans), T.abbrev_season, T.code_branch ",
                    "from V_norm_sel_transaction T ",
                    "where day_season >= 0 and day_season < 365 ",
                    "group by T.abbrev_season, T.code_branch", sep="")
rs1 <- dbSendQuery(con, circ_query)
circ <- dbFetch(rs1)
colnames(circ) <- c("Freq", "Book", "Code")


# ---- C6 ----
# Read the total holdings data
holds_query <- 
  paste("select O.abbrev_season, R.code_branch, count(H.id_holding) ",
        "from CPL_branch R, CPL_holding H, OBOC_season O, CPL_book B ",
        "where H.book_holding = B.id_book and B.oboc_type_book = 'Y' and ",
        "B.season_book = O.id_season and R.id_branch = H.branch_holding and ",
        "(H.date_holding <= (O.launch_date_season + 365) or H.date_holding is null) ",
        "group by O.abbrev_season, R.code_branch", sep="")
rs2 <- dbSendQuery(con, holds_query)
hold <- dbFetch(rs2)
colnames(hold) <- c("Book", "Code", "Holds")

# ---- C7a ----
year_query = "select O.abbrev_season, extract(year from O.launch_date_season) from OBOC_season O"
rs3 <- dbSendQuery(con, year_query)
book.year <- dbFetch(rs3)
colnames(book.year) <- c("Book", "Year")

# ---- C7b ----
# Extract total circulation data and associate by book-year
circ.year1 <- demo[,c("Code", "CR_2011", "CR_2012", "CR_2013",
                     "CR_2014", "CR_2015")]
colnames(circ.year1) <- c("Code", "2011", "2012", "2013", "2014", "2015")
circ.year2 <- melt(circ.year1, id.vars="Code")
colnames(circ.year2) <- c("Code", "Year", "TotalCirc")
circ.year2$CircS <- as.vector(scale(circ.year2$TotalCirc, center=F, scale=T))
circ.year <- join(circ.year2, book.year, by="Year", type="left")
# The BA branch was closed in 2012
circ.year[is.na(circ.year$CircS),]$CircS <- 0
# Drop Year and TotalCirc columns
circ.year <- circ.year[,c("Code", "Book", "CircS")]

# ---- C7c ----
# Read the visitor data
path.visit <- paste(RCR_DATA, "branch/visitor-count.csv", sep="")
visit <- read.csv(path.visit)
# This replaces NA values with the mean over the non-NA years
visit.noNA <- visit %>% dplyr::group_by(Code) %>% 
    dplyr::mutate(YTD = replace(YTD, is.na(YTD), mean(YTD, na.rm=TRUE)))
visit.year <- join(as.data.frame(visit.noNA), book.year, by="Year", type="left")
# Drop Year column
visit.year <- visit.year[,c("Code", "Book", "YTD")]
visit.year$VisitS <- as.vector(scale(visit.year$YTD, center=F, scale=T))

# ---- C8 ----
# Join book / branch event data for circulation, frequency and holdings
eventdf <- join(circ, hold, by=c("Code", "Book"))
eventdf <- join(eventdf, visit.year, by=c("Code", "Book"))
eventdf <- join(eventdf, circ.year, by=c("Code", "Book"))
fulldf <- join(eventdf, demo, by=c("Code"), type="left")

# ---- C9 ----
# Drop PW, which has no visitor data for some reason
fulldf <- fulldf[!fulldf$Code=="PW",]
# Compute circulation per 1k visitors
fulldf$CircNorm <- fulldf$Freq / fulldf$YTD* 1000
# Transformed PC2 by squaring
fulldf$PC2X <- fulldf$PC2*fulldf$PC2/5.0

# ---- P1 ----
bookcount <- fulldf %>% dplyr::group_by(Book) %>% dplyr::summarize(Total = sum(Freq))

p <- ggplot(bookcount, aes(x=Book, y=Total))
p <- p + geom_bar(stat="identity")
print(p)


# ---- LPFull1 ----
# lattice plots of features
p <- ggplot(data=fulldf, aes(x=Holds, y=CircNorm, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPFull2 ----
p <- ggplot(data=fulldf, aes(x=VisitS, y=CircNorm, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPFull3 ----
p <- ggplot(data=fulldf, aes(x=PC1, y=CircNorm, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPFull4 ----
p <- ggplot(data=fulldf, aes(x=PC2X, y=CircNorm, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPFull4a ----
p <- ggplot(data=fulldf, aes(x=PC2, y=CircNorm, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPFull5 ----
p <- ggplot(data=fulldf, aes(x=PC3, y=CircNorm, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPFull6 ----
p <- ggplot(data=fulldf, aes(x=PC4, y=CircNorm, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)



# ---- M1 ----
m1 <- lm(CircNorm ~ Holds + PC1 + PC2X + PC3 + PC4, data=fulldf)
summary(m1)
AIC(m1)

# ---- M2 ----
m2 <-lmer(CircNorm ~ 1 + (1 | Book), data=fulldf)
summary(m2)
coef(m2)
AIC(m2)
# ICC
#45.73/(45.73+1156.66)

# ---- M3 ----
## By branch

m3 <-lmer(CircNorm ~ 1 + (1 | Code), data=fulldf)
summary(m3)
AIC(m3)
# ICC
#914.8/(914.8+296.1)


# ---- M5 ----
m5 <- lmer(CircNorm ~ Holds + PC1 + PC2X + PC3 +
             (1 + Holds + PC1 + PC2X + PC3 | Book) +
             (1 | Book), data=fulldf, REML=F)
summary(m5)
coef(m5)
AIC(m5)
#55.5/(55.5+376.7)


# ---- C13 ----
feSims5 <- FEsim(m5, n.sims = 100)
plotFEsim(feSims5, level = 0.9, stat = 'median', intercept = FALSE)

# ---- C14 ----
reSims5 <- REsim(m5, n.sims = 100)
plotREsim(reSims5, stat = 'median', sd = TRUE)

# ---- C15 ----
path.results <- paste(RCR_RESULTS, "rdata/lmer-norm-fit-", 
                   DATA_VERSION, ".Rdata", sep="")

save("m5", "fulldf", file=path.results)

# ---- C16 ----
m5.coef <- coef(m5)$Book
m5.coef$Book <- rownames(m5.coef)
m5.coef <- plyr::rename(m5.coef, replace=c("(Intercept)"="Baseline",
                                           "Holds"="Holdings"))
coef.melt <- melt(m5.coef[,-(1:2)], id.vars="Book")

# ---- C17 ----
path.results <- paste(RCR_RESULTS, "lmer-norm-fit-coef-", 
                      DATA_VERSION, ".csv", sep="")

write.csv(coef.melt, file=path.results, row.names=F)

# ---- PCoef1 ----
g <- ggplot(data=coef.melt, aes(x=variable, y=value*1000, fill=Book))
g <- g + geom_bar(stat="identity", position="dodge")
g <- g + scale_y_continuous("Slope")
g <- g + scale_x_discrete("Principal Component")
print(g)

# ---- PCoef2 ----
g <- ggplot(data=m5.coef, aes(x=Book, y=Holdings*1000))
g <- g + geom_bar(stat="identity")
g <- g + scale_y_continuous("Slope (Holdings)")
g <- g + theme_bw()
print(g)

setEPS()
postscript("holdings.eps", width=6.5, height=4.0)
g <- ggplot(data=m5.coef, aes(x=Book, y=Holdings*1000))
g <- g + geom_bar(stat="identity")
g <- g + scale_y_continuous("Slope (Holdings)")
g <- g + theme_bw()
print(g)
dev.off()

# ---- PCoef3 ----
g <- ggplot(data=m5.coef, aes(x=Book, y=Baseline*1000, fill=Book))
g <- g + geom_bar(stat="identity")
g <- g + scale_y_continuous("Intercept")
print(g)
