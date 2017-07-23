# multi-level-model.R
# Computes multi-level regression model for circulation.

# TODO: Segment traffic data

# See multi-level-model.Rmd for more information

# ---- C1 --------
# Load project constants
setwd("/home/rburke/oboc/src/")
source("params.R")

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

# ---- C3 ----
# Read the principal components of the demographic data
path.demo <- paste(RCR_DATA, "branch/branch-data-prcomp-", 
              DATA_VERSION, ".csv", sep="")
demo <- read.csv(path.demo)
# Drop the parts that we don't use
demo <- demo[,c("Code", "CR_2011", "CR_2012", "CR_2013", "CR_2014", "CR_2015",
                "PC1", "PC2", "PC3", "PC4", "PC5", "PC6")]

# ---- C4 ----
# Read the total circulation data
path.circ <- paste(RCR_DATA, "circulation-total-", 
                  DATA_VERSION, ".csv", sep="")
circ <- read.csv(path.circ)
# For now, drop the holds and only use the check-outs
circ <- circ[circ$Type=="CH",]
circ <- circ[,c("Code", "Book", "Freq")]

# ---- C5 ----
# Read the total holdings data
path.hold <- paste(RCR_DATA, "holdings-", 
                   DATA_VERSION, ".csv", sep="")
hold <- read.csv(path.hold)

# ---- C6 ----
# Extract total circulation data and associate by book-year
book.year <- data.frame(Book=BOOK_LAUNCH$Book, 
                        Year=as.POSIXlt(as.Date(BOOK_LAUNCH$Date))[[6]]+1900)
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

# ---- C7 ----
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
fulldf.noH0 <- fulldf[!(fulldf$Code=="H0"),]
fulldf.noRG <- fulldf.noH0[!(fulldf.noH0$Code=="S1"),]
fulldf.noRG <- fulldf.noRG[!(fulldf.noRG$Code=="W1"),]

# ---- C10 ----
cor(fulldf$CircS, fulldf$VisitS)

# ---- P1 ----
bookcount <- fulldf %>% dplyr::group_by(Book) %>% dplyr::summarize(Total = sum(Freq))
p <- ggplot(bookcount, aes(x=Book, y=Total))
p <- p + geom_bar(stat="identity")
print(p)


# ---- LPFull1 ----
# lattice plots of features
p <- ggplot(data=fulldf, aes(x=Holds, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPFull2 ----
p <- ggplot(data=fulldf, aes(x=VisitS, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPFull3 ----
p <- ggplot(data=fulldf, aes(x=PC1, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPFull4 ----
p <- ggplot(data=fulldf, aes(x=PC2, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPFull5 ----
p <- ggplot(data=fulldf, aes(x=PC3, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPFull6 ----
p <- ggplot(data=fulldf, aes(x=PC4, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)



# ---- LPRG1 ----
# lattice plots of features
p <- ggplot(data=fulldf.noRG, aes(x=Holds, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPRG2 ----
p <- ggplot(data=fulldf.noRG, aes(x=VisitS, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# Chinatown is a huge outlier.

# ---- LPRG3 ----
p <- ggplot(data=fulldf.noRG, aes(x=PC1, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPRG4 ----
p <- ggplot(data=fulldf.noRG, aes(x=PC2, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPRG5 ----
p <- ggplot(data=fulldf.noRG, aes(x=PC3, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- LPRG6 ----
p <- ggplot(data=fulldf.noRG, aes(x=PC4, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)


# ---- C11 ---
mpc2a <- lm(Freq ~ PC2, data=fulldf.noRG)
summary(mpc2a)
mpc2b <- lm(Freq ~ PC2*PC2, data=fulldf.noRG)
summary(mpc2b)

# ---- C12 ----
fulldf.XF <- fulldf.noRG
fulldf.XF <- fulldf.XF[!fulldf.XF$Code=="CH",]
# Division by 5 gives it a similar range to the other PCs
fulldf.XF$PC2X <- fulldf.XF$PC2*fulldf.XF$PC2/5.0

# ---- LPXF1 ----
p <- ggplot(data=fulldf.XF, aes(x=PC2X, y=Freq, label=Code))
p <- p + geom_text() + geom_smooth(method="lm")
p <- p + facet_wrap(~Book)
print(p)

# ---- M1 ----
m1 <- lm(Freq ~ Holds + VisitS + PC1 + PC2X + PC3 + PC4, data=fulldf.XF)
summary(m1)
AIC(m1)

# ---- M2 ----
m2 <-lmer(Freq ~ 1 + (1 | Book), data=fulldf.XF)
summary(m2)
coef(m2)
AIC(m2)
# ICC
45.73/(45.73+1156.66)

# ---- M3 ----
## By branch

m3 <-lmer(Freq ~ 1 + (1 | Code), data=fulldf.XF)
summary(m3)
AIC(m3)
# ICC
914.8/(914.8+296.1)

# ---- M4 ----
m4 <- lmer(Freq ~ Holds + VisitS + PC1 + PC2X + PC3 + PC4 +
             (1 | Book), data=fulldf.XF)
summary(m4)
coef(m4)
AIC(m4)
63/(360+63)

# ---- MBad ----
#m <- lmer(Freq ~ Holds + VisitS + PC1 + PC2X + PC3 + PC4 +
#             (1 + Holds + VisitS + PC1 + PC2X + PC3 + PC4 | Book) +
#             (1 | Book), data=fulldf.XF, REML=F, verbose=1)
#m <- lmer(Freq ~ Holds + PC1 + PC2X + PC3 + PC4 +
#             (1 + Holds + PC1 + PC2X + PC3 + PC4 | Book) +
#             (1 | Book), data=fulldf.XF, REML=F, verbose=1)

# ---- M5 ----
m5 <- lmer(Freq ~ Holds + PC1 + PC2X + PC3 +
             (1 + Holds + PC1 + PC2X + PC3 | Book) +
             (1 | Book), data=fulldf.XF, REML=F)
summary(m5)
coef(m5)
AIC(m5)
55.5/(55.5+376.7)

# ---- C13 ----
feSims5 <- FEsim(m5, n.sims = 100)
plotFEsim(feSims5, level = 0.9, stat = 'median', intercept = FALSE)

# ---- C14 ----
reSims5 <- REsim(m5, n.sims = 100)
plotREsim(reSims5, stat = 'median', sd = TRUE)

# ---- C15 ----
path.results <- paste(RCR_RESULTS, "rdata/lmer-fit-", 
                   DATA_VERSION, ".Rdata", sep="")

save("m5", "fulldf.XF", file=path.results)

# ---- C16 ----
m5.coef <- coef(m5)$Book
m5.coef$Book <- rownames(m5.coef)
m5.coef <- plyr::rename(m5.coef, replace=c("(Intercept)"="Baseline",
                                           "Holds"="Holdings"))
coef.melt <- melt(m5.coef[,-(1:2)], id.vars="Book")

# ---- C17 ----
path.results <- paste(RCR_RESULTS, "lmer-fit-coef-", 
                      DATA_VERSION, ".csv", sep="")

write.csv(coef.melt, file=path.results, row.names=F)

# ---- PCoef1 ----
g <- ggplot(data=coef.melt, aes(x=variable, y=value, fill=Book))
g <- g + geom_bar(stat="identity", position="dodge")
g <- g + scale_y_continuous("Slope")
g <- g + scale_x_discrete("Principal Component")
print(g)

# ---- PCoef2 ----
g <- ggplot(data=m5.coef, aes(x=Book, y=Holdings, fill=Book))
g <- g + geom_bar(stat="identity")
g <- g + scale_y_continuous("Slope (Holdings)")
print(g)

# ---- PCoef3 ----
g <- ggplot(data=m5.coef, aes(x=Book, y=Baseline, fill=Book))
g <- g + geom_bar(stat="identity")
g <- g + scale_y_continuous("Intercept")
print(g)

# ---- RLM1 ----
fulldf$PC2X <- fulldf$PC2*fulldf$PC2/5.0

rm1 <- rlm(Freq ~ Holds + PC1 + PC2X + PC3, data=fulldf,
           method="MM", wt.method="inv.var")
summary(rm1)
AIC(rm1)
summary(rm1$w)

# ---- PWeight ----
p <- ggplot(data=data.frame(Book=fulldf$Book, Weight=rm1$w, Code=fulldf$Code),
            aes(x=Book, y=Weight, label=Code)) +
      geom_text()
print (p)

# ---- M5F ----
m5f <- lmer(Freq ~ Holds + PC1 + PC2X + PC3 +
             (1 + Holds + PC1 + PC2X + PC3 | Book) +
             (1 | Book), data=fulldf, REML=F, verbose=1,
            weights=rm1$w+0.1)
summary(m5f)
coef(m5f)
AIC(m5f)

# ---- C18 ----
m5f.coef <- coef(m5f)$Book
m5f.coef$Book <- rownames(m5f.coef)
m5f.coef <- plyr::rename(m5f.coef, replace=c("(Intercept)"="Baseline",
                                           "Holds"="Holdings"))
coef.melt.f <- melt(m5f.coef[,-(1:2)], id.vars="Book")
# Very similar to original weights
cor(coef.melt.f$value, coef.melt$value)

# ---- C19 ----
path.results <- paste(RCR_RESULTS, "lmer-full-coef-", 
                      DATA_VERSION, ".csv", sep="")

write.csv(coef.melt.f, file=path.results, row.names=F)

# ---- PCoef4 ----
g <- ggplot(data=coef.melt.f, aes(x=variable, y=value, fill=Book))
g <- g + geom_bar(stat="identity", position="dodge")
g <- g + scale_y_continuous("Slope")
g <- g + scale_x_discrete("Principal Component")
print(g)

# ---- PCoef5 ----
g <- ggplot(data=m5f.coef, aes(x=Book, y=Holdings, fill=Book))
g <- g + geom_bar(stat="identity")
g <- g + scale_y_continuous("Slope (Holdings)")
print(g)

# ---- PCoef6 ----
g <- ggplot(data=m5f.coef, aes(x=Book, y=Baseline, fill=Book))
g <- g + geom_bar(stat="identity")
g <- g + scale_y_continuous("Intercept")
print(g)
