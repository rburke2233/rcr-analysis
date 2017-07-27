# time-series.R
# Produce plots based on the circulation time series
# See time-series.Rmd for more details.

# ---- C1 --------
# Load project constants
setwd("/home/rburke/oboc/src/rcr-analysis/src/")
source("common.R")

# ---- C2 ----
library("ggplot2")
library("dplyr")
library("RMySQL")

# ---- C3 ----
# Database connection
con <- dbConnect(MySQL(),
                 user=params$username, password=params$password,
                 dbname="oboc", host="localhost")

# ---- C4 ----
# Aggregate by day and book
series_query <- paste("select count(T.id_trans), T.abbrev_season, T.day_season ",
  "from V_norm_sel_transaction T ",
  "where T.day_season > -90 and T.day_season < 365 ",
  "group by T.abbrev_season, T.day_season", sep="")
rs <- dbSendQuery(con, series_query)
daily.df <- dbFetch(rs, n=-1)
colnames(daily.df) <- c("DayTotal", "Book", "DateOffset")
daily.df$Book <- as.factor(daily.df$Book)


# ---- C5 ----
# Daily line plot
p <- ggplot(data=daily.df, aes(x=DateOffset, y=DayTotal, color=Book))
p <- p + geom_line() + geom_smooth(method="loess", span=0.1)
print (p)

# ---- C6 ----
# Just the smoothed line
p <- ggplot(data=daily.df, aes(x=DateOffset, y=DayTotal, color=Book))
p <- p + geom_smooth(method="loess", span=0.1, se=FALSE)
print (p)

# ---- C7 ----
# Overall density
bookTotal <- as.data.frame(xtabs(DayTotal ~ Book, data=daily.df))
daily.df <- merge(daily.df, bookTotal, by=c("Book"))
daily.df$Fraction <- daily.df$DayTotal / daily.df$Freq
p <- ggplot(daily.df, aes(x=DateOffset, y=Fraction, fill=Book))
p <- p + stat_smooth(geom = 'area', method = 'loess', span = 0.2, alpha = 0.3)
print (p)

# ---- C8 ----
# Weekly aggregate
weekly <- daily.df %>% mutate(WeekOffset=DateOffset %/% 7) %>%
  group_by(Book, WeekOffset) %>% dplyr::summarize(WeekTotal=sum(DayTotal))

weekly.df <- as.data.frame(weekly)

# ---- C9 ----
p <- ggplot(data=weekly.df, aes(x=WeekOffset, y=WeekTotal, color=Book))
p <- p + geom_line()
print (p)


