# Setup----

suppressMessages(library(dplyr))
library(ggplot2)
library(ggExtra)
library(tidyr)
library(grid)
library(lubridate)
library(RColorBrewer)
library(gridExtra)
library(cowplot)
library(airtabler)
#library(chron)

xwords_airtable_obj <- 
  airtable(
    base = "appswGBfJabphdFCx", 
    tables = c("Log")
  )
xwords_df <- xwords_airtable_obj$Log$select_all()

names(xwords_df)[names(xwords_df)=="Puzzle date"] = "PuzzleDate"
names(xwords_df)[names(xwords_df)=="Solve date"] = "SolveDate"
names(xwords_df)[names(xwords_df)=="Solve time"] = "SolveTime"

# Format
xwords_df$PuzzleDate <- as.Date(xwords_df$PuzzleDate)
xwords_df$Day2 <- factor(xwords_df$Day, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
tfmt <- "%H:%M:%S"
xwords_df$SolveTime <- as.duration(xwords_df$SolveTime)

# Get "current era"
xwords_df = xwords_df[xwords_df$PuzzleDate>=as.Date("2019-01-07"),]
# xwords_df = xwords_df[xwords_df$PuzzleDate == xwords_df$SolveDate,]
Nrecords = length(xwords_df$PuzzleDate)
xwords_df = xwords_df[order(xwords_df$PuzzleDate, decreasing = TRUE),]

# Find gaps ----
Nstreak = 1
for (i in seq(1, Nrecords-1)) {
  if (xwords_df$PuzzleDate[i+1] != xwords_df$PuzzleDate[i]-1) {
    break
    # print(xwords_df$PuzzleDate[i]-1)
  }
  Nstreak = Nstreak + 1
  streakStartDate = xwords_df$PuzzleDate[i+1]
}
# print(Nstreak)
# print(streakStartDate)
