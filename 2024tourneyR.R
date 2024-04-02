library(tidyverse)
library(readr)
library(dplyr)
library(caret)
library(randomForest)
library(vip)
library(matrixStats)
set.seed(17)
defense <- read.csv("~/Shared/C/Personal Projects/2024tourney/defense24.csv")
offense <- read.csv("~/Shared/C/Personal Projects/2024tourney/offense24.csv")
height <- read.csv("~/Shared/C/Personal Projects/2024tourney/height24.csv")
offenseMisc <- read.csv("~/Shared/C/Personal Projects/2024tourney/misc24off.csv")
defenseMisc <- read.csv("~/Shared/C/Personal Projects/2024tourney/misc24def.csv")
pointDist <- read.csv("~/Shared/C/Personal Projects/2024tourney/pointdist24.csv")
summary <- read.csv("~/Shared/C/Personal Projects/2024tourney/summary24.csv")

# Fix up height df
height <- height %>%
  select(TeamName,
         Season,
         Size,
         HgtEff,
         Exp,
         Bench)
## BIND
offdef <- merge(offense, defense, by = c("TeamName", "Season"), suffixes = c(".offense", ".defense"))
offdefMisc <- merge(offenseMisc, defenseMisc, by = c("TeamName", "Season"), suffixes = c(".offense", ".defense"))
df <- merge(offdef, offdefMisc, by = c("TeamName", "Season"))
df <- merge(df, height, by = c("TeamName", "Season"))
df <- merge(df, pointDist, by = c("TeamName", "Season"))
df <- merge(df, summary, by = c("TeamName", "Season"))

  ## Filter out all ranks
dfWithRank <- df
df <- df[!grepl("Rank", colnames(df))]

## FEATURE SELECTION

## OFFENSE


# Get rid of all offense defense stuff
offenseDf1 <- df[!grepl(".defense", colnames(df))]
# Get rid of variables with no meaning/strings
offenseDf <- offenseDf1 %>%
  select(-TeamName,
         -Season,
         -AdjEM,
         -AdjDE,
         -DE,
         -OE,
         -Def_3,
         -Def_2,
         -Def_1,
         -DFP.offense
         )
# Create empty df to put iterated data in
drop_order <- data.frame(t(offenseDf))  %>%
  select(-everything())
# for (i in 1:100) {
#   col_name <- paste0("iter", i)
#   drop_order <- mutate(drop_order, !!col_name := NA)  # Add a new column with NA values
# }
# 
# for (i in  1:100) {
#   # Make copy of offenseDf as offenseCopy
#   offenseCopy <- data.frame(offenseDf)
#   # Initialize n as len of df - 1 (for explained variable)
#   n <- ncol(offenseCopy) - 1
#   while (ncol(offenseCopy) > 0) {
#     off <- randomForest(AdjOE ~ ., offenseCopy)
#     # Measure Importance
#     importanceMeasure <- importance(off)
#     importanceMeasure <- importanceMeasure[order(importanceMeasure[, "IncNodePurity"]), ]
#     # Drop lease important variable
#     leastImportant <- names(importanceMeasure)[1]
#     if (!is.null(leastImportant) && nchar(leastImportant) > 0) {
#     # Record the least important variable's name
#     drop_order[leastImportant, i] <- n
#     print(leastImportant)
#     # Drop least important column
#     offenseCopy <- offenseCopy[, !(names(offenseCopy) == leastImportant)]
#     } else {
#       # If leastImportant is NULL or empty, break the loop
#       break
#     }
#     # Decrement by 1
#     n = n - 1
#   }
#   print(i)
# }
dropOrderCopy <- drop_order
dropOrderCopy <- dropOrderCopy[rownames(dropOrderCopy) != "AdjOE", ]
dropOrderCopy <- dropOrderCopy %>%
  mutate_all(~ replace_na(., 1))


bestFeatures <- data.frame(
  rowMean = rowMeans(as.matrix(dropOrderCopy)),
  rowSd = rowSds(as.matrix(dropOrderCopy))
)

# Initially select features
offenseSelected <- offenseDf1 %>%
  select(TeamName,
         eFGPct.offense,
         FG2Pct.offense,
         FG3Pct.offense,
         Exp,
         TOPct.offense,
         ORPct.offense,
         NSTRate.offense,
         HgtEff,
         Size,
         OppStlRate.offense,
         FTPct.offense,
         ARate.offense,
         OppFG2Pct.offense,
         FTRate.offense,
         Off_1,
         OppNSTRate.offense,
         Bench,
         AdjOE
         )
offenseSelectedNoTeam <- offenseSelected %>%
  select(-TeamName)
# Check for correlation, remove less important if above 0.7
print(cor(offenseSelectedNoTeam))
# Remove ones we dont want
offenseSelected <- offenseSelected %>%
  select(-FG2Pct.offense,
         -FG3Pct.offense,
         -NSTRate.offense,
         -OppStlRate.offense,
         -Size,
         -Off_1
         )

# Create Training and testing data
train_index <- createDataPartition(y = offenseSelected$AdjOE, p = 0.7, list = FALSE)
train_data <- offenseSelected[train_index, ]
test_data <- offenseSelected[-train_index, ]

# Train the model
offenseForestModel <- randomForest(AdjOE ~ ., train_data)
# Test
offensePredictions <- predict(offenseForestModel, newdata = test_data)
# See Accuracy within 5
accuracy <- mean(abs(offensePredictions - test_data$AdjOE) <= 5) # 88% ACCURACY


# PREDICT ALL
# Train the model
offenseForestModelAll <- randomForest(AdjOE ~ . - TeamName, offenseDf1)
# Test
offensePredictionsAll <- predict(offenseForestModelAll, newdata = offenseDf1)
# Create a data frame with TeamName and predicted AdjOE
predictionsOffenseFinal <- data.frame(
  TeamName = offenseDf1$TeamName,
  Tempo = df$AdjTempo,
  Predicted_AdjOE = offensePredictionsAll
)
# Normalize
predictionsOffenseFinal <- predictionsOffenseFinal %>%
  mutate(averagePtsPerGame = (Predicted_AdjOE) * Tempo / mean(predictionsOffenseFinal$Predicted_AdjOE),
         per100offense = (Predicted_AdjOE) * 100 / mean(predictionsOffenseFinal$Predicted_AdjOE))





## DEFENSE
 




# Get rid of all offense defense stuff
defenseDf1 <- df[!grepl(".offense", colnames(df))]
# Get rid of variables with no meaning/strings
defenseDf <- defenseDf1 %>%
  select(-TeamName,
         -Season,
         -AdjEM,
         -AdjOE,
         -DE,
         -OE,
         -Off_3,
         -Off_2,
         -Off_1,
         -DFP.defense
  )
# Create empty df to put iterated data in
drop_order <- data.frame(t(defenseDf))  %>%
  select(-everything())
# for (i in 1:100) {
#   col_name <- paste0("iter", i)
#   drop_order <- mutate(drop_order, !!col_name := NA)  # Add a new column with NA values
# }
# 
# for (i in  1:100) {
#   # Make copy of defenseDf as defenseCopy
#   defenseCopy <- data.frame(defenseDf)
#   # Initialize n as len of df - 1 (for explained variable)
#   n <- ncol(defenseCopy) - 1
#   while (ncol(defenseCopy) > 0) {
#     def <- randomForest(AdjDE ~ ., defenseCopy)
#     # Measure Importance
#     importanceMeasure <- importance(def)
#     importanceMeasure <- importanceMeasure[order(importanceMeasure[, "IncNodePurity"]), ]
#     # Drop lease important variable
#     leastImportant <- names(importanceMeasure)[1]
#     if (!is.null(leastImportant) && nchar(leastImportant) > 0) {
#       # Record the least important variable's name
#       drop_order[leastImportant, i] <- n
#       print(leastImportant)
#       # Drop least important column
#       defenseCopy <- defenseCopy[, !(names(defenseCopy) == leastImportant)]
#     } else {
#       # If leastImportant is NULL or empty, break the loop
#       break
#     }
#     # Decrement by 1
#     n = n - 1
#   }
#   print(i)
# }
dropOrderCopy <- drop_order
dropOrderCopy <- dropOrderCopy[rownames(dropOrderCopy) != "AdjDE", ]
dropOrderCopy <- dropOrderCopy %>%
  mutate_all(~ replace_na(., 1))


bestFeaturesDef <- data.frame(
  rowMean = rowMeans(as.matrix(dropOrderCopy)),
  rowSd = rowSds(as.matrix(dropOrderCopy))
)

# Initially select features
defenseSelected <- defenseDf1 %>%
  select(TeamName,
         eFGPct.defense,
         OppFG2Pct.defense,
         OppFG3Pct.defense,
         Exp,
         HgtEff,
         StlRate.defense,
         ORPct.defense,
         TOPct.defense,
         FG2Pct.defense,
         BlockPct.defense,
         Size,
         ARate.defense,
         NSTRate.defense,
         OppStlRate.defense,
         Def_2,
         FG3Pct.defense,
         OppNSTRate.defense,
         AdjDE
  )
noNameDefense <- defenseSelected %>%
  select(-TeamName)
# Check for correlation, remove less important if above 0.7
print(cor(noNameDefense))
# Remove ones we dont want
defenseSelected <- defenseSelected %>%
  select(
    -OppFG2Pct.defense,
    -OppFG3Pct.defense,
    -Size,
    -StlRate.defense,
    -OppNSTRate.defense
  )

# Create Training and testing data
train_index <- createDataPartition(y = defenseSelected$AdjDE, p = 0.7, list = FALSE)
train_data <- defenseSelected[train_index, ]
test_data <- defenseSelected[-train_index, ]

# Train the model
defenseForestModel <- randomForest(AdjDE ~ ., train_data)
# Test
defensePredictions <- predict(defenseForestModel, newdata = test_data)
# See Accuracy within 5
accuracy <- mean(abs(defensePredictions - test_data$AdjDE) <= 5) # 89% ACCURACY


# PREDICT ALL
# Train the model
defenseForestModelAll <- randomForest(AdjDE ~ . - TeamName, defenseDf1)
# Test
defensePredictionsAll <- predict(defenseForestModelAll, newdata = defenseDf1)
# Create a data frame with TeamName and predicted AdjDE
predictionsDefenseFinal <- data.frame(
  TeamName = defenseDf1$TeamName,
  Tempo = df$AdjTempo,
  Predicted_AdjDE = defensePredictionsAll
)
# Normalize
predictionsDefenseFinal <- predictionsDefenseFinal %>%
  mutate(averagePtsAllowedPerGame = (Predicted_AdjDE) * Tempo / mean(predictionsDefenseFinal$Predicted_AdjDE),
         per100defense = (Predicted_AdjDE) * 100 / mean(predictionsDefenseFinal$Predicted_AdjDE))



## DONE WITH MODEL MERGE DATA

combinedFinal <- merge(predictionsOffenseFinal, predictionsDefenseFinal, by = c("TeamName", "Tempo"))

combinedFinal <- combinedFinal %>%
  mutate(funAdjEM = averagePtsPerGame - averagePtsAllowedPerGame,
         funAdjKenpomEM = per100offense - per100defense,
         ranking = rank(desc(funAdjKenpomEM)))
combinedFinaltoKenpom <- combinedFinal %>%
  mutate(kenpomRank = dfWithRank$RankAdjEM,
         kenpomEM = df$AdjEM,
         diffEM = kenpomEM - funAdjKenpomEM,
         diffRank = kenpomRank - ranking)
combinedFinal <- combinedFinal[, c("funAdjEM", setdiff(names(combinedFinal), "funAdjEM"))]
combinedFinal <- combinedFinal[, c("TeamName", setdiff(names(combinedFinal), "TeamName"))]
combinedFinal <- combinedFinal[, c("ranking", setdiff(names(combinedFinal), "ranking"))]

combinedFinaltoKenpom <- combinedFinaltoKenpom[, c("funAdjEM", setdiff(names(combinedFinaltoKenpom), "funAdjEM"))]
combinedFinaltoKenpom <- combinedFinaltoKenpom[, c("diffEM", setdiff(names(combinedFinaltoKenpom), "diffEM"))]
combinedFinaltoKenpom <- combinedFinaltoKenpom[, c("funAdjKenpomEM", setdiff(names(combinedFinaltoKenpom), "funAdjKenpomEM"))]
combinedFinaltoKenpom <- combinedFinaltoKenpom[, c("kenpomEM", setdiff(names(combinedFinaltoKenpom), "kenpomEM"))]
combinedFinaltoKenpom <- combinedFinaltoKenpom[, c("TeamName", setdiff(names(combinedFinaltoKenpom), "TeamName"))]
combinedFinaltoKenpom <- combinedFinaltoKenpom[, c("diffRank", setdiff(names(combinedFinaltoKenpom), "diffRank"))]
combinedFinaltoKenpom <- combinedFinaltoKenpom[, c("kenpomRank", setdiff(names(combinedFinaltoKenpom), "kenpomRank"))]
combinedFinaltoKenpom <- combinedFinaltoKenpom[, c("ranking", setdiff(names(combinedFinaltoKenpom), "ranking"))]




## FUNCTION

compareTeams <- function() {
  # Have user input teams
  team1_name <- readline(prompt = "Enter Team 1 name: ")
  team2_name <- readline(prompt = "Enter Team 2 name: ")
  # Find teams in df
  team1_index <- which(combinedFinaltoKenpom$TeamName == team1_name)
  team2_index <- which(combinedFinaltoKenpom$TeamName == team2_name)
  # Throw error if team not found
  if (length(team1_index) == 0 || length(team2_index) == 0) {
    cat("One or both team names not found in the data.\n")
    return(NULL)
  }
  tempoMean <- (df$Tempo[team1_index] + df$Tempo[team2_index]) / 2
  team1EM <- round(combinedFinaltoKenpom$funAdjKenpomEM[team1_index], 2)
  team2EM <- round(combinedFinaltoKenpom$funAdjKenpomEM[team2_index], 2)
  team1offensiveRating <- round(combinedFinaltoKenpom$per100offense[team1_index], 2)
  team2offensiveRating <- round(combinedFinaltoKenpom$per100offense[team2_index], 2)
  team1defensiveRating <- round(combinedFinaltoKenpom$per100defense[team1_index], 2)
  team2defensiveRating <- round(combinedFinaltoKenpom$per100defense[team2_index], 2)
  avgRating1 <- (team1offensiveRating + team1defensiveRating) / 2
  avgRating2 <- (team2offensiveRating + team2defensiveRating) / 2
  pointsScoredTeam1 <- as.numeric(round(combinedFinaltoKenpom$Predicted_AdjOE[team1_index] * tempoMean / mean(combinedFinaltoKenpom$Predicted_AdjOE), 2))
  pointsScoredTeam2 <- as.numeric(round(combinedFinaltoKenpom$Predicted_AdjOE[team2_index] * tempoMean / mean(combinedFinaltoKenpom$Predicted_AdjOE), 2))
  pointsAllowedTeam1 <- as.numeric(round(combinedFinaltoKenpom$Predicted_AdjDE[team1_index] * tempoMean / mean(combinedFinaltoKenpom$Predicted_AdjDE), 2))
  pointsAllowedTeam2 <- as.numeric(round(combinedFinaltoKenpom$Predicted_AdjDE[team2_index] * tempoMean / mean(combinedFinaltoKenpom$Predicted_AdjDE), 2))
  avgEM <- round(abs((team1EM - team2EM)), 2)
  team1scoreEM <- ifelse(team1EM > team2EM, avgEM / 2, (-1 * avgEM) / 2)
  team2scoreEM <- ifelse(team2EM > team1EM, avgEM / 2, (-1 * avgEM) / 2)
  team1adjEM <- team1scoreEM * (tempoMean / 100)
  team2adjEM <- team2scoreEM * (tempoMean / 100)
  avgPoints <- round((pointsScoredTeam1 + pointsScoredTeam2 + pointsAllowedTeam1 + pointsAllowedTeam2) / 4, 2)
  team1score <- avgPoints + team1adjEM
  team2score <- avgPoints + team2adjEM
  bettingLineNum <- ifelse(team1score > team2score, team1score - team2score, team2score - team1score)
  bettingLineTeam <- ifelse(team1score > team2score, team1_name, team2_name)
  cat("\t", team1_name, "\t", team2_name, "\n")
  cat("Ranking\t", combinedFinaltoKenpom$ranking[team1_index], "\t\t", combinedFinaltoKenpom$ranking[team2_index], "\n")
  cat("AdjEM:\t", team1EM, "\t\t", team2EM, "\n")
  cat("Adj. OR:", team1offensiveRating, "\t", team2offensiveRating, "\n")
  cat("Adj. DR:", team1defensiveRating, "\t\t", team2defensiveRating, "\n")
  cat("Score:\t", round(team1score, 1), "\t\t", round(team2score, 1), "\n\n")
  cat("Betting: ", bettingLineTeam, " -", round(bettingLineNum, 1), "\tTotal: ", round(team1score + team2score, 1), sep = "")
}

