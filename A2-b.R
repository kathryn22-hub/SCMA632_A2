# Install and Load Necessary Packages
options(repos = c(CRAN = "https://cran.rstudio.com/"))

if (!require("stringdist")) install.packages("stringdist")
if (!require("car")) install.packages("car")
if (!require("caret")) install.packages("caret")
if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
if (!require("readxl")) install.packages("readxl")




library(dplyr)
library(readr)
library(readxl)

# Set the working directory
setwd('E:\\ASSIGNMENT\\Data')
getwd()

# Load the data sets
df_ipl <- read_csv("IPL_ball_by_ball_updated till 2024.csv")
salary <- read_excel("IPL SALARIES 2024.xlsx")

# Display column names
print(colnames(df_ipl))

# Group the data by Season, Striker, and Bowler
grouped_data <- df_ipl %>%
  group_by(Season, Striker, Bowler) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE), 
            wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()
print(grouped_data)

# Calculate total runs for each player in each season
total_runs_each_year <- grouped_data %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup()
print(total_runs_each_year)

# Calculate total wickets for each bowler in each season
total_wicket_each_year <- grouped_data %>%
  group_by(Season, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()
print(total_wicket_each_year)

# Install the `stringdist` package if not already installed
install.packages("stringdist")
library(stringdist)

# Function to match names
match_names <- function(name, names_list) {
  match <- amatch(name, names_list, method = "jw", maxDist = 0.2)
  if (!is.na(match)) return(names_list[match])
  return(NA)
}


salary$Matched_Player <- sapply(salary$Player, match_names, names_list = total_runs_each_year$Striker)

df_merged <- merge(salary, total_runs_each_year, by.x = "Matched_Player", by.y = "Striker", all.x = TRUE)
df_original <- df_merged

df_merged <- df_merged %>% filter(Season %in% c('2021', '2022', '2023'))
print(unique(df_merged$Season))
print(head(df_merged))


# Linear regression model
X <- df_merged$runs_scored
y <- df_merged$Rs

install.packages("caret")
install.packages("car")
library(caret)
library(car)

# Split the data into training and test sets (80% for training, 20% for testing)
set.seed(42)
trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[trainIndex]
X_test <- X[-trainIndex]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]

# Fit the model on the training data
model <- lm(y_train ~ X_train)
print(summary(model))


# Matching bowlers
salary$Matched_Player <- sapply(salary$Player, match_names, names_list = total_wicket_each_year$Bowler)

# Merge the DataFrames on the matched names
df_merged_bowler <- merge(salary, total_wicket_each_year, by.x = "Matched_Player", by.y = "Bowler", all.x = TRUE)
df_merged_bowler <- df_merged_bowler %>% filter(wicket_confirmation > 10)
print(head(df_merged_bowler))

# Linear regression model for bowlers
X_bowler <- df_merged_bowler$wicket_confirmation
y_bowler <- df_merged_bowler$Rs

# Split the data into training and test sets (80% for training, 20% for testing)
trainIndex_bowler <- createDataPartition(y_bowler, p = 0.8, list = FALSE)
X_train_bowler <- X_bowler[trainIndex_bowler]
X_test_bowler <- X_bowler[-trainIndex_bowler]
y_train_bowler <- y_bowler[trainIndex_bowler]
y_test_bowler <- y_bowler[-trainIndex_bowler]

# Fit the model on the training data
model_bowler <- lm(y_train_bowler ~ X_train_bowler)
print(summary(model_bowler))


