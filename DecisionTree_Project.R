#install.packages("tree")
#install.packages("caret")
#install.packages("rpart.plot")
# Libraries
library(tree)
library(caret)
library(dplyr)
library(randomForest)

## Load datasets 
central <- read.csv("central_europe_clean_csv.csv")
eastern <- read.csv("eastern_europe_clean_csv.csv")
nordics <- read.csv("nordics_clean_csv.csv")
southern <- read.csv("southern_europe_clean_csv.csv")
uk <- read.csv("uk_clean_csv.csv")
west_central <- read.csv("wce_clean_csv.csv")

## Making Copies for Analysis
central_2 <- central
eastern_2 <- eastern
nordics_2 <- nordics
southern_2 <- southern
uk_2 <- uk
west_central_2 <- west_central

## Convert variables to factors 
factoring <- function(df) {
  vars <- c("pplfair", "pplhlp", "ppltrst", "trstprl", "trstlgl", "trstplc", 
            "trstplt", "trstprt", "trstsci")
  for (v in vars) {
    df[[v]] <- as.factor(df[[v]])
  }
  return(df)
}

central_2 <- factoring(central_2)
eastern_2 <- factoring(eastern_2)
nordics_2 <- factoring(nordics_2)
southern_2 <- factoring(southern_2)
uk_2 <- factoring(uk_2)
west_central_2 <- factoring(west_central_2)

## Categorize 'implvdm' into Low, Medium, High for 'dem_imp' 
categorize_democracy <- function(df) {
  df$dem_imp <- ifelse(df$implvdm < 5, "Low",
                       ifelse(df$implvdm >= 7, "High", "Medium"))
  df$dem_imp <- as.factor(df$dem_imp)
  #df$implvdm <- NULL
  return(df)
}

central_2 <- categorize_democracy(central_2)
eastern_2 <- categorize_democracy(eastern_2)
nordics_2 <- categorize_democracy(nordics_2)
southern_2 <- categorize_democracy(southern_2)
uk_2 <- categorize_democracy(uk_2)
west_central_2 <- categorize_democracy(west_central_2)

# Decision Tree Function with Balanced Sampling, Cross-validation, and Pruning
decision_tree_model <- function(data, region_name = "Region") {
  set.seed(1)
  
  # data set is already cleaned and we already factored dem_imp in categorize_democracy
  #data <- na.omit(data)
  #data$dem_imp <- as.factor(data$dem_imp)
  
  # Balance the dataset
  min_n <- min(table(data$dem_imp))
  data_bal <- data %>%
    group_by(dem_imp) %>%
    slice_sample(n = min_n) %>%
    ungroup()
  
  # Stratified train/test split
  train_idx <- createDataPartition(data_bal$dem_imp, p = 0.7, list = FALSE)
  train_data <- data_bal[train_idx, ]
  test_data <- data_bal[-train_idx, ]
  
  # Train the decision tree model
  tree_model <- tree(dem_imp ~ ppltrst + pplhlp + pplfair + trstprl + trstlgl +
                       trstplc + trstplt + trstprt + trstsci,
                     data = train_data)
  
  # Predict and evaluate
  predictions <- predict(tree_model, test_data, type = "class")
  conf_matrix <- table(Predicted = predictions, Actual = test_data$dem_imp)
  
  # Calculate accuracy
  correct <- sum(diag(conf_matrix))
  total <- sum(conf_matrix)
  accuracy <- correct / total
  
  # Output
  cat("\n=== Confusion Matrix for", region_name, "===\n")
  print(conf_matrix)
  cat(sprintf("\nAccuracy for %s: %.2f%%\n", region_name, accuracy * 100))
  
  # Plot the decision tree
  cat("\nDecision Tree for", region_name, ":\n")
  plot(tree_model)
  text(tree_model, pretty = 0)
  
  return(invisible(list(confusion_matrix = conf_matrix, accuracy = accuracy)))
}

# Call the function for each regions
decision_tree_model(central_2, "Central Europe")
decision_tree_model(eastern_2, "Eastern Europe")
decision_tree_model(nordics_2, "Nordics")
decision_tree_model(southern_2, "Southern Europe")
decision_tree_model(uk_2, "UK")
decision_tree_model(west_central_2, "Western Central Europe")


random_forest_model <- function(region_data, region_name) {
  set.seed(1)
  forest.train = sample(1:nrow(region_data), nrow(region_data)/2)
  region.xtest = region_data[-forest.train, c(2:10)]
  region.ytest = region_data[-forest.train, 12]
  region.forest = randomForest(dem_imp ~ ppltrst + pplhlp + pplfair + trstprl +
                                 trstlgl + trstplc + trstplt + trstprt + trstsci, 
                               xtest = region.xtest, ytest = region.ytest,  
                               subset = forest.train, mtry = 3, keep.forest = TRUE, 
                               data = region_data, importance = TRUE)
  print(region_name)
  print(importance(region.forest))
  varImpPlot(region.forest, main = region_name)
}

random_forest_model(central_2, "Central Europe")
random_forest_model(eastern_2, "Eastern Europe")
random_forest_model(nordics_2, "Nordics")
random_forest_model(southern_2, "Southern Europe")
random_forest_model(uk_2, "UK")
random_forest_model(west_central_2, "Western Central Europe")
