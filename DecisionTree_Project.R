#install.packages("tree")
#install.packages("caret")
#install.packages("rpart.plot")
# Libraries
library(tree)
library(caret)
library(dplyr)

## Load datasets 
central <- central_europe_clean_csv
eastern <- eastern_europe_clean_csv
nordics <- nordics_clean_csv
southern <- southern_europe_clean_csv
uk <- uk_clean_csv
west_central <- wce_clean_csv

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
  df$implvdm <- NULL
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
  
  data <- na.omit(data)
  data$dem_imp <- as.factor(data$dem_imp)
  
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
