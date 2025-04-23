#install.packages("tree")
#install.packages("caret")
#install.packages("rpart.plot")
# Libraries
library(tree)
library(caret)
library(dplyr)

## Load datasets 
# Load datasets
central <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/central_europe_clean_csv.csv")
eastern <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/eastern_europe_clean_csv.csv")
nordics <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/nordics_clean_csv.csv")
southern <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/southern_europe_clean_csv.csv")
uk <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/uk_clean_csv.csv")
west_central <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/wce_clean_csv.csv")

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
decision_tree_model <- function(data, region_name = "Region", split_ratio = 0.7) {
  set.seed(1)
  
  data <- na.omit(data)
  data$dem_imp <- as.factor(data$dem_imp)
  
  # Balance the dataset
  min_n <- min(table(data$dem_imp))
  data_bal <- data %>%
    group_by(dem_imp) %>%
    slice_sample(n = min_n) %>%
    ungroup()
  
  # Custom train/test split
  train_idx <- createDataPartition(data_bal$dem_imp, p = split_ratio, list = FALSE)
  train_data <- data_bal[train_idx, ]
  test_data <- data_bal[-train_idx, ]
  
  # Train the decision tree model
  tree_model <- tree(dem_imp ~ ppltrst + pplhlp + pplfair + trstprl + trstlgl +
                       trstplc + trstplt + trstprt + trstsci,
                     data = train_data)
  
  # Predict and evaluate
  predictions <- predict(tree_model, test_data, type = "class")
  conf_matrix <- table(Predicted = predictions, Actual = test_data$dem_imp)
  
  # Accuracy
  correct <- sum(diag(conf_matrix))
  total <- sum(conf_matrix)
  accuracy <- correct / total
  
  # Output
  cat("\n=== Confusion Matrix for", region_name, "===\n")
  print(conf_matrix)
  cat(sprintf("\nAccuracy for %s (%.0f/%.0f split): %.2f%%\n", 
              region_name, split_ratio*100, (1-split_ratio)*100, accuracy * 100))
  
  # Plot the decision tree
  cat("\nDecision Tree for", region_name, ":\n")
  plot(tree_model)
  text(tree_model, pretty = 0)
  
  return(list(tree_model = tree_model, test_data = test_data, train_data = train_data))

}

# Prune and evaluate function
prune_and_evaluate <- function(train_data, test_data, region_name = "Region") {
  
  tree_model <- tree(dem_imp ~ ppltrst + pplhlp + pplfair + trstprl + trstlgl +
                       trstplc + trstplt + trstprt + trstsci, data = train_data)
  
  cv_result <- cv.tree(tree_model, FUN = prune.misclass)
  
  plot(cv_result$size, cv_result$dev, type = "b",
       main = paste("CV Tree Size vs Deviance -", region_name),
       xlab = "Tree Size", ylab = "Deviance")
  
  best_size <- cv_result$size[which.min(cv_result$dev)]
  pruned_tree <- prune.misclass(tree_model, best = best_size)
  
  predictions <- predict(pruned_tree, test_data, type = "class")
  conf_matrix <- table(Predicted = predictions, Actual = test_data$dem_imp)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  cat("\n=== Pruned Confusion Matrix for", region_name, "===\n")
  print(conf_matrix)
  cat(sprintf("\nPruned Accuracy for %s: %.2f%%\n", region_name, accuracy * 100))
  
  cat("\n=== Pruned Decision Tree for", region_name, "===\n")
  plot(pruned_tree)
  text(pruned_tree, pretty = 0)
  title(main = paste("Pruned Decision Tree -", region_name))  # <-- Add title
  
  return(invisible(list(
    pruned_tree = pruned_tree,
    confusion_matrix = conf_matrix,
    accuracy = accuracy
  )))
}

# ------- manual --------

# Central Europe
cat("\n### Central Europe (70/30) ###\n")
ce_tree_output <- decision_tree_model(central_2, "Central Europe", split_ratio = 0.7)
train_data <- ce_tree_output$train_data
test_data <- ce_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Central Europe (70/30)")

cat("\n### Central Europe (80/20) ###\n")
ce_tree_output <- decision_tree_model(central_2, "Central Europe", split_ratio = 0.8)
train_data <- ce_tree_output$train_data
test_data <- ce_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Central Europe (80/20)")


# === Eastern Europe ===
cat("\n### Eastern Europe (70/30) ###\n")
ee_tree_output <- decision_tree_model(eastern_2, "Eastern Europe", split_ratio = 0.7)
train_data <- ee_tree_output$train_data
test_data <- ee_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Eastern Europe (70/30)")

cat("\n### Eastern Europe (80/20) ###\n")
ee_tree_output <- decision_tree_model(eastern_2, "Eastern Europe", split_ratio = 0.8)
train_data <- ee_tree_output$train_data
test_data <- ee_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Eastern Europe (80/20)")


# === Nordics ===
cat("\n### Nordics (70/30) ###\n")
n_tree_output <- decision_tree_model(nordics_2, "Nordics", split_ratio = 0.7)
train_data <- n_tree_output$train_data
test_data <- n_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Nordics (70/30)")

cat("\n### Nordics (80/20) ###\n")
n_tree_output <- decision_tree_model(nordics_2, "Nordics", split_ratio = 0.8)
train_data <- n_tree_output$train_data
test_data <- n_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Nordics (80/20)")


# === Southern Europe ===
cat("\n### Southern Europe (70/30) ###\n")
se_tree_output <- decision_tree_model(southern_2, "Southern Europe", split_ratio = 0.7)
train_data <- se_tree_output$train_data
test_data <- se_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Southern Europe (70/30)")

cat("\n### Southern Europe (80/20) ###\n")
se_tree_output <- decision_tree_model(southern_2, "Southern Europe", split_ratio = 0.8)
train_data <- se_tree_output$train_data
test_data <- se_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Southern Europe (80/20)")


# === UK ===
cat("\n### UK (70/30) ###\n")
uk_tree_output <- decision_tree_model(uk_2, "UK", split_ratio = 0.7)
train_data <- uk_tree_output$train_data
test_data <- uk_tree_output$test_data
prune_and_evaluate(train_data, test_data, "UK (70/30)")

cat("\n### UK (80/20) ###\n")
uk_tree_output <- decision_tree_model(uk_2, "UK", split_ratio = 0.8)
train_data <- uk_tree_output$train_data
test_data <- uk_tree_output$test_data
prune_and_evaluate(train_data, test_data, "UK (80/20)")


# === Western Central Europe ===
cat("\n### Western Central Europe (70/30) ###\n")
wce_tree_output <- decision_tree_model(west_central_2, "Western Central Europe", split_ratio = 0.7)
train_data <- wce_tree_output$train_data
test_data <- wce_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Western Central Europe (70/30)")

cat("\n### Western Central Europe (80/20) ###\n")
wce_tree_output <- decision_tree_model(west_central_2, "Western Central Europe", split_ratio = 0.8)
train_data <- wce_tree_output$train_data
test_data <- wce_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Western Central Europe (80/20)")

