#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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

# Decision Tree Function with Balanced Sampling
decision_tree_model <- function(data, region_name = "Region", split_ratio = 0.7) {
  set.seed(1)

  # Balance the dataset
  min_n <- min(table(data$dem_imp))
  data_bal <- data %>%
    group_by(dem_imp) %>%
    slice_sample(n = min_n) %>%
    ungroup()
  
  # Stratified train/test split 
  train_idx <- createDataPartition(data_bal$dem_imp, p = split_ratio, list = FALSE)
  train_data <- data_bal[train_idx, ]
  test_data <- data_bal[-train_idx, ]
  
  # Train the decision tree model
  tree_model <- tree(dem_imp ~ ppltrst + pplhlp + pplfair + trstprl + trstlgl +
                       trstplc + trstplt + trstprt + trstsci,
                     data = train_data)
  
  # Predict on test set
  predictions <- predict(tree_model, test_data, type = "class")
  conf_matrix <- table(Predicted = predictions, Actual = test_data$dem_imp)
  
  # Calculate accuracy
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # Output
  cat("\n=== Unpruned Decision Tree for", region_name, "===\n")
  cat("Balanced Sample Size:", nrow(data_bal), "| Train:", nrow(train_data), "| Test:", nrow(test_data), "\n")
  print(conf_matrix)
  cat(sprintf("Accuracy: %.2f%%\n", accuracy * 100))
  
  # Plot tree
  plot(tree_model)
  text(tree_model, pretty = 0)
  
  return(invisible(list(confusion_matrix = conf_matrix, accuracy = accuracy)))
}


# 70/30 Splits
decision_tree_model(central_2, "Central Europe (70/30)", split_ratio = 0.7)
decision_tree_model(eastern_2, "Eastern Europe (70/30)", split_ratio = 0.7)
decision_tree_model(nordics_2, "Nordics (70/30)", split_ratio = 0.7)
decision_tree_model(southern_2, "Southern Europe (70/30)", split_ratio = 0.7)
decision_tree_model(uk_2, "UK (70/30)", split_ratio = 0.7)
decision_tree_model(west_central_2, "Western Central Europe (70/30)", split_ratio = 0.7)

# 80/20 Split
decision_tree_model(central_2, "Central Europe (80/20)", split_ratio = 0.8)
decision_tree_model(eastern_2, "Eastern Europe (80/20)", split_ratio = 0.8)
decision_tree_model(nordics_2, "Nordics (80/20)", split_ratio = 0.8)
decision_tree_model(southern_2, "Southern Europe (80/20)", split_ratio = 0.8)
decision_tree_model(uk_2, "UK (80/20)", split_ratio = 0.8)
decision_tree_model(west_central_2, "Western Central Europe (80/20)", split_ratio = 0.8)



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
