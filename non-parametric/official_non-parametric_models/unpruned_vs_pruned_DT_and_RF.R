# credit to Tan T for base Decision Trees and Random Forest

# load necessary libraries
library(tree)
library(caret)
library(dplyr)
library(randomForest)

# Llad datasets
central <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/central_europe_clean_csv.csv")
eastern <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/eastern_europe_clean_csv.csv")
nordics <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/nordics_clean_csv.csv")
southern <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/southern_europe_clean_csv.csv")
uk <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/uk_clean_csv.csv")
west_central <- read.csv("~/Documents/Github/ESS_Data_Science/non-parametric/wce_clean_csv.csv")

# making Copies for Analysis
central_2 <- central
eastern_2 <- eastern
nordics_2 <- nordics
southern_2 <- southern
uk_2 <- uk
west_central_2 <- west_central
# Convert variables to factors
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

# Categorize 'implvdm' into Low, Medium, High for 'dem_imp'
categorize_democracy <- function(df) {
  df$dem_imp <- ifelse(df$implvdm < 5, "Low", 
                       ifelse(df$implvdm >= 7, "High", "Medium"))
  df$dem_imp <- as.factor(df$dem_imp)
  return(df)
}

central_2 <- categorize_democracy(central_2)
eastern_2 <- categorize_democracy(eastern_2)
nordics_2 <- categorize_democracy(nordics_2)
southern_2 <- categorize_democracy(southern_2)
uk_2 <- categorize_democracy(uk_2)
west_central_2 <- categorize_democracy(west_central_2)

# Decision tree model function
decision_tree_model <- function(data, region_name = "Region") {
  set.seed(1)
  
  min_n <- min(table(data$dem_imp))
  data_bal <- data %>%
    group_by(dem_imp) %>%
    slice_sample(n = min_n) %>%
    ungroup()
  
  train_idx <- createDataPartition(data_bal$dem_imp, p = 0.7, list = FALSE)
  train_data <- data_bal[train_idx, ]
  test_data <- data_bal[-train_idx, ]
  
  tree_model <- tree(dem_imp ~ ppltrst + pplhlp + pplfair + trstprl + trstlgl +
                       trstplc + trstplt + trstprt + trstsci, data = train_data)
  
  predictions <- predict(tree_model, test_data, type = "class")
  conf_matrix <- table(Predicted = predictions, Actual = test_data$dem_imp)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  cat("\n=== Unpruned Confusion Matrix for", region_name, "===\n")
  print(conf_matrix)
  cat(sprintf("\nUnpruned Accuracy for %s: %.2f%%\n", region_name, accuracy * 100))
  
  cat("\n=== Unpruned Decision Tree for", region_name, "===\n")
  plot(tree_model)
  text(tree_model, pretty = 0)
  title(main = paste("Unpruned Decision Tree -", region_name))  # <-- Add title
  
  return(list(tree_model = tree_model, test_data = test_data, train_data = train_data))
}


# prune and evaluate function
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


# region-specific tree evaluations

# Central Europe
cat("\n### Central Europe ###\n")
central_tree_output <- decision_tree_model(central_2, "Central Europe")
train_data <- central_tree_output$train_data
test_data <- central_tree_output$test_data
pruned_output <- prune_and_evaluate(train_data, test_data, "Central Europe")

### Eastern Europe ###
eastern_tree_output <- decision_tree_model(eastern_2, "Eastern Europe")
train_data <- eastern_tree_output$train_data
test_data <- eastern_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Eastern Europe")

### Nordics ###
nordics_tree_output <- decision_tree_model(nordics_2, "Nordics")
train_data <- nordics_tree_output$train_data
test_data <- nordics_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Nordics")

### Southern Europe ###
southern_tree_output <- decision_tree_model(southern_2, "Southern Europe")
train_data <- southern_tree_output$train_data
test_data <- southern_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Southern Europe")

### UK ###
uk_tree_output <- decision_tree_model(uk_2, "UK")
train_data <- uk_tree_output$train_data
test_data <- uk_tree_output$test_data
prune_and_evaluate(train_data, test_data, "UK")

### Western Central Europe ###
wce_tree_output <- decision_tree_model(west_central_2, "Western Central Europe")
train_data <- wce_tree_output$train_data
test_data <- wce_tree_output$test_data
prune_and_evaluate(train_data, test_data, "Western Central Europe")



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





