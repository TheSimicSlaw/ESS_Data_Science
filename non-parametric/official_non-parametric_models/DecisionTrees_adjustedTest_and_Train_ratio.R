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
  
  return(invisible(list(confusion_matrix = conf_matrix, accuracy = accuracy)))
}


# ------- manual --------
# 70/30 split
decision_tree_model(central_2, "Central Europe", split_ratio = 0.7)

# 80/20 split
decision_tree_model(central_2, "Central Europe", split_ratio = 0.8)


decision_tree_model(central_2, "Eastern Europe", split_ratio = 0.7)


# -----------------------

# these are for 70 30 split 
region_list <- list(
  "Central Europe" = central_2,
  "Eastern Europe" = eastern_2,
  "Nordics" = nordics_2,
  "Southern Europe" = southern_2,
  "UK" = uk_2,
  "Western Central Europe" = west_central_2
)

# Then apply the function for each region:
for (region_name in names(region_list)) {
  decision_tree_model(region_list[[region_name]], region_name, split_ratio = 0.7)
}

# -----------------------

# these are for 80 20 split 
region_list <- list(
  "Central Europe" = central_2,
  "Eastern Europe" = eastern_2,
  "Nordics" = nordics_2,
  "Southern Europe" = southern_2,
  "UK" = uk_2,
  "Western Central Europe" = west_central_2
)

# Then apply the function for each region:
for (region_name in names(region_list)) {
  decision_tree_model(region_list[[region_name]], region_name, split_ratio = 0.8)
}



