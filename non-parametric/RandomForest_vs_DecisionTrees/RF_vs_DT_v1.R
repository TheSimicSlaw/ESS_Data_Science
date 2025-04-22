# load data
# Libraries
library(tree)
library(caret)
library(dplyr)
library(ggplot2)
library(tidyr)
library(randomForest)

# Load datasets
datasets <- list(
  "Central Europe" = central_europe_clean_csv,
  "Eastern Europe" = eastern_europe_clean_csv,
  "Nordics" = nordics_clean_csv,
  "Southern Europe" = southern_europe_clean_csv,
  "UK" = uk_clean_csv,
  "Western Central Europe" = wce_clean_csv
)

# Factor conversion
factoring <- function(df) {
  vars <- c("pplfair", "pplhlp", "ppltrst", "trstprl", "trstlgl",
            "trstplc", "trstplt", "trstprt", "trstsci")
  for (v in vars) {
    df[[v]] <- as.factor(df[[v]])
  }
  return(df)
}

# Categorize democracy
categorize_democracy <- function(df) {
  df$dem_imp <- ifelse(df$implvdm < 5, "Low",
                       ifelse(df$implvdm >= 7, "High", "Medium"))
  df$dem_imp <- as.factor(df$dem_imp)
  df$implvdm <- NULL
  return(df)
}

# Apply preprocessing to all datasets
processed_datasets <- lapply(datasets, function(df) {
  df <- factoring(df)
  df <- categorize_democracy(df)
  return(df)
})


# --------    Decision Tree   --------
get_dt_importance <- function(model) {
  var_freq <- model$frame$var
  var_freq <- var_freq[var_freq != "<leaf>"]
  as.data.frame(table(var_freq))
}

decision_tree_model <- function(data, region_name = "Region") {
  set.seed(1)
  data <- na.omit(data)
  data$dem_imp <- as.factor(data$dem_imp)
  
  min_n <- min(table(data$dem_imp))
  data_bal <- data %>%
    group_by(dem_imp) %>%
    slice_sample(n = min_n) %>%
    ungroup()
  
  train_idx <- createDataPartition(data_bal$dem_imp, p = 0.7, list = FALSE)
  train_data <- data_bal[train_idx, ]
  test_data <- data_bal[-train_idx, ]
  
  tree_model <- tree(dem_imp ~ ., data = train_data)
  
  predictions <- predict(tree_model, test_data, type = "class")
  conf_matrix <- table(Predicted = predictions, Actual = test_data$dem_imp)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  importance <- get_dt_importance(tree_model)
  names(importance) <- c("Feature", "Count")
  importance$Region <- region_name
  
  return(list(
    model = tree_model,
    confusion_matrix = conf_matrix,
    accuracy = accuracy,
    importance = importance
  ))
}

# ----------    Random Forest   -----------
run_random_forest <- function(data, region_name = "Region") {
  set.seed(1)
  data <- na.omit(data)
  data$dem_imp <- as.factor(data$dem_imp)
  
  min_n <- min(table(data$dem_imp))
  data_bal <- data %>%
    group_by(dem_imp) %>%
    slice_sample(n = min_n) %>%
    ungroup()
  
  train_idx <- createDataPartition(data_bal$dem_imp, p = 0.7, list = FALSE)
  train_data <- data_bal[train_idx, ]
  test_data <- data_bal[-train_idx, ]
  
  rf_model <- randomForest(dem_imp ~ ., data = train_data, importance = TRUE)
  predictions <- predict(rf_model, test_data)
  conf_matrix <- table(Predicted = predictions, Actual = test_data$dem_imp)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  importance_df <- as.data.frame(importance(rf_model))
  importance_df$Feature <- rownames(importance_df)
  importance_df$Region <- region_name
  
  return(list(
    model = rf_model,
    confusion_matrix = conf_matrix,
    accuracy = accuracy,
    importance = importance_df
  ))
}

# --------  Run Models across all regions   ---------
# Run all models
dt_results <- mapply(decision_tree_model, processed_datasets, names(processed_datasets), SIMPLIFY = FALSE)
rf_results <- mapply(run_random_forest, processed_datasets, names(processed_datasets), SIMPLIFY = FALSE)


# ------    Summary Plots   -----------
accuracy_df <- data.frame(
  Region = names(dt_results),
  DecisionTree = sapply(dt_results, function(x) round(x$accuracy * 100, 2)),
  RandomForest = sapply(rf_results, function(x) round(x$accuracy * 100, 2))
)

accuracy_long <- pivot_longer(accuracy_df, cols = -Region, names_to = "Model", values_to = "Accuracy")

ggplot(accuracy_long, aes(x = Region, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model Accuracy by Region", y = "Accuracy (%)", x = "Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("accuracy_comparison.png", width = 10, height = 6)


# ----------  Feature Importance  ---------------------

# ------------ FI for RF ------------------
rf_importance_all <- do.call(rbind, lapply(rf_results, `[[`, "importance"))

ggplot(rf_importance_all, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini, fill = Feature)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~Region, scales = "free_y") +
  labs(title = "Random Forest Feature Importance", y = "Mean Decrease in Gini") +
  theme_minimal()

ggsave("rf_feature_importance_all_regions.png", width = 12, height = 8)


# ----------- FI for Decision Trees -------------
dt_importance_all <- do.call(rbind, lapply(dt_results, `[[`, "importance"))

ggplot(dt_importance_all, aes(x = reorder(Feature, Count), y = Count, fill = Feature)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~Region, scales = "free_y") +
  labs(title = "Decision Tree Feature Split Frequency", y = "Split Count") +
  theme_minimal()

ggsave("dt_feature_importance_all_regions.png", width = 12, height = 8)


