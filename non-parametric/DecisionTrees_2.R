# ================================
# Required Libraries
# ================================
library(tree)
library(caret)
library(dplyr)
library(ggplot2)



## Load datasets 
central <- read.csv("../datasets/central_europe_clean_csv.csv")
eastern <- read.csv("../datasets/eastern_europe_clean_csv.csv")
nordics <- read.csv("../datasets/nordics_clean_csv.csv")
southern <- read.csv("../datasets/southern_europe_clean_csv.csv")
uk <- read.csv("../datasets/uk_clean_csv.csv")
west_central <- read.csv("../datasets/wce_clean_csv.csv")

## Making Copies for Analysis
central_2 <- central
eastern_2 <- eastern
nordics_2 <- nordics
southern_2 <- southern
uk_2 <- uk
west_central_2 <- west_central


# ================================
# Preprocessing: Binning & Factor
# ================================
preprocess_data <- function(df) {
  df <- df %>%
    mutate(across(c(ppltrust, pplhlp, pplfair, trstprl, trstlgl,
                    trstplc, trstplt, trstprt, trstsci),
                  ~ cut(.x, breaks = c(-Inf, 4, 7, 10),
                        labels = c("Low", "Medium", "High")))) %>%
    mutate(dmcntov_cat = cut(dmcntov, breaks = c(-Inf, 4, 7, 10),
                             labels = c("Low", "Medium", "High"))) %>%
    select(ppltrust, pplhlp, pplfair, trstprl, trstlgl,
           trstplc, trstplt, trstprt, trstsci, dmcntov_cat) %>%
    na.omit()
  return(df)
}

# ================================
# Balance + Train-Test Split
# ================================
split_balanced <- function(data, ratio = 0.7, seed = 123) {
  set.seed(seed)
  min_class <- min(table(data$dmcntov_cat))
  balanced <- data %>%
    group_by(dmcntov_cat) %>%
    slice_sample(n = min_class) %>%
    ungroup()
  idx <- createDataPartition(balanced$dmcntov_cat, p = ratio, list = FALSE)
  list(train = balanced[idx, ], test = balanced[-idx, ])
}

# ================================
# Train + Prune + Evaluate Tree
# ================================
train_and_prune <- function(train_data, test_data, region = "Region") {
  formula <- dmcntov_cat ~ .
  model <- tree(formula, data = train_data)
  
  # Cross-validated pruning
  cv_result <- cv.tree(model, FUN = prune.misclass)
  best_size <- cv_result$size[which.min(cv_result$dev)]
  pruned <- prune.misclass(model, best = best_size)
  
  # Predictions
  pred <- predict(pruned, test_data, type = "class")
  acc <- mean(pred == test_data$dmcntov_cat)
  
  # Confusion matrix
  cat(paste("\n===", region, "===\n"))
  print(table(Predicted = pred, Actual = test_data$dmcntov_cat))
  cat(sprintf("Pruned Accuracy: %.2f%%\n", acc * 100))
  
  # Optional: plot tree
  plot(pruned)
  text(pruned, pretty = 0)
  title(paste("Pruned Decision Tree -", region))
  
  return(list(model = pruned, accuracy = acc))
}

# ================================
# Run Full Pipeline on Region
# ================================
run_pipeline <- function(df, region_name) {
  data <- preprocess_data(df)
  split <- split_balanced(data)
  train_and_prune(split$train, split$test, region_name)
}

central_results <- run_pipeline(central, "Central Europe")
eastern_results <- run_pipeline(eastern, "Eastern Europe")
nordic_results <- run_pipeline(nordics, "Nordics")
southern_results <- run_pipeline(southern, "Southern Europe")
uk_results <- run_pipeline(uk, "British Isles")
wce_results <- run_pipeline(west_central, "Western Continental Europe")

