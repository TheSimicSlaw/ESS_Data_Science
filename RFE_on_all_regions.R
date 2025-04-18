# ================================
#  Required Libraries
# ================================
library(tree)
library(caret)
library(dplyr)
library(randomForest)
library(doParallel)  # For RFE parallelism


# ================================
# Load Datasets
# ================================
central <- read.csv("central_europe_clean_csv.csv")
eastern <- read.csv("eastern_europe_clean_csv.csv")
nordics <- read.csv("nordics_clean_csv.csv")
southern <- read.csv("southern_europe_clean_csv.csv")
uk <- read.csv("uk_clean_csv.csv")
west_central <- read.csv("wce_clean_csv.csv")


# ================================
# Preprocessing Functions
# ================================
factoring <- function(df) {
  vars <- c("pplfair", "pplhlp", "ppltrst", "trstprl", "trstlgl",
            "trstplc", "trstplt", "trstprt", "trstsci")
  for (v in vars) {
    df[[v]] <- as.factor(df[[v]])
  }
  return(df)
}

categorize_democracy <- function(df) {
  df$dem_imp <- ifelse(df$implvdm < 5, "Low",
                       ifelse(df$implvdm >= 7, "High", "Medium"))
  df$dem_imp <- as.factor(df$dem_imp)
  return(df)
}


# ================================
# Apply Preprocessing
# ================================
central_2 <- categorize_democracy(factoring(central))
eastern_2 <- categorize_democracy(factoring(eastern))
nordics_2 <- categorize_democracy(factoring(nordics))
southern_2 <- categorize_democracy(factoring(southern))
uk_2 <- categorize_democracy(factoring(uk))
west_central_2 <- categorize_democracy(factoring(west_central))


# ================================
# Balance + Split Function
# ================================
prepare_data_split <- function(data, split_ratio = 0.7, seed = 1) {
  set.seed(seed)
  min_n <- min(table(data$dem_imp))
  data_bal <- data %>%
    group_by(dem_imp) %>%
    slice_sample(n = min_n) %>%
    ungroup()
  idx <- createDataPartition(data_bal$dem_imp, p = split_ratio, list = FALSE)
  train_data <- data_bal[idx, ]
  test_data <- data_bal[-idx, ]
  return(list(train = train_data, test = test_data))
}


# ================================
# Train Decision Tree
# ================================
decision_tree_model <- function(train_data, test_data, region_name = "Region") {
  tree_model <- tree(dem_imp ~ ppltrst + pplhlp + pplfair + trstprl + trstlgl +
                       trstplc + trstplt + trstprt + trstsci,
                     data = train_data)
  
  predictions <- predict(tree_model, test_data, type = "class")
  conf_matrix <- table(Predicted = predictions, Actual = test_data$dem_imp)
  accuracy <- mean(predictions == test_data$dem_imp)
  
  cat("\n=== Confusion Matrix for", region_name, "===\n")
  print(conf_matrix)
  cat(sprintf("\nAccuracy for %s: %.2f%%\n", region_name, accuracy * 100))
  
  cat("\nDecision Tree for", region_name, ":\n")
  plot(tree_model)
  text(tree_model, pretty = 0)
  
  return(tree_model)
}


# ================================
# ✂️ Tree Pruning Function
# ================================
get_prune_chart <- function(tree_model, train_data_input, test_data, region_name = "Region") {
  assign("train_data", train_data_input, envir = .GlobalEnv)
  prune_results <- cv.tree(tree_model, FUN = prune.misclass)
  rm(train_data, envir = .GlobalEnv)
  
  par(mfrow = c(1, 2))
  plot(prune_results$size, prune_results$dev, type = "b", main = "Tree Size vs Deviance")
  plot(prune_results$k, prune_results$dev, type = "b", main = "Complexity Param vs Deviance")
  
  best_size <- prune_results$size[which.min(prune_results$dev)]
  pruned_tree <- prune.misclass(tree_model, best = best_size)
  
  pruned_preds <- predict(pruned_tree, test_data, type = "class")
  conf_matrix <- table(Predicted = pruned_preds, Actual = test_data$dem_imp)
  accuracy <- mean(pruned_preds == test_data$dem_imp)
  
  cat("\n=== PRUNED Confusion Matrix for", region_name, "===\n")
  print(conf_matrix)
  cat(sprintf("\nPruned Accuracy for %s: %.2f%%\n", region_name, accuracy * 100))
  cat("\nPruned Tree for", region_name, ":\n")
  plot(pruned_tree)
  text(pruned_tree, pretty = 0)
  
  return(pruned_tree)
}


# ================================
#  Run Pipeline on Region
# ================================
run_pipeline <- function(region_data, region_name) {
  split <- prepare_data_split(region_data)
  tree <- decision_tree_model(split$train, split$test, region_name)
  pruned <- get_prune_chart(tree, split$train, split$test, region_name)
  return(list(tree = tree, pruned = pruned))
}


# ================================
# Execute All Regions
# ================================
central_results <- run_pipeline(central_2, "Central Europe")
eastern_results <- run_pipeline(eastern_2, "Eastern Europe")
nordics_results <- run_pipeline(nordics_2, "Nordics")
southern_results <- run_pipeline(southern_2, "Southern Europe")
uk_results <- run_pipeline(uk_2, "United Kingdom")
west_central_results <- run_pipeline(west_central_2, "Western Central Europe")


# ================================
# caret::rfe() Feature Selection
# Combined Plot Saving
# ================================

run_rfe_and_tree_combined_plot <- function(region_df, region_name, output_dir = "/Users/jacquelinesanchez/Documents/Github/ESS_Data_Science/rfe_outputs") {
  # --- Load Libraries ---
  suppressPackageStartupMessages({
    library(doParallel)
    library(caret)
    library(tree)
    library(dplyr)
    library(grid)
    library(gridExtra)
    library(ggplot2)
    library(png)
  })
  
  # --- Safe Output Name ---
  safe_name <- gsub(" ", "_", region_name)
  
  # --- Preprocessing ---
  region_clean <- region_df %>%
    mutate(dem_imp = factor(ifelse(implvdm < 5, "Low",
                                   ifelse(implvdm >= 7, "High", "Medium")))) %>%
    select(ppltrst, pplhlp, pplfair, trstprl, trstlgl,
           trstplc, trstplt, trstprt, trstsci, dem_imp) %>%
    na.omit()
  
  x <- region_clean[, -ncol(region_clean)]
  y <- region_clean$dem_imp
  
  # --- RFE ---
  cl <- makePSOCKcluster(parallel::detectCores() - 1)
  registerDoParallel(cl)
  
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 3, allowParallel = TRUE)
  result <- rfe(x, y, sizes = c(3, 5, 7), rfeControl = control)
  
  stopCluster(cl)
  
  # --- Create Output Directory ---
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # --- Create ggplot RFE Plot ---
  rfe_df <- data.frame(Variables = result$results$Variables,
                       Accuracy = result$results$Accuracy)
  
  rfe_plot <- ggplot(rfe_df, aes(x = Variables, y = Accuracy)) +
    geom_point() +
    geom_line() +
    ggtitle(paste("RFE Accuracy -", region_name)) +
    theme_minimal()
  
  rfe_file <- file.path(output_dir, paste0(safe_name, "_rfe_plot.png"))
  ggsave(rfe_file, rfe_plot, width = 8, height = 6, dpi = 300)
  
  # --- Train Decision Tree ---
  selected_vars <- predictors(result)
  formula_text <- paste("dem_imp ~", paste(selected_vars, collapse = " + "))
  formula <- as.formula(formula_text)
  tree_model <- tree(formula, data = region_clean)
  
  tree_file <- file.path(output_dir, paste0(safe_name, "_tree_plot.png"))
  png(tree_file, width = 800, height = 600)
  plot(tree_model, main = paste("Decision Tree -", region_name))
  text(tree_model, pretty = 0)
  dev.off()
  
  # --- Combine Plots ---
  combined_file <- file.path(output_dir, paste0(safe_name, "_combined.png"))
  rfe_img <- png::readPNG(rfe_file)
  tree_img <- png::readPNG(tree_file)
  
  png(combined_file, width = 1600, height = 700)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  grid.raster(rfe_img, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid.raster(tree_img, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
  dev.off()
  
  cat(sprintf("Combined plot saved to: %s\n", combined_file))
  return(list(rfe = result, tree = tree_model))
}




install.packages("png")


central_results <- run_rfe_and_tree_combined_plot(central, "Central Europe")


eastern_results <- run_rfe_and_tree_combined_plot(eastern, "Eastern Europe")


nordics_results <- run_rfe_and_tree_combined_plot(nordics, "Nordics")


southern_results <- run_rfe_and_tree_combined_plot(southern, "Southern Europe")

uk_results <- run_rfe_and_tree_combined_plot(uk, "United Kingdom")

west_central_results <- run_rfe_and_tree(west_central, "Western Central Europe")


# inspect the tree structures:
summary(central_results$tree)
summary(west_central_results$tree)
