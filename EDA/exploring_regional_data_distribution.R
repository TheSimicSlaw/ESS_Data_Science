# Cleaned & Modular Version: Value Distributions of Predictors Across Regions

library(ggplot2)
library(dplyr)
library(tidyr)

# --- Helper Functions ---
add_region_label <- function(df, region_name) {
  df$Region <- region_name
  return(df)
}

factor_predictors <- function(df, vars) {
  for (v in vars) df[[v]] <- as.factor(df[[v]])
  return(df)
}

categorize_democracy <- function(df) {
  df$dem_imp <- ifelse(df$implvdm < 5, "Low",
                       ifelse(df$implvdm >= 7, "High", "Medium"))
  df$dem_imp <- as.factor(df$dem_imp)
  df$implvdm <- NULL
  return(df)
}

# --- Data Loading ---
paths <- list(
  Central = "~/Documents/Github/ESS_Data_Science/non-parametric/central_europe_clean_csv.csv",
  Eastern = "~/Documents/Github/ESS_Data_Science/non-parametric/eastern_europe_clean_csv.csv",
  Nordics = "~/Documents/Github/ESS_Data_Science/non-parametric/nordics_clean_csv.csv",
  Southern = "~/Documents/Github/ESS_Data_Science/non-parametric/southern_europe_clean_csv.csv",
  UK = "~/Documents/Github/ESS_Data_Science/non-parametric/uk_clean_csv.csv",
  "West Central" = "~/Documents/Github/ESS_Data_Science/non-parametric/wce_clean_csv.csv"
)

data_list <- lapply(names(paths), function(region) {
  df <- read.csv(paths[[region]])
  df <- add_region_label(df, region)
  return(df)
})

all_regions <- bind_rows(data_list)

# --- Predictors ---
predictors <- c("pplfair", "pplhlp", "ppltrst", 
                "trstprl", "trstlgl", "trstplc", 
                "trstplt", "trstprt", "trstsci")

# --- Plot 1: All Predictors Across Regions (Faceted) ---
long_data <- all_regions %>%
  pivot_longer(cols = all_of(predictors), names_to = "Predictor", values_to = "Value") %>%
  count(Predictor, Region, Value)

long_data$Value <- factor(long_data$Value, levels = as.character(0:10))

print(
  ggplot(long_data, aes(x = Value, y = n, fill = Region)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(label = n), position = position_dodge(width = 0.9),
              vjust = -0.3, size = 2.8) +
    facet_wrap(~ Predictor, scales = "free_y") +
    scale_x_discrete(drop = FALSE) +
    labs(title = "Value Distributions of Predictors Across Regions",
         x = "Value", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
)

# --- Plot 2: One Plot per Predictor Across Regions ---
for (var in predictors) {
  p <- long_data %>%
    filter(Predictor == var) %>%
    ggplot(aes(x = Value, y = n, fill = Region)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(label = n), position = position_dodge(width = 0.9),
              vjust = -0.3, size = 3) +
    facet_wrap(~ Region, ncol = 3, scales = "free_y") +
    scale_x_discrete(drop = FALSE, limits = as.character(0:10)) +
    labs(title = paste("Value Distribution of", var, "Across Regions"),
         x = "Value (0–10)", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  readline(prompt = paste("Press [Enter] to continue to next plot (", var, ")"))
}

# --- Plot 3: One Plot per Predictor by Democracy Importance ---
processed_data <- all_regions %>%
  filter(!is.na(implvdm)) %>%
  factor_predictors(predictors) %>%
  categorize_democracy()

long_data_demimp <- processed_data %>%
  pivot_longer(cols = all_of(predictors), names_to = "Predictor", values_to = "Value") %>%
  count(Predictor, Region, dem_imp)

for (var in predictors) {
  p <- long_data_demimp %>%
    filter(Predictor == var) %>%
    ggplot(aes(x = Region, y = n, fill = dem_imp)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    geom_text(aes(label = n), position = position_dodge(width = 0.8),
              vjust = -0.3, size = 3) +
    scale_fill_manual(values = c("Low" = "#2ca02c", "Medium" = "#1f77b4", "High" = "#d62728")) +
    labs(title = paste("Democracy Importance by Region for Predictor:", var),
         x = "Region", y = "Count",
         fill = "Democracy Importance") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  readline(prompt = paste("Press [Enter] to continue to next plot (", var, ")"))
}

