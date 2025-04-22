setwd("~/Documents/Spring 2025/MATH 4322 - Data Science /Group Project/Cleaned Datasets")
install.packages("MASS")
library(MASS)

## Delete your art 
rm(list = ls())

## Loading datasets

central <- read.csv("central_europe_clean_csv.csv", header = TRUE)
eastern <- read.csv("eastern_europe_clean_csv.csv", header = TRUE)
nordics <- read.csv("nordics_clean_csv.csv", header = TRUE)
southern <- read.csv("southern_europe_clean_csv.csv", header = TRUE)
uk <- read.csv("uk_clean_csv.csv", header = TRUE)
west_central <- read.csv("wce_clean_csv.csv", header = TRUE)

## Making Copies for Analysis

central_2 <- central
eastern_2 <- eastern
nordics_2 <- nordics
southern_2 <- southern
uk_2 <- uk
west_central_2 <- west_central

## Making scalers into factors
central_2$pplfair <- as.factor(central_2$pplfair)
eastern_2$pplfair <- as.factor(eastern_2$pplfair)
nordics_2$pplfair <- as.factor(nordics_2$pplfair)
southern_2$pplfair <- as.factor(southern_2$pplfair)
uk_2$pplfair <- as.factor(uk_2$pplfair)
west_central_2$pplfair <- as.factor(west_central_2$pplfair)

central_2$pplhlp <- as.factor(central_2$pplhlp)
eastern_2$pplhlp <- as.factor(eastern_2$pplhlp)
nordics_2$pplhlp <- as.factor(nordics_2$pplhlp)
southern_2$pplhlp <- as.factor(southern_2$pplhlp)
uk_2$pplhlp <- as.factor(uk_2$pplhlp)
west_central_2$pplhlp <- as.factor(west_central_2$pplhlp)

central_2$ppltrst <- as.factor(central_2$ppltrst)
eastern_2$ppltrst <- as.factor(eastern_2$ppltrst)
nordics_2$ppltrst <- as.factor(nordics_2$ppltrst)
southern_2$ppltrst <- as.factor(southern_2$ppltrst)
uk_2$ppltrst <- as.factor(uk_2$ppltrst)
west_central_2$ppltrst <- as.factor(west_central_2$ppltrst)

central_2$trstlgl <- as.factor(central_2$trstlgl)
eastern_2$trstlgl <- as.factor(eastern_2$trstlgl)
nordics_2$trstlgl <- as.factor(nordics_2$trstlgl)
southern_2$trstlgl <- as.factor(southern_2$trstlgl)
uk_2$trstlgl <- as.factor(uk_2$trstlgl)
west_central_2$trstlgl <- as.factor(west_central_2$trstlgl)

central_2$trstplc <- as.factor(central_2$trstplc)
eastern_2$trstplc <- as.factor(eastern_2$trstplc)
nordics_2$trstplc <- as.factor(nordics_2$trstplc)
southern_2$trstplc <- as.factor(southern_2$trstplc)
uk_2$trstplc <- as.factor(uk_2$trstplc)
west_central_2$trstplc <- as.factor(west_central_2$trstplc)

central_2$trstplt <- as.factor(central_2$trstplt)
eastern_2$trstplt <- as.factor(eastern_2$trstplt)
nordics_2$trstplt <- as.factor(nordics_2$trstplt)
southern_2$trstplt <- as.factor(southern_2$trstplt)
uk_2$trstplt <- as.factor(uk_2$trstplt)
west_central_2$trstplt <- as.factor(west_central_2$trstplt)

central_2$trstprt <- as.factor(central_2$trstprt)
eastern_2$trstprt <- as.factor(eastern_2$trstprt)
nordics_2$trstprt <- as.factor(nordics_2$trstprt)
southern_2$trstprt <- as.factor(southern_2$trstprt)
uk_2$trstprt <- as.factor(uk_2$trstprt)
west_central_2$trstprt <- as.factor(west_central_2$trstprt)

central_2$trstsci <- as.factor(central_2$trstsci)
eastern_2$trstsci <- as.factor(eastern_2$trstsci)
nordics_2$trstsci <- as.factor(nordics_2$trstsci)
southern_2$trstsci <- as.factor(southern_2$trstsci)
uk_2$trstsci <- as.factor(uk_2$trstsci)
west_central_2$trstsci <- as.factor(west_central_2$trstsci)

## Low, Medium, High Partition for implvdm

central_2$dem_imp <- ifelse(central_2$implvdm < 5, "Low", 
                           ifelse(central_2$implvdm >= 7, "High", "Medium"))
eastern_2$dem_imp <- ifelse(eastern_2$implvdm < 5, "Low", 
                            ifelse(eastern_2$implvdm >= 7, "High", "Medium"))
nordics_2$dem_imp <- ifelse(nordics_2$implvdm < 5, "Low", 
                            ifelse(nordics_2$implvdm >= 7, "High", "Medium"))
southern_2$dem_imp <- ifelse(southern_2$implvdm < 5, "Low", 
                            ifelse(southern_2$implvdm >= 7, "High", "Medium"))
uk_2$dem_imp <- ifelse(uk_2$implvdm < 5, "Low", 
                            ifelse(uk_2$implvdm >= 7, "High", "Medium"))
west_central_2$dem_imp <- ifelse(west_central_2$implvdm < 5, "Low", 
                            ifelse(west_central_2$implvdm >= 7, "High", "Medium"))

## Remove implvdm and country as factors
central_2$implvdm <- NULL
eastern_2$implvdm <- NULL
nordics_2$implvdm <- NULL
southern_2$implvdm <- NULL
uk_2$implvdm <- NULL
west_central_2$implvdm <- NULL

central_2$cntry <- NULL
eastern_2$cntry <- NULL
nordics_2$cntry <- NULL
southern_2$cntry <- NULL
uk_2$cntry <- NULL
west_central_2$cntry <- NULL

## Split Into Testing and Training Data
central_2_index <- sample(1:nrow(central_2), size = 0.7 * nrow(central_2))
central_2_train <- central_2[central_2_index, ]
central_2_test <- central_2[-central_2_index, ]

eastern_2_index <- sample(1:nrow(eastern_2), size = 0.7 * nrow(eastern_2))
eastern_2_train <- eastern_2[eastern_2_index, ]
eastern_2_test <- eastern_2[-eastern_2_index, ]

nordics_2_index <- sample(1:nrow(nordics_2), size = 0.7 * nrow(nordics_2))
nordics_2_train <- nordics_2[nordics_2_index, ]
nordics_2_test <- nordics_2[-nordics_2_index, ]

southern_2_index <- sample(1:nrow(southern_2), size = 0.7 * nrow(southern_2))
southern_2_train <- southern_2[southern_2_index, ]
southern_2_test <- southern_2[-southern_2_index, ]

uk_2_index <- sample(1:nrow(uk_2), size = 0.7 * nrow(uk_2))
uk_2_train <- uk_2[uk_2_index, ]
uk_2_test <- uk_2[-uk_2_index, ]

west_central_2_index <- sample(1:nrow(west_central_2), size = 0.7 * nrow(west_central_2))
west_central_2_train <- west_central_2[west_central_2_index, ]
west_central_2_test <- west_central_2[-west_central_2_index, ]

## Linear Discriminant Analysis

central.lda <- lda(dem_imp ~ . ,data = central_2_train)
eastern.lda <- lda(dem_imp ~ . ,data = eastern_2_train)
nordics.lda <- lda(dem_imp ~ . ,data = nordics_2_train)
southern.lda <- lda(dem_imp ~ ., data = southern_2_train)
uk.lda <- lda(dem_imp ~ . ,data = uk_2_train)
west_central.lda <- lda(dem_imp ~ . ,data = west_central_2_train)

central.lda.pred = predict(central.lda,central_2_test)
eastern.lda.pred = predict(eastern.lda,eastern_2_test)
nordics.lda.pred = predict(nordics.lda,nordics_2_test)
southern.lda.pred = predict(southern.lda,southern_2_test)
uk.lda.pred = predict(uk.lda,uk_2_test)
west_central.lda.pred = predict(west_central.lda,west_central_2_test)

## Confusion Matrices
central_confusion <- table(central.lda.pred$class,central_2_test$dem_imp)
eastern_confusion <- table(eastern.lda.pred$class,eastern_2_test$dem_imp)
nordics_confusion <- table(nordics.lda.pred$class,nordics_2_test$dem_imp)
southern_confusion <- table(southern.lda.pred$class,southern_2_test$dem_imp)
uk_confusion <- table(uk.lda.pred$class,uk_2_test$dem_imp)
west_central_confusion <- table(west_central.lda.pred$class,west_central_2_test$dem_imp)

print(central_confusion)
print(eastern_confusion)
print(nordics_confusion)
print(southern_confusion)
print(uk_confusion)
print(west_central_confusion)

## Accuracy Rate

correct_central <- sum(diag(central_confusion))
total_central <- sum(central_confusion)
accuracy_central <- correct_central / total_central

correct_eastern <- sum(diag(eastern_confusion))
total_eastern <- sum(eastern_confusion)
accuracy_eastern <- correct_eastern / total_eastern

correct_nordics <- sum(diag(nordics_confusion))
total_nordics <- sum(nordics_confusion)
accuracy_nordics <- correct_nordics / total_nordics

correct_southern <- sum(diag(southern_confusion))
total_southern <- sum(southern_confusion)
accuracy_southern <- correct_southern / total_southern

correct_uk <- sum(diag(uk_confusion))
total_uk <- sum(uk_confusion)
accuracy_uk <- correct_uk / total_uk

correct_west_central <- sum(diag(west_central_confusion))
total_west_central <- sum(west_central_confusion)
accuracy_west_central <- correct_west_central / total_west_central

accuracies <- c(Central = accuracy_central, Eastern = accuracy_eastern, Nordics = accuracy_nordics, Southern = accuracy_southern, 
                UK = accuracy_uk, Western_Continental = accuracy_west_central)
print(round(accuracies, 4))

## Relative importance of variables LD1

central_importance <- abs(central.lda$scaling[, 1])
central_importance_sorted <- sort(central_importance, decreasing = TRUE)
print(central_importance_sorted)

eastern_importance <- abs(eastern.lda$scaling[, 1])
eastern_importance_sorted <- sort(eastern_importance, decreasing = TRUE)
print(eastern_importance_sorted)

nordics_importance <- abs(nordics.lda$scaling[, 1])
nordics_importance_sorted <- sort(nordics_importance, decreasing = TRUE)
print(nordics_importance_sorted)

southern_importance <- abs(southern.lda$scaling[, 1])
southern_importance_sorted <- sort(southern_importance, decreasing = TRUE)
print(southern_importance_sorted)

uk_importance <- abs(uk.lda$scaling[, 1])
uk_importance_sorted <- sort(uk_importance, decreasing = TRUE)
print(uk_importance_sorted)

west_central_importance <- abs(west_central.lda$scaling[, 1])
west_central_importance_sorted <- sort(west_central_importance, decreasing = TRUE)
print(west_central_importance_sorted)

## Graphing of relative importance of variables by LD1

par(mfrow = c(2, 3), mar = c(6, 4, 4, 2)) 

barplot(central_importance_sorted, las = 2, col = "steelblue", main = "Central", ylab = "Abs Coefficient")
barplot(eastern_importance_sorted, las = 2, col = "darkorange", main = "Eastern", ylab = "Abs Coefficient")
barplot(nordics_importance_sorted, las = 2, col = "seagreen", main = "Nordics", ylab = "Abs Coefficient")
barplot(southern_importance_sorted, las = 2, col = "tomato", main = "Southern", ylab = "Abs Coefficient")
barplot(uk_importance_sorted, las = 2, col = "darkviolet", main = "UK", ylab = "Abs Coefficient")
barplot(west_central_importance_sorted, las = 2, col = "goldenrod", main = "Western Continental", ylab = "Abs Coefficient")

par(mfrow = c(1, 1))

## K-Fold Cross Validation Function (Scrappy_Jet)
kfold_lda <- function(data, target_col, k = 5) {
  
  set.seed(123)  ## For reproducibility
  folds <- sample(rep(1:k, length.out = nrow(data)))
  accuracy_list <- c()
  
  for (i in 1:k) {
    train_data <- data[folds != i, ]
    test_data <- data[folds == i, ]
    
    model <- lda(as.formula(paste(target_col, "~ .")), data = train_data)
    pred <- predict(model, test_data)
    
    actual <- test_data[[target_col]]
    predicted <- pred$class
    
    acc <- mean(actual == predicted)
    accuracy_list <- c(accuracy_list, acc)
  }
  
  avg_accuracy <- mean(accuracy_list)
  return(list(accuracies = accuracy_list, average_accuracy = avg_accuracy))
}

## Calling K-Fold
central_cv_results <- kfold_lda(central_2, "dem_imp", k = 5)
eastern_cv_results <- kfold_lda(eastern_2, "dem_imp", k = 5)
nordics_cv_results <- kfold_lda(nordics_2, "dem_imp", k = 5)
southern_cv_results <- kfold_lda(southern_2, "dem_imp", k = 5)
uk_cv_results <- kfold_lda(uk_2, "dem_imp", k = 5)
west_central_cv_results <- kfold_lda(west_central_2, "dem_imp", k = 5)

## Printing K-Fold Results
print(central_cv_results)
print(eastern_cv_results)
print(nordics_cv_results)
print(southern_cv_results)
print(uk_cv_results)
print(west_central_cv_results)