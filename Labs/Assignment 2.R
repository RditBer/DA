# ============================================
# Data Analytics Assignment 2
# Student: Ardit Berisha
# Date: 2/12/2026
# ============================================

# Load libraries
library(readr)
library(ggplot2)
library(class)

# Load dataset
epi_results_2024_pop_gdp <- read_csv("C:/Users/Diti/Desktop/Data Analytics/Assignment 2/epi_results_2024_pop_gdp.csv")
dataset <- epi_results_2024_pop_gdp

# Choose variable for analysis
chosen_var <- "AIR.new"

# ============================================
# TASK 1: Variable Distributions
# ============================================

# 1.1: Histogram with density overlay
ggplot(dataset, aes(x = .data[[chosen_var]])) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "dodgerblue1", color = "royalblue4") +
  geom_density(color = "midnightblue", linewidth = 1.5) +
  labs(title = paste("Distribution of", chosen_var),
       x = chosen_var,
       y = "Density") +
  theme_minimal()

# 1.2: Boxplots by region
ggplot(dataset, aes(x = region, y = .data[[chosen_var]])) +
  geom_boxplot(fill = "dodgerblue1", color = "royalblue4", alpha = 0.8) +
  labs(title = paste("Distribution of", chosen_var, "by Region"),
       x = "Region",
       y = chosen_var) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================
# TASK 2: Regional Subsets and QQ Plot
# ============================================

# Create subsets for two regions
region1 <- "Eastern Europe"
region2 <- "Former Soviet States"

subset1 <- dataset[dataset$region == region1, ]
subset2 <- dataset[dataset$region == region2, ]

# 2.1: Histograms for each region
ggplot(subset1, aes(x = .data[[chosen_var]])) +
  geom_histogram(bins = 15, fill = "dodgerblue1", color = "royalblue4") +
  labs(title = paste(chosen_var, "in", region1),
       x = chosen_var,
       y = "Frequency") +
  theme_minimal()

ggplot(subset2, aes(x = .data[[chosen_var]])) +
  geom_histogram(bins = 15, fill = "springgreen3", color = "chartreuse4") +
  labs(title = paste(chosen_var, "in", region2),
       x = chosen_var,
       y = "Frequency") +
  theme_minimal()

# 2.2: QQ plot
qqplot(subset1[[chosen_var]], subset2[[chosen_var]],
       main = paste("QQ Plot:", region1, "vs", region2),
       xlab = paste(chosen_var, "-", region1),
       ylab = paste(chosen_var, "-", region2),
       pch = 19, col = "dodgerblue1", cex = 1.2)
abline(0, 1, col = "royalblue4", lwd = 2)

# ============================================
# TASK 3: Linear Models
# ============================================

# Clean data (remove NAs)
subset1_clean <- subset1[!is.na(subset1$population) & !is.na(subset1$gdp) & !is.na(subset1[[chosen_var]]), ]
subset2_clean <- subset2[!is.na(subset2$population) & !is.na(subset2$gdp) & !is.na(subset2[[chosen_var]]), ]

# --- Eastern Europe ---

# 3.1: Scatter plots with best fit lines
ggplot(subset1_clean, aes(x = log(population), y = .data[[chosen_var]])) +
  geom_point(size = 3, color = "dodgerblue1") +
  geom_smooth(method = "lm", se = FALSE, color = "royalblue4", linewidth = 1.2) +
  labs(title = paste(region1, ":", chosen_var, "vs log(Population)"),
       x = "log(Population)",
       y = chosen_var) +
  theme_minimal()

ggplot(subset1_clean, aes(x = gdp, y = .data[[chosen_var]])) +
  geom_point(size = 3, color = "dodgerblue1") +
  geom_smooth(method = "lm", se = FALSE, color = "royalblue4", linewidth = 1.2) +
  labs(title = paste(region1, ":", chosen_var, "vs GDP"),
       x = "GDP per capita",
       y = chosen_var) +
  theme_minimal()

# 3.2: Fit linear models
model1_subset1 <- lm(AIR.new ~ log(population), data = subset1_clean)
summary(model1_subset1)

model2_subset1 <- lm(AIR.new ~ gdp, data = subset1_clean)
summary(model2_subset1)

# Plot residuals
plot(model1_subset1$fitted.values, model1_subset1$residuals,
     main = paste(region1, "- Model 1 Residuals"),
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 19, col = "dodgerblue1", cex = 1.2)
abline(h = 0, col = "royalblue4", lwd = 2)

plot(model2_subset1$fitted.values, model2_subset1$residuals,
     main = paste(region1, "- Model 2 Residuals"),
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 19, col = "dodgerblue1", cex = 1.2)
abline(h = 0, col = "royalblue4", lwd = 2)

# --- Former Soviet States ---

# 3.1: Scatter plots
ggplot(subset2_clean, aes(x = log(population), y = .data[[chosen_var]])) +
  geom_point(size = 3, color = "springgreen3") +
  geom_smooth(method = "lm", se = FALSE, color = "chartreuse4", linewidth = 1.2) +
  labs(title = paste(region2, ":", chosen_var, "vs log(Population)"),
       x = "log(Population)",
       y = chosen_var) +
  theme_minimal()

ggplot(subset2_clean, aes(x = gdp, y = .data[[chosen_var]])) +
  geom_point(size = 3, color = "springgreen3") +
  geom_smooth(method = "lm", se = FALSE, color = "chartreuse4", linewidth = 1.2) +
  labs(title = paste(region2, ":", chosen_var, "vs GDP"),
       x = "GDP per capita",
       y = chosen_var) +
  theme_minimal()

# 3.2: Fit linear models
model1_subset2 <- lm(AIR.new ~ log(population), data = subset2_clean)
summary(model1_subset2)

model2_subset2 <- lm(AIR.new ~ gdp, data = subset2_clean)
summary(model2_subset2)

# Plot residuals
plot(model1_subset2$fitted.values, model1_subset2$residuals,
     main = paste(region2, "- Model 1 Residuals"),
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 19, col = "springgreen3", cex = 1.2)
abline(h = 0, col = "chartreuse4", lwd = 2)

plot(model2_subset2$fitted.values, model2_subset2$residuals,
     main = paste(region2, "- Model 2 Residuals"),
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 19, col = "springgreen3", cex = 1.2)
abline(h = 0, col = "chartreuse4", lwd = 2)

# ============================================
# TASK 4: kNN Classification
# ============================================

# Create subset with 2 regions
knn_subset <- dataset[dataset$region %in% c("Eastern Europe", "Former Soviet States"), ]
knn_data <- knn_subset[!is.na(knn_subset$population) & 
                         !is.na(knn_subset$gdp) & 
                         !is.na(knn_subset[[chosen_var]]), ]

# 4.1: Train kNN model with AIR.new

# Prepare features with transformations
features <- data.frame(
  log_population = log(knn_data$population),
  gdp = knn_data$gdp,
  air = knn_data[[chosen_var]]
)

labels <- as.factor(knn_data$region)

# Split data
set.seed(123)
train_indices <- sample(1:nrow(features), size = 0.7 * nrow(features))
train_features <- features[train_indices, ]
test_features <- features[-train_indices, ]
train_labels <- labels[train_indices]
test_labels <- labels[-train_indices]

# Try different k values
k_values <- c(1, 3, 5, 7)
for(k in k_values) {
  knn_pred <- knn(train = train_features, test = test_features, 
                  cl = train_labels, k = k)
  accuracy <- sum(knn_pred == test_labels) / length(test_labels)
  cat("k =", k, "| Accuracy =", round(accuracy, 4), "\n")
}

# Best model (k=1)
best_k <- 1
knn_pred_best <- knn(train = train_features, test = test_features, 
                     cl = train_labels, k = best_k)

# Confusion matrix
confusion_matrix <- table(Predicted = knn_pred_best, Actual = test_labels)
print(confusion_matrix)
cat("Accuracy:", sum(knn_pred_best == test_labels) / length(test_labels), "\n")

# 4.2: Train model with EPI.new

features2 <- data.frame(
  log_population = log(knn_data$population),
  gdp = knn_data$gdp,
  epi = knn_data$EPI.new
)

train_features2 <- features2[train_indices, ]
test_features2 <- features2[-train_indices, ]

knn_pred2 <- knn(train = train_features2, test = test_features2, 
                 cl = train_labels, k = best_k)

confusion_matrix2 <- table(Predicted = knn_pred2, Actual = test_labels)
print(confusion_matrix2)
accuracy2 <- sum(knn_pred2 == test_labels) / length(test_labels)
cat("Accuracy:", accuracy2, "\n")
